module Fapper.SqlGen

type SqlSyntax =
| Any
| Ora


type Table = Table of string
    with
        member x.Col(s) = ColRef(x, s)
        member x.Name =
            let (Table n) = x
            n
        member x.Cols(names: #seq<string>) = names |> Seq.map (fun n -> x.Col(n))

and ColRef = ColRef of Table*string
    with
        member x.Str =
            let (ColRef((Table t), c)) = x
            sprintf "%s.%s" t c
        member x.Table =
            let (ColRef((Table t), _)) = x
            t
        member x.Right = match x with
                         | ColRef(_, r) -> r
        static member PrefixTable prefix (ColRef(Table(t), col)) = ColRef((Table (prefix + t)),col)


let inline (?) (this: Table) (colName:string) =
    this.Col colName

let sqlQuoted (s: string) = s.Replace("'", "''") |> sprintf "'%s'"

type Cond =
    | ConstEq of ColRef*string
    | ColsEq of ColRef*ColRef
    | ConstBinOp of string*ColRef*string

with
    member x.Str = match x with
                   | ConstEq(cr,value) -> sprintf "%s=%s" cr.Str value
                   | ColsEq(l,r) -> sprintf "%s=%s" l.Str r.Str
                   | ConstBinOp(op, l,r) -> sprintf "%s %s %s" l.Str op r



type DDLType =
    | Int
    | SmallInt
    | BigInt
    | TinyInt
    | Float
    | Real
    | Date
    | DateTime
    | DateTime2
    | VarChar of int
    | Number of int*int
    | Text
    | NotNull of DDLType
    | PrimaryKey of DDLType

type DDLCol = (string*DDLType)

module DDLCol =
    let rec typeToString (syntax: SqlSyntax) (t: DDLType) =
        match t with
        | Int -> "int"
        | SmallInt -> "smallint"
        | BigInt -> "bigint"
        | TinyInt -> "tinyint"
        | Float -> "float"
        | Real -> "real"
        | Date -> "date"
        | DateTime -> "datetime"
        | DateTime2 -> "datetime2"
        | VarChar c ->
            match syntax with
            | Ora -> sprintf "nvarchar2(%d)" c
            | Any -> sprintf "varchar(%d)" c
        | Number(a,b) ->
            match syntax with
            | Ora -> sprintf "number(%d,%d)" a b
            | Any -> sprintf "decimal(%d,%d)" a b
        | Text -> "text"
        | NotNull t -> sprintf "%s NOT NULL" (typeToString syntax t)
        | PrimaryKey t -> sprintf "%s PRIMARY KEY" (typeToString syntax t)


// inline operators

// compare column against column
let (==) (l: ColRef) (r: ColRef) = ColsEq(l,r)
// compare column against const, user needs to add quotes if needed
let (===^) (l: ColRef) (r: string) = ConstEq(l,r)
// compare column against const, add quotes around constant value
let (===) (l: ColRef) (r: string) = l ===^ (sqlQuoted r)

let (<=>) (l: ColRef) (r: string) = ConstBinOp("<>", l,r)
let IsAny (l: ColRef) (r: string) = ConstBinOp("in", l, r )

type LineJoinerFunc = string seq -> string

module LineJoiners =
    let ParensAndCommas (lines: string seq) =
        sprintf "(\n%s\n)" (String.concat ",\n" lines)
    let SingleQuotes (lines: string seq) =
        sprintf "'%s'" ((String.concat ",\n" lines).Replace("'", "''"))
    let Terminated (lines: string seq) =
        (String.concat "\n" lines) + ";"


type Frag =
    | SelectS of string seq
    | Select of ColRef seq
    | SelectAs of (ColRef*string) seq
    | FromS of string list
    | From of Table
    | FromAs of Table*Table // real table, alias name
    | Indent of Frag seq // indent subtree
    | Nest of Frag seq // indent + paren wrapping
    | NestAs of string*(Frag seq) // (provide alias for nested seg, e.g. (select ID from Emp) MyIds
    | Raw of string
    | RawSyntax of ((SqlSyntax*string) seq) // select string to emit by syntax
    | WhereS of string
    | Where of Cond seq
    | OrderBy of string list
    | GroupBy of string list
    | Skip  // skip does not emit a line
    | Join of Cond
    | JoinOn of ColRef*ColRef*Table*string // other, this, correlation name, join type ("LEFT OUTER", "INNER" etc)
    | Many of Frag seq  // many does emit a line, but emits its children instead
    | LineJoiner of (LineJoinerFunc*(Frag list))
    | Update of Table
    | Insert of Table*((string*string) seq)
    | Set of (string*string) list
    | Page of (int*int)
    // DDL
    | ColDef of DDLCol
    | TypeName of DDLType
    // 4gl specialities
    | VarDef of (string*DDLType*string) // @name type = value;

// functions that look like frags, but generate other frags instead
let AliasAs (aliasTo: Table) frag =
    match frag with
    | From t -> FromAs(t, aliasTo)
    | Join cond ->
        match cond with
        | ColsEq(other,this) ->
            JoinOn(other, this, aliasTo, "")
        | _ -> failwithf "No alias for %A" cond
    | _ -> failwithf "Aliasing not implemented %A" frag

let Exists (frags: Frag seq) =
    Nest [
        Raw "exists"
        Nest frags
    ]

let CreateTable (Table t) (cols: DDLCol list) =
    Many [
            Raw <| sprintf "create table %s" t
            Indent [
                LineJoiner(LineJoiners.ParensAndCommas, (cols |> List.map ColDef))
            ]
    ]

// pl/sql, tsql and "nice" stuff like that

module Pl =
    let Stm frags = LineJoiner(LineJoiners.Terminated, frags)
    let Exec frags =
        Many [
            RawSyntax [
                SqlSyntax.Any, "execute sp_executesql"
                SqlSyntax.Ora, "execute immediate"
            ]
            Indent [
                LineJoiner(LineJoiners.SingleQuotes, frags)
           ]
        ]
    let IfThen (cond: string) frags =
        Many [
            Raw <| "if " + cond
            Raw "then"
            Indent frags
            Raw "end if;"
        ]
    let Begin frags =
        Many [
            Raw "begin"
            Indent frags
            Raw "end;"
        ]


// shorthands for SELECTing stuff
let (-->) (l: Table) (r: string list) = Many [SelectS r; From l]
let (--->) (l: Table) (rs: ColRef seq) = Many [Select rs; From l]

let colonList strings = String.concat ", " strings

let rec serializeFrag (syntax: SqlSyntax) frag =
    match frag with
    | SelectS els -> "select " + colonList els
    | Select cols ->
        "select " +
            (cols |> Seq.map (fun c -> c.Str) |> colonList)
    | SelectAs cols ->
        "select " +
            (cols |> Seq.map (fun (c,alias) -> sprintf "%s as %s" c.Str alias) |> colonList)
    | FromS els -> "from " + colonList els
    | From (Table t) -> "from " + t
    | FromAs (Table t, Table alias) -> sprintf "from %s as %s" t alias

    | WhereS s -> "where " + s
    | Where conds ->
        let joined = conds |> Seq.map (fun c -> c.Str) |> String.concat " and "
        "where " + joined
    | OrderBy els -> "order by " + colonList els
    | GroupBy els -> "group by " + colonList els
    | Join cond ->
        match cond with
        | ColsEq(other,this) -> sprintf "join %s on %s" other.Table cond.Str
        | _ -> failwith "Join cond not supported"

    | JoinOn(other: ColRef, this: ColRef, (Table alias), joinKind: string) ->
        let (ColRef(Table otherTable, _)) = other
        let aliasedOther = if alias = "" then other else match other with
                                                         | ColRef (_, col) -> ColRef(Table alias, col)
        let joinType = if joinKind = "" then "inner" else joinKind
        sprintf "%s join %s %s on %s=%s" joinType otherTable alias this.Str aliasedOther.Str
    | Skip -> ""
    | Update (Table t) -> "update " + t
    | Set (updlist) ->
        let updates = updlist |> List.map (fun (k,v) -> sprintf "%s = %s" k v) |> String.concat ", "
        "set " + updates
    | Raw txt -> txt
    | Page(offset,limit) -> sprintf "offset %d rows fetch next %d rows only" offset limit
    | Insert(Table t, values) ->
        let collist = values |> Seq.map fst |> String.concat ","
        let vallist = values |> Seq.map snd |> String.concat ","
        sprintf "insert into %s (%s) values (%s)" t collist vallist
    | RawSyntax(rules) ->
        rules |> Seq.find (fun r -> fst r = syntax) |> snd

    | ColDef(name, typ) -> sprintf "%s %s" name (DDLCol.typeToString syntax typ)
    | TypeName typ -> DDLCol.typeToString syntax typ
    | VarDef(name, typ, value) ->
        match syntax with
        | SqlSyntax.Ora -> sprintf "%s %s := %s;" name (DDLCol.typeToString syntax typ) value
        | SqlSyntax.Any -> sprintf "@%s %s = %s;" name (DDLCol.typeToString syntax typ) value

    | Nest _ | NestAs _ | Many _  | LineJoiner _ | Indent _ -> failwith "Should never see subquery at serialization"


let serializeSql syntax frags =
    let nSpaces n = (String.replicate (n*4) " ")
    let emitFrag nestingLevel frag  =
        (nSpaces nestingLevel) + serializeFrag syntax frag


    let rec emitFrags nestingLevel frags =

        let nestInParens level txt =
            sprintf "%s(\n%s\n%s)" (nSpaces level) txt (nSpaces level)

        let doIndent subfrags = (emitFrags (nestingLevel+1) subfrags |> String.concat "\n")

        let doNest subfrags = (doIndent subfrags |> nestInParens nestingLevel)

        seq {
            for frag in frags do
                match frag with
                | Indent subfrags ->
                    yield (doIndent subfrags)

                | Many frags ->
                    let emittedChildren = emitFrags nestingLevel frags
                    yield! emittedChildren
                | Nest subfrags -> yield (doNest subfrags)
                | NestAs(alias, subfrags) -> yield (sprintf "%s %s" (doNest subfrags) alias)
                | LineJoiner(func, frags) ->
                    let emittedChildren = emitFrags nestingLevel frags
                    yield func(emittedChildren)
                | Skip -> ()
                | _ -> yield (emitFrag nestingLevel frag)
        }
    frags |> emitFrags 0 |>  String.concat "\n"

module Alter =
    let Operation (operation: string) (col: ColRef) (typ: DDLType) =
        let (ColRef(t, c)) = col
        Pl.Stm [
            Raw <| sprintf "alter table %s %s %s" t.Name operation c
            TypeName typ
        ]

    let addCol (col: ColRef) (typ: DDLType) =
        Operation "add" col typ

    let modifyCol (col: ColRef) (typ: DDLType) =
        Operation "modify column" col typ
    let dropCol (col: ColRef) =
        let (ColRef(t, c)) = col
        Pl.Stm [ Raw <| sprintf "alter table %s drop column %s" t.Name c ]

    let createIndex (tab: Table)  (idxName: string) (cols: string seq) =
        Raw <| sprintf "create index %s on %s (%s);" idxName tab.Name (colonList cols)
