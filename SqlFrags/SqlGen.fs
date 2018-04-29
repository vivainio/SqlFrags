module SqlFrags.SqlGen

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


type LineJoinerFunc = string seq -> string

module LineJoiners =
    let ParensAndCommas (lines: string seq) =
        sprintf "(\n%s\n)" (String.concat ",\n" lines)
    let SingleQuotes (lines: string seq) =
        sprintf "'%s'" ((String.concat ",\n" lines).Replace("'", "''"))
    let Terminated (lines: string seq) =
        (String.concat "\n" lines) + ";"



type Frag =
    | SelectAs of (ColRef*string) seq // every col getsn an alias
    | FromS of string list
    | From of Table
    | FromAs of Table*Table // real table, alias name
    | Indent of Frag seq // indent subtree
    | Nest of Frag seq // indent + paren wrapping
    | NestAs of string*(Frag seq) // (provide alias for nested seg, e.g. (select ID from Emp) MyIds
    | Raw of string
    | RawSyntax of ((SqlSyntax*string) seq) // select string to emit by syntax
    | Skip  // skip does not emit a line
    | JoinOn of ColRef*ColRef*Table*string // other, this, correlation name, join type ("LEFT OUTER", "INNER" etc)
    | Many of Frag seq  // many does emit a line, but emits its children instead
    | LineJoiner of (LineJoinerFunc*(Frag list))

    // DDL
    | ColDef of DDLCol
    | TypeName of DDLType
    // 4gl specialities
    | VarDef of (string*DDLType*string) // @name type = value;

and Cond =
    | ConstEq of ColRef*string
    | ColsEq of ColRef*ColRef
    | ConstBinOp of string*ColRef*string
    | CondCompose of string*(Cond seq)

    with
        member x.Str =
            match x with
            | ConstEq(cr,value) -> sprintf "%s=%s" cr.Str value
            | ColsEq(l,r) -> sprintf "%s=%s" l.Str r.Str
            | ConstBinOp(op, l,r) -> sprintf "%s %s %s" l.Str op r
            | CondCompose(op, parts) -> parts |> Seq.map (fun p -> p.Str) |> String.concat (sprintf " %s " op) |> sprintf "(%s)"
        member x.AsFrag =
            match x with
            | CondCompose(op, parts) ->
                parts |> Seq.map (fun p -> p.AsFrag) |> TextUtil.Interleave (Raw op) |> Nest
            | _ -> Raw x.Str

// functions that look like frags, but generate other frags instead

// WHERE that does no nesting. Deprecate?

let colonList strings = String.concat ", " strings

// Columns that takes a list of strings
let ColumnsS (cols: string seq) = cols |> TextUtil.Colonize |> TextUtil.TextWrap "  " 75 |> Seq.map Raw |> Many

// Columns that takes a list of colrefs
let Columns (cols: ColRef seq) = cols |> Seq.map (fun c -> c.Str) |> ColumnsS




let Where (conds: Cond seq) =
    let joined = conds |> Seq.map (fun c -> c.Str) |> String.concat " and "
    Raw <| "where " + joined

let WhereS s = sprintf "where %s" s |> Raw

let Exists (frags: Frag seq) =
    Nest [
        Raw "exists"
        Nest frags
    ]

let Page offset limit =
    Raw <| sprintf "offset %d rows fetch next %d rows only" offset limit

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
let rec serializeFrag (syntax: SqlSyntax) frag =
    match frag with
    | SelectAs cols ->
        "select " +
            (cols |> Seq.map (fun (c,alias) -> sprintf "%s as %s" c.Str alias) |> colonList)
    | FromS els -> "from " + colonList els
    | From (Table t) -> "from " + t
    | FromAs (Table t, Table alias) -> sprintf "from %s as %s" t alias
    //| OrderBy els -> "order by " + colonList els
    //| GroupBy els -> "group by " + colonList els
    | JoinOn(other: ColRef, this: ColRef, (Table alias), joinKind: string) ->
        let (ColRef(Table otherTable, _)) = other
        let aliasedOther = if alias = "" then other else match other with
                                                         | ColRef (_, col) -> ColRef(Table alias, col)
        let joinType = if joinKind = "" then "inner" else joinKind
        sprintf "%s join %s %s on %s=%s" joinType otherTable alias this.Str aliasedOther.Str
    | Raw txt -> txt
    | RawSyntax(rules) ->
        rules |> Seq.find (fun r -> fst r = syntax) |> snd

    | ColDef(name, typ) -> sprintf "%s %s" name (DDLCol.typeToString syntax typ)
    | TypeName typ -> DDLCol.typeToString syntax typ
    | VarDef(name, typ, value) ->
        match syntax with
        | SqlSyntax.Ora -> sprintf "%s %s := %s;" name (DDLCol.typeToString syntax typ) value
        | SqlSyntax.Any -> sprintf "@%s %s = %s;" name (DDLCol.typeToString syntax typ) value

    | Skip _ | Nest _ | NestAs _ | Many _  | LineJoiner _ | Indent _ -> failwith "Should never see subquery at serialization"


// the main function for everything
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

    let AddCol (col: ColRef) (typ: DDLType) =
        Operation "add" col typ

    let ModifyCol (col: ColRef) (typ: DDLType) =
        Operation "modify column" col typ
    let DropCol (col: ColRef) =
        let (ColRef(t, c)) = col
        Pl.Stm [ Raw <| sprintf "alter table %s drop column %s" t.Name c ]

    let CreateIndex (tab: Table)  (idxName: string) (cols: string seq) =
        Raw <| sprintf "create index %s on %s (%s);" idxName tab.Name (colonList cols)

    let CreateTable (Table t) (cols: DDLCol list) =
        Many [
                Raw <| sprintf "create table %s" t
                Indent [
                    LineJoiner(LineJoiners.ParensAndCommas, (cols |> List.map ColDef))
                ]
        ]

module Typed =
    open FSharp.Quotations.Patterns
    open FSharp.Quotations

    // yield all sequence parts from expr tree
    let rec private unseq (tree: Expr) =
        seq {
            match tree with
            | Sequential( h,t) ->
                yield h
                yield! unseq t
            | t ->
                yield t
        }

    let private extractPropSets parts =
        seq {
            for p in parts do
                match p with
                | (PropertySet (Some(k), pi, _, valHolder)) ->
                    match valHolder with
                    | ValueWithName(value, _, _) ->
                        yield (pi.Name, value)
                        //printfn "value %s is %A" pi.Name value
                    | Value(value, _) ->
                        yield (pi.Name, value)
                    | _ ->
                        failwithf "Unsupported val pattern %A" valHolder
                | _ -> ()
        }
    let private objAsString (o: obj) =
        match o with
        | :? int as i -> string i
        | :? bool as b -> if b then "true" else "false"
        | :? string as s -> sqlQuoted s
        | _ -> string o

    let private extractProps (tree: Expr) =
        let parts = unseq tree
        extractPropSets parts

    let AsList (tree: Expr) =
        extractProps tree |> Seq.map (fun (a,b) -> a, objAsString b ) |> List.ofSeq

    let AsListForTable (Table t) (tree: Expr) =
        let l = AsList tree
        l |> List.map (fun (k,v) -> (t + "." + k, v))

    // combine typed.AsList's to set defaults etc in parent
    let Drill (children: Expr seq) (parent: Expr) =
        let lastKeyWins (l: (string*string) list) =
            l |> List.rev |> List.distinctBy fst |> List.rev

        let parentL = AsList parent
        // for "child wins" logic
        Seq.map (AsList >> ((fun cl -> parentL @ cl) >> lastKeyWins)) children


// extension methods

type Table with
    member x.Delete = Raw (sprintf "delete from %s" x.Name)
    member x.Select cols =  Many [
                                    Raw "select"; ColumnsS cols; From x]
    member x.SelectC cols =  Many [ Raw "select"; Columns cols; From x]

    member x.SelectAll = Raw (sprintf "select * from %s" x.Name)
    member x.Update (updlist: (string*string) seq)=
        let updates = updlist |> Seq.map (fun (k,v) -> sprintf "%s = %s" k v) |> String.concat ", "
        Many [
            Raw (sprintf "update %s" x.Name)
            Raw (sprintf "set %s" updates)
        ]
    member t.Insert values =
        let collist = values |> Seq.map fst |> String.concat ","
        let vallist = values |> Seq.map snd |> String.concat ","
        sprintf "insert into %s (%s) values (%s)" t.Name collist vallist |> Raw

type ColRef with
    member l.Equals r = ConstEq(l,r)
    member l.EqualsQ r = ConstEq(l,sqlQuoted r)
    member l.EqualsCol r = ColsEq(l, r)

    member l.In r = ConstBinOp("in", l, r)
    member l.Op op r = ConstBinOp(op,l,r)

// operators

module Conds =
    let Like l r = ConstBinOp("like", l,sqlQuoted r)
    let In (l: ColRef) (r: string) = ConstBinOp("in", l, r )
    let And conds = CondCompose("and", conds)
    let Or conds = CondCompose("or",conds)

    // the better WHERE that doesn't assume "and"
    let Where (cond: Cond) =
        Many [
            Raw "where"
            Indent [cond.AsFrag]
        ]



// compare column against column
let (==) (l: ColRef) (r: ColRef) = ColsEq(l,r)
// compare column against const, user needs to add quotes if needed
let (===^) (l: ColRef) (r: string) = ConstEq(l,r)
// compare column against const, add quotes around constant value
let (===) (l: ColRef) (r: string) = l ===^ (sqlQuoted r)

let (<=>) (l: ColRef) (r: string) = ConstBinOp("<>", l,r)
let IsAny (l: ColRef) (r: string) = ConstBinOp("in", l, r )

module Funcs =
    let private unary name = sprintf "%s(%s)" name
    let Count  = unary "count"
    let Avg = unary "avg"
    let Sum = unary "sum"
    let As s alias = sprintf "%s as %s" alias s
    let Convert typ s = sprintf "convert(%s,%s)" typ s
    let IsNull replacement s = sprintf "isnull(%s,%s)" s replacement
    let Replace needle replacement s = sprintf "replace(%s, %s, %s)" s needle replacement
    let ReplaceQuoted needle replacement s = sprintf "replace(%s, %s, %s)" s (sqlQuoted needle) (sqlQuoted replacement)


let private opWithStringColumns op cols =
    Many [
        Raw op
        ColumnsS cols
    ]

let OrderBy cols = opWithStringColumns "order by" cols
let GroupBy cols = opWithStringColumns "group by" cols

// glorious public api
module Frags =
    let Emit syntax frags =
        serializeSql syntax frags


