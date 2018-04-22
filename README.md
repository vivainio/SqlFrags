# SqlFrags - a SQL generator for F#

## The problem:

Tools like Dapper make it easy to materialize objects, but you need to write SQL strings by hand.

SQL generation is painful and requires careful typing. It can be made easier by adding a static component.
SqlFrags is that static component.

F# gives you a nice list syntax, so you provide a list of "Frags" (a distriminated union of SQL fragments you want to have in your query), like so:

```fsharp
open SqlFrags.SqlGen

// declaring some table names for later reference...

let Emp = Table "employee"
let Org = Table "organization"

let upd = [
    Emp.Update
        [
            "salary", "10"
            "name", "'heimo'"
            "address", "@addressparam"

        ]
    WhereS "foo > bar"
]


let rendered = upd |> Frags.Emit SqlSyntax.Any
```

This renders to string:

```sql
update employee
set salary = 10, name = 'heimo', address = @addressparam
where foo > bar
```


That should make it a bit harder to screw up.

You can do nested subqueries:

```fsharp
let nested = [
    SelectS ["*"]
    Raw "from"
    NestAs("root",  [
                    SelectS ["*"]
                    From User
    ])
]
```

This gives you indented, parenthesized SQL (aliased as you specified):

```sql
select *
from
(
    select *
    from USER_DATA
) root
```


Simple updates are easy enough to do with existing micro-orms like Dapper.Contrib or PetaPoco.
However, you often need to produce complex queries, so you can crank up the difficulty with nesting, aliases etc.
Emitting completely illegal SQL is fine, SqlFrags is not one to second guess you - it diligently renders the garbage
you feed it:

```fsharp
let query = [
    Select <| Emp.Cols ["id";"name"; "salary"; "team"]
    SelectAs [Emp?Foo, "testalias"]
    From Emp
    WhereS "salary > 1000"
    Many [
        Skip
        WhereS "foo > bar"
        Skip
    ]
    JoinOn( Org.Col "ID", Emp.Col "OrgID", Table "OrgAlias", "")
    Where [Emp?Company == Org?Id]
    GroupBy ["team"]
    OrderBy ["salary"]
]
```

Did you you see that JoinOn? It does:

``` sql
inner join organization OrgAlias on employee.OrgID=OrgAlias.ID
```

If you wanted "outer", just pass "outer" as the last argument to JoinOn (empty string defaults to "inner join").

And what are those "Many" and Skip parts? They are provided for convenience, when splicing sublists in programmatically
generated queries.

There are some overloaded operators and extension methods to simplify selectand where clauses:

```fsharp
        // select stuff with --> and --->
[ Emp --> [ "Salary"; "Name" ] ]
|> rendersTo "select Salary, Name\nfrom employee"

// same with extension method - please use extension methods instead of operators!
[ Emp.Select [ "Salary"; "Name" ] ]
|> rendersTo "select Salary, Name\nfrom employee"

[ Emp ---> [ Emp?Salary; Emp?Name ] ]
|> rendersTo "select employee.Salary, employee.Name\nfrom employee"
// ===^ (where condition without quoting)
[
    Emp --> ["*"]
    Where [Emp?ID ===^ "@ID"]
] |> rendersTo "select *\nfrom employee\nwhere employee.ID=@ID"

// === (where condition with quoting)
[
    Emp --> ["*"]
    Where [Emp?ID === "jorma"]
] |> rendersTo "select *\nfrom employee\nwhere employee.ID='jorma'"

[ Emp.Delete ]
|> rendersTo "delete from employee"

[
    Where [Emp?ID.Equals "@bar"]
    Where [Emp?ID.EqualsQ "bar"]
    Where [Emp?ID.EqualsCol Org?ID]
    Where [Emp?ID.In "[1,2]"]
    Where [Emp?ID.Op "<" "12"]

] |> rendersTo "where employee.ID=@bar\nwhere employee.ID='bar'\nwhere employee.ID=organization.ID\nwhere employee.ID in [1,2]\nwhere employee.ID < 12"

```


## FAQ

### Why SqlFrags when there are millions of other SQL generators on the web?

There aren't for .NET. Search for yourself.

### Can I use this on C#?

Nope, too tied to F# data structures. Similar "mechanical SQL emission" philosophy for C# is implemented e.g. in https://github.com/sqlkata/querybuilder

### What's up with the name?

Yes, this project was earlier known as "Fapper", which turned out to be rather problematic. Enjoy the new name.


### What databases does it support?

All of them, but depends. E.g. "Page" fragment won't work in old Oracle versions. If your SQL contains @ like query parameters they won't work
with oracle (and I didn't yet do a helper for that). You get the idea. The API has support to branch the rendering based on Sql syntax,
but currently only SqlSyntax.Any is used.

### Why not use XXX or YYY instead?

SqlFrags allows you to compose queries from fragments. You can create the fragments (or lists of fragments) in functions, assign
repeated fragments to variables, etc. This is like creating HTML with Suave.Html, Giraffe ViewModel or Elm.

You don't need to have access to database schema (yet alone live database, like with SqlProvider) to create queries. This helps if
you are building software against arbitrary databases (think tools like Django Admin), or where schema is configurable.

You don't need to learn to "trick" the ORM to emit the SQL you want. What you write is what you get.

The codebase is trivial mapping of DU cases to emitted strings:

```fsharp
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
...
```

So, if you want to add something you need, you just do it. Copy the SqlGen.fs to your project, or make a PR and join the SqlFrags family.


## Installation

https://www.nuget.org/packages/SqlFrags

## License

MIT
