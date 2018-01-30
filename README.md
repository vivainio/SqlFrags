# Fapper - a SQL generator for F#

## The problem:

Tools like Dapper make it easy to materialize objects, but you need to write SQL strings by hand.

SQL generation is painful and requires careful typing. It can be made easier by adding a static component.
Fapper is that static component.

F# gives you a nice list syntax, so you provide a list of "Frags" (a distriminated union of SQL fragments you want to have in your query), like so:

```fsharp
open Fapper.SqlGen

// declaring some table names for later reference...

let Emp = Table "employee"
let Org = Table "organization"

let upd = [
    Update Emp
    Set [
        "salary", "10"
        "name", "'heimo'"
        "address", "@addressparam"

    ]
    WhereS "foo > bar"
]

let rendered = upd |> serializeSql SqlSyntax.Any

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
Emitting completely illegal SQL is fine, Fapper is not one to second guess you - it diligently renders the garbage
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


## FAQ

### Why Fapper when there are millions of other SQL generators on the web?

There aren't for .NET. Search for yourself.

### Can I use this on C#?

Nope, too tied to F# data structures. I would love for someone to "steal the idea" and do a similar thing for C# (maybe with fluent builder pattern or whatever).

### What's up with the name?

It's a "piece of F# code you can run before feeding the query to Dapper", hence F#apper or Fapper.

I'm aware the same word is used as a vulgar noun in some youth oriented internet subcultures,
but that is so orthogonal to the topic of SQL Generation that I don't expect there to be confusion.

It's also mildly amusing, for the time being.

### Is it tied to Dapper somehow?

No. In fact I use it directly with conn.CreateCommand() and query.ExecuteReader() and the untyped readers, using SqlClient or ODP.net data providers.

### What databases does it support?

All of them, but depends. E.g. "Page" fragment won't work in old Oracle versions. If your SQL contains @ like query parameters the won't work
with oracle (and I didn't yet do a helper for that). You get the idea. The API has support to branch the rendering based Sql syntax, but currently
SqlSyntax.Any is used.

### What databases does it support?

I'm using it against Oracle and MSSQL.

## Installation

https://www.nuget.org/packages/Fapper

## License

MIT
