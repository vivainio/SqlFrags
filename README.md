# Fapper - a SQL generator for F#

## The problem:

Tools like Dapper make it easy to materialize objects, but you need to write SQL strings by hand.

SQL generation is painful and requires careful typing. It can be made easier by adding a static component.
Fapper is that static component.

F# gives you a nice list syntax, so you provide a list of "Frags" (a distriminated union of SQL fragments you want to have in your query), like so:

```fsharp

open Fapper.SqlGen

let Emp = Table "employee"
let upd = [
    Update Emp
    Set [
        "salary", "10"
        "Name", "'heimo'"
    ]
    WhereS "foo > bar"
]
```

This renders to:

```sql
update employee\nset salary = 10, Name = 'heimo'\nwhere foo > bar
```


That should make it a bit harder to screw up.

## Installation

https://www.nuget.org/packages/Fapper

## License

MIT
