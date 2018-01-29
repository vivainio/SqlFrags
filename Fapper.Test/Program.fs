// Learn more about F# at http://fsharp.org

open System
open Fapper

let testOrm() =
    let Emp = Table "employee"
    let Org = Table "organization"
    let User = Table "USER_DATA"
    let upd = [
        Update Emp
        Set [
            "salary", "10"
            "Name", "'heimo'"
        ]
        WhereS "foo > bar"
    ]

    upd |> (serializeSql SqlSyntax.Any) |> printfn "%s"

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
        JoinOn( Org.Col "ID", Emp.Col "OrgID", Table "EMPLOYEE", "")
        Where [Emp?Company == Org?Id]
        GroupBy ["team"]
        OrderBy ["salary"]
    ]
    query |> serializeSql SqlSyntax.Any |> printfn "%s"

    let nested = [
        SelectS ["*"]
        Raw "from"
        NestAs("root",  [
                        SelectS ["*"]
                        From User
        ])
    ]
    let out2 = serializeSql SqlSyntax.Any nested

    let values = [ "a","1" ; "b","2"]
    let writes = [
                    Insert(Emp, values)
                 ] 

    
    writes |> serializeSql SqlSyntax.Any |> printfn "%s"
    ()
[<EntryPoint>]
let main argv =
    testOrm()
    0 // return an integer exit code
