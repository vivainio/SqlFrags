// Learn more about F# at http://fsharp.org

open System
open Fapper.SqlGen
open TrivialTestRunner

let rendersTo expected (frags: Frag list)  = 
    let rendered = frags |> serializeSql SqlSyntax.Any
    let ok = rendered = expected
    if (not ok) then do 
        printfn "Got\n%s\ninstead" (rendered.Replace("\n", "\\n"))
    Assert.IsTrue(ok)

type Tests() =
    [<Case>]
    static member TestOrm() =
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
        upd |> rendersTo "update employee\nset salary = 10, Name = 'heimo'\nwhere foo > bar"

        //upd |> (serializeSql SqlSyntax.Any) |> printVerbatim

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
        query |> rendersTo "select employee.id, employee.name, employee.salary, employee.team\nselect employee.Foo as testalias\nfrom employee\nwhere salary > 1000\n\nwhere foo > bar\n\ninner join organization EMPLOYEE on employee.OrgID=EMPLOYEE.ID\nwhere employee.Company=organization.Id\ngroup by team\norder by salary"

        let nested = [
            SelectS ["*"]
            Raw "from"
            NestAs("root",  [
                            SelectS ["*"]
                            From User
            ])
        ]
        nested |> rendersTo "select *\nfrom\n(\n    select *\n    from USER_DATA\n) root"
        let out2 = serializeSql SqlSyntax.Any nested

        let values = [ "a","1" ; "b","2"]
        let writes = [
                        Insert(Emp, values)
                     ] 
        writes |> rendersTo "insert into employee (a,b) values (1,2)"
    
        ()

[<EntryPoint>]
let main argv =
    TRunner.AddTests<Tests>()
    TRunner.RunTests()
    TRunner.ReportAll()
    
    0 // return an integer exit code
