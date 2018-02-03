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
                "name", "'heimo'"
                "address", "@addressparam"

            ]
            WhereS "foo > bar"
        ]
        upd |> rendersTo "update employee\nset salary = 10, name = 'heimo', address = @addressparam\nwhere foo > bar"

        //upd |> (serializeSql SqlSyntax.Any) |> printVerbatim

        // this is not legal sql. Fapper does absolutely no structure validity checking whatsoever
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
        query |> rendersTo "select employee.id, employee.name, employee.salary, employee.team\nselect employee.Foo as testalias\nfrom employee\nwhere salary > 1000\n\nwhere foo > bar\n\ninner join organization OrgAlias on employee.OrgID=OrgAlias.ID\nwhere employee.Company=organization.Id\ngroup by team\norder by salary"

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


        let countq = [
            SelectS ["count(1)"]
            Raw "from"
            NestAs("Users",[
                            SelectS ["*"]
                            From User
            ])
        
        ]
        countq |> rendersTo "select count(1)\nfrom\n(\n    select *\n    from USER_DATA\n) Users"

        let inq = [
            SelectS ["*"]
            From (Table "TASK")
            WhereS "recipient_id in "
            Nest [
                Select [User.Col("ID")] 
                From User
            ]
        ]

        inq |> rendersTo "select *\nfrom TASK\nwhere recipient_id in \n(\n    select USER_DATA.ID\n    from USER_DATA\n)"
    
        // select stuff with --> and --->
        let easySelectRaw = [ Emp --> [ "Salary"; "Name" ] ];
        easySelectRaw |> rendersTo "select Salary, Name\nfrom employee"

        let easySelect = [ Emp ---> [ Emp?Salary; Emp?Name ] ] 
        easySelect |> rendersTo "select employee.Salary, employee.Name\nfrom employee"
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


[<EntryPoint>]
let main argv =
    TRunner.AddTests<Tests>()
    TRunner.RunTests()
    TRunner.ReportAll()
    
    0 // return an integer exit code
