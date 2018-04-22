open Fapper.SqlGen
open TrivialTestRunner

let rendersToSyntax syntax expected (frags: Frag list)  =
    let rendered = frags |> serializeSql syntax
    let ok = rendered = expected
    if (not ok) then do
        printfn "Got\n%s\ninstead of\n%s\n" (rendered.Replace("\n", "\\n")) (expected.Replace("\n","\\n"))
    Assert.IsTrue(ok)

let rendersTo expected frags = rendersToSyntax SqlSyntax.Any expected frags


type TestType() =
    member val foo = "" with get,set
    member val bar = 0 with get,set
    member val mybool = false with get,set

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
        query |> rendersTo "select employee.id, employee.name, employee.salary, employee.team\nselect employee.Foo as testalias\nfrom employee\nwhere salary > 1000\nwhere foo > bar\ninner join organization OrgAlias on employee.OrgID=OrgAlias.ID\nwhere employee.Company=organization.Id\ngroup by team\norder by salary"

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
        [ Emp --> [ "Salary"; "Name" ] ]
        |> rendersTo "select Salary, Name\nfrom employee"

        // same with extension method
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


    [<Case>]
    static member TestNesting() =
        [ Many [Many [ Raw "Inner" ] ]
        ] |> rendersTo "Inner"

        [ Nest [ Raw "Inner" ]
        ] |> rendersTo "(\n    Inner\n)"

        let onetwolj = [ LineJoiner(LineJoiners.ParensAndCommas, [ Raw "One"; Raw "Two" ]) ]
        onetwolj |> rendersTo "(\nOne,\nTwo\n)"

        [ Nest onetwolj ] |> rendersTo "(\n(\n    One,\n    Two\n)\n)"

        [
            Raw "a"
            Indent [
                Raw "a.1"
                Indent [
                    Raw "a.1.1"
                    Raw "a.1.2"

                ]
                Raw "a.2"
            ]
            Raw "b"
        ] |> rendersTo "a\n    a.1\n        a.1.1\n        a.1.2\n    a.2\nb"

    [<Case>]
    static member TestCreates() =
        let Emp = Table "Employee"
        [
            Alter.CreateTable Emp
                [
                    "Name", VarChar 50
                    "Salary", Number(10,2)
                    "Address", Text |> NotNull
                ]
        ] |> rendersTo "create table Employee\n(\n    Name varchar(50),\n    Salary decimal(10,2),\n    Address text NOT NULL\n)"

    [<Case>]
    static member TestSyntax() =
        let r =
            [
                RawSyntax [
                    SqlSyntax.Any, "hello sql"
                    SqlSyntax.Ora, "hello oracle"
                ]
            ]
        let r1 = r |> serializeSql SqlSyntax.Any
        let r2 = r |> serializeSql SqlSyntax.Ora
        r1="hello sql" |> Assert.IsTrue
        r2="hello oracle" |> Assert.IsTrue

    [<Case>]
    static member PlSql() =
        let q =
            [
                Pl.Exec [
                    Raw "hello"
                    Raw "o'malley"
                ]

            ]

        q |> rendersToSyntax SqlSyntax.Any "execute sp_executesql\n'    hello,\n    o''malley'"
        q |> rendersToSyntax SqlSyntax.Ora "execute immediate\n'    hello,\n    o''malley'"

        // semicolon terminated statements
        [
            Pl.Stm [
                Raw "ab"
                Raw "cd"
            ]
        ] |> rendersTo "ab\ncd;"

        // if then
        [
            Pl.IfThen "a > 10" [
                [Raw "stuff in then"] |> Pl.Stm
                [Raw "other line"] |> Pl.Stm
            ]
        ] |> rendersTo "if a > 10\nthen\n    stuff in then;\n    other line;\nend if;"


        let vdef =
            [
                VarDef("foo", VarChar 10, "'hi'")
            ]
        vdef |> rendersToSyntax SqlSyntax.Any "@foo varchar(10) = 'hi';"
        vdef |> rendersToSyntax SqlSyntax.Ora "foo nvarchar2(10) := 'hi';"

    [<Case>]
    static member ModifyTables() =
        let Emp = Table "Employee"
        [
            Alter.AddCol Emp?Foo (VarChar 15)
            Alter.ModifyCol Emp?Foo (VarChar 11)
            Alter.DropCol Emp?Foo
        ] |> rendersTo "alter table Employee add Foo\nvarchar(15);\nalter table Employee modify column Foo\nvarchar(11);\nalter table Employee drop column Foo;"

        [
            Alter.CreateIndex Emp "foo_idx" ["foo"; "bar"]

        ] |> rendersTo "create index foo_idx on Employee (foo, bar);"

    [<Case>]
    static member ComposePlSql() =
        // demo of composing tedious pl/sql stuff with functions
        let Emp = Table "E"
        let createFrame category vertag frags =
            Many [
                Raw "declare"
                VarDef("new_version", VarChar 32, sprintf "replace('%s', '-')" vertag);
                Pl.Begin [
                    Pl.IfThen "not db_version_exists(new_version)" [
                        Many frags
                        Raw <| sprintf "insert into VERSIONS values (new_version, '%s', CURRENT_TIMESTAMP)" category
                    ]
                ]
            ]
        let upgradeSlug =
            createFrame "SOMEPRODUCT" "1212-2323" [
                Insert(Emp, [
                           "foo", "1"
                ])
        ]

        [upgradeSlug] |> rendersToSyntax SqlSyntax.Ora "declare\nnew_version nvarchar2(32) := replace('1212-2323', '-');\nbegin\n    if not db_version_exists(new_version)\n    then\n        insert into E (foo) values (1)\n        insert into VERSIONS values (new_version, 'SOMEPRODUCT', CURRENT_TIMESTAMP)\n    end if;\nend;"
        ()

    [<Case>]
    static member TypedTest() =
        let tt = TestType()

        [
            Set <| Typed.AsList
                    <@
                        tt.bar <- 12
                        tt.foo <- "hello"
                        tt.mybool <- false
                    @>
        ] |> rendersTo "set bar = 12, foo = 'hello', mybool = false"

        let emp = Table "employee"

        [
            Insert(emp,
                Typed.AsList
                    <@
                        tt.bar <- 12
                        tt.foo <- "huuhaa"
                    @>)

        ] |> rendersTo "insert into employee (bar,foo) values (12,'huuhaa')"

        let fortable =
            Typed.AsListForTable emp
                <@
                    tt.bar <- 9
                @>
        [("employee.bar", "9")] = fortable |> Assert.IsTrue

    [<Case>]
    static member TypedDrill() =
        let tt = TestType()

        // create a list of default values etc, then create many lists using those default values
        let l3 =
            <@
                tt.bar <- 1
                tt.foo <- "defaultval"
            @> |>
                Typed.Drill [
                    <@ tt.foo <- "a" @>
                    <@ tt.foo <- "b" @>
                    <@ () @>
                ]
        let expected = [
            [("bar", "1"); ("foo", "'a'")]; [("bar", "1"); ("foo", "'b'")];
            [("bar", "1"); ("foo", "'defaultval'")]]
        (List.ofSeq l3) = expected |> Assert.IsTrue

[<EntryPoint>]
let main argv =
    TRunner.CrashHard <- false
    TRunner.AddTests<Tests>()
    TRunner.RunTests()
    TRunner.ReportAll()

    0 // return an integer exit code
