module SqlFrags.Test


open SqlFrags.SqlGen
open TrivialTestRunner

let rendersToSyntax syntax expected (frags: Frag list)  =
    let rendered = frags |> Frags.Emit syntax
    let ok = rendered = expected
    if (not ok) then do
        printfn "Got\n%s\ninstead of\n%s\n" (rendered.Replace("\n", "\\n")) (expected.Replace("\n","\\n"))
        printfn "Indented:\n---\n%s\n---" rendered
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
            Emp.Update
                [
                    "salary", "10"
                    "name", "'heimo'"
                    "address", "@addressparam"

                ]
            WhereS "foo > bar"
        ]
        upd |> rendersTo "update employee\nset salary = 10, name = 'heimo', address = @addressparam\nwhere foo > bar"


        // this is not legal sql. SqlFrags does absolutely no structure validity checking whatsoever
        let query = [
            Emp.SelectC <| Emp.Cols ["id";"name"; "salary"; "team"]
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
            GroupBy ["team"; "foo"]
            OrderBy ["salary asc"; "hello desc"]
        ]
        query |> rendersTo "select\n  employee.id, employee.name, employee.salary, employee.team\nfrom employee\nselect employee.Foo as testalias\nfrom employee\nwhere salary > 1000\nwhere foo > bar\ninner join organization OrgAlias on employee.OrgID=OrgAlias.ID\nwhere employee.Company=organization.Id\ngroup by\n  team, foo\norder by\n  salary asc, hello desc"

        let nested = [
            Raw "select * from"
            NestAs("root",
                [
                    User.SelectAll
                ])
        ]
        nested |> rendersTo "select * from\n(\n    select * from USER_DATA\n) root"

        let values = [ "a","1" ; "b","2"]
        let writes = [ Emp.Insert values ]

        writes |> rendersTo "insert into employee (a,b) values (1,2)"

        let countq = [
            Raw "select count(1) from"
            NestAs("Users",[ User.SelectAll ])

        ]
        countq |> rendersTo "select count(1) from\n(\n    select * from USER_DATA\n) Users"

        let inq = [
            (Table "TASK").SelectAll
            WhereS "recipient_id in "
            Nest [
                User.SelectC [User?ID]
            ]
        ]

        inq |> rendersTo "select * from TASK\nwhere recipient_id in \n(\n    select\n      USER_DATA.ID\n    from USER_DATA\n)"



        // same with extension method - please use extension methods instead of operators!
        [ Emp.Select [ "Salary"; "Name" ] ]
        |> rendersTo "select\n  Salary, Name\nfrom employee"

        [ Emp.SelectAll ]
        |> rendersTo "select * from employee"

        [
            Where [Emp?ID ===^ "@ID"]
        ] |> rendersTo "where employee.ID=@ID"

        // === (where condition with quoting)
        [
            Where [Emp?ID === "jorma"]
        ] |> rendersTo "where employee.ID='jorma'"

        [ Emp.Delete ]
        |> rendersTo "delete from employee"

        [
            Where [Emp?ID.Equals "@bar"]
            Where [Emp?ID.EqualsQ "bar"]
            Where [Emp?ID.EqualsCol Org?ID]
            Where [Emp?ID.In "[1,2]"]
            Where [Emp?ID.Op "<" "12"]

        ] |> rendersTo "where employee.ID=@bar\nwhere employee.ID='bar'\nwhere employee.ID=organization.ID\nwhere employee.ID in [1,2]\nwhere employee.ID < 12"

        [
            Raw "select distinct"
            Columns [Emp?ID; Emp?Name]
        ] |> rendersTo "select distinct\n  employee.ID, employee.Name"


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
        let r1 = r |> Frags.Emit SqlSyntax.Any
        let r2 = r |> Frags.Emit SqlSyntax.Ora
        r1="hello sql" |> Assert.IsTrue
        r2="hello oracle" |> Assert.IsTrue


    [<Case>]
    static member TestConds() =
        let e = Table "emp"
        let conds =
            Conds.And [
                e?a.Equals "12"
                e?b.Equals "13"
                Conds.Or [
                    e?c.Equals "14"
                    e?d.Equals "15"
                ]
                Conds.And [
                    e?e.Equals "16"
                    e?f.Equals "17"
                ]
            ]
        [ conds.AsFrag ]
        |> rendersTo "(\n    emp.a=12\n    and\n    emp.b=13\n    and\n    (\n        emp.c=14\n        or\n        emp.d=15\n    )\n    and\n    (\n        emp.e=16\n        and\n        emp.f=17\n    )\n)"

        [ Where [conds]]
        |> rendersTo "where (emp.a=12 and emp.b=13 and (emp.c=14 or emp.d=15) and (emp.e=16 and emp.f=17))"

        [ Conds.Where <| Conds.And [e?a.Equals "10"; e?b.Equals "11" ] ]
        |> rendersTo "where\n    (\n        emp.a=10\n        and\n        emp.b=11\n    )"

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
                Emp.Insert ["foo", "1"]
        ]

        [upgradeSlug] |> rendersToSyntax SqlSyntax.Ora "declare\nnew_version nvarchar2(32) := replace('1212-2323', '-');\nbegin\n    if not db_version_exists(new_version)\n    then\n        insert into E (foo) values (1)\n        insert into VERSIONS values (new_version, 'SOMEPRODUCT', CURRENT_TIMESTAMP)\n    end if;\nend;"
        ()

    [<Case>]
    static member TypedTest() =
        let tt = TestType()
        let emp = Table "employee"

        [ emp.Update <|
            Typed.AsList
                <@
                    tt.bar <- 12
                    tt.foo <- "hello"
                    tt.mybool <- false
                @>
        ] |>

        rendersTo "update employee\nset bar = 12, foo = 'hello', mybool = false"


        [
            emp.Insert <|
                Typed.AsList
                    <@
                        tt.bar <- 12
                        tt.foo <- "huuhaa"
                    @>

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
    [<Case>]
    static member TestFuncs() =
        let s =
            "Foo"
            |> Funcs.IsNull "11" |> Funcs.Avg |> Funcs.Convert "tinyint"
            |> Funcs.Sum |> Funcs.Count |> Funcs.Replace "'a'" "'b'"
            |> Funcs.ReplaceQuoted "a" "b" |> Funcs.As "hippie"
        Assert.AreEqual(s, "replace(replace(count(sum(convert(tinyint,avg(isnull(Foo,11))))), 'a', 'b'), 'a', 'b') as hippie")
    [<Case>]
    static member TestTextUtil() =
        let emp = Table "employee"
        let wrapped = TextUtil.TextWrap "-" 10 (Array.replicate 10 "hey!") |> String.concat "&"
        Assert.AreEqual(wrapped, "-hey!hey!hey!&-hey!hey!hey!&-hey!hey!hey!&-hey!")
        let il = TextUtil.Interleave 777 [1;2;3;4] |> List.ofSeq
        Assert.AreEqual([1;777;2;777;3;777;4],il)
        let manycols = Array.replicate 20 emp?a
        let c =
            [
                Columns manycols
            ] |> rendersTo "  employee.a, employee.a, employee.a, employee.a, employee.a, employee.a, employee.a\n  , employee.a, employee.a, employee.a, employee.a, employee.a, employee.a, employee.a\n  , employee.a, employee.a, employee.a, employee.a, employee.a, employee.a"

        ()

[<EntryPoint>]
let main argv =
    TRunner.CrashHard <- false
    TRunner.AddTests<Tests>()
    TRunner.RunTests()
    TRunner.ReportAll()

    0 // return an integer exit code
