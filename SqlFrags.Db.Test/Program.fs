// Learn more about F# at http://fsharp.org

open TrivialTestRunner
open SqlFrags.Db

open DbConnector
open SqlFrags.Db.QueryRunner
open SqlFrags.SqlGen

let connector = Connector(MsSql, ConnectionString [DataSource "localhost"; Catalog "IA"; IntegratedSecurity true ])




module Introspect =
    type Columns() =
        member __.T = Table "INFORMATION_SCHEMA.COLUMNS"
        member x.ColName = "COLUMN_NAME"
        member x.DataType = "DATA_TYPE"
        member x.MaxLen = "CHARACTER_MAXIMUM_LENGTH"
    type Tables() =
        member __.T = Table "INFORMATION_SCHEMA.TABLES"
        member x.Name = "TABLE_NAME"


type Test() =
    [<Case>]
    static member Connect() =
        // replace xx with your own catalog name
        let db = connector.Connect()
        db.Open()
        let res = Query db "select 1"
        Assert.AreEqual(1, res.Rows.[0].[0] :?> int)

    [<Case>]
    static member Lab() =
        let db = connector.Connect()
        db.Open()
        [
            Raw <| "Select 1"
        ] |> Lab.Bench db "Select 1" 500

        let cols = Introspect.Columns()

        [
            SelectS [
                Funcs.Count cols.ColName |> Funcs.Convert "bigint" |> Funcs.As "countti";
                cols.DataType |> Funcs.ReplaceQuoted "int" "INTTI"
            ]
            //SelectS [ cols.T?DATA_TYPE; ]

            From cols.T
            GroupBy [cols.DataType]
        ] |> Lab.Dump db
        Lab.DumpResults()


[<EntryPoint>]
let main argv =
    TRunner.AddTests<Test>()
    TRunner.RunTests()
    TRunner.ReportAll()
    0 // return an integer exit code
