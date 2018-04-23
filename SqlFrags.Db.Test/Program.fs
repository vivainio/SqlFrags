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
        member x.ColName = x.T?COLUMN_NAME
        member x.ColDataType = x.T?DATA_TYPE


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
            Select [cols.T?COLUMN_NAME.WrapIn("sum") ; cols.T?DATA_TYPE]
            From cols.T
            GroupBy ["DATA_TYPE"]
        ] |> Lab.Dump db
        Lab.DumpResults()


[<EntryPoint>]
let main argv =
    TRunner.AddTests<Test>()
    TRunner.RunTests()
    TRunner.ReportAll()
    0 // return an integer exit code
