// Learn more about F# at http://fsharp.org

open TrivialTestRunner
open Fapper.Db

open DbConnector
type Test() =
    [<Case>]
    static member Connect() =
        // replace xx with your own catalog name
        DefaultConnector <- Connector(MsSql, ConnectionString [DataSource "localhost"; Catalog "IA"; IntegratedSecurity true ])
        let c = Conn()
        c.Connection.Open()
        let res = c.Query "select 1"
        Assert.AreEqual(2, res.Rows.[0].[0] :?> int)


[<EntryPoint>]
let main argv =
    TRunner.AddTests<Test>()
    TRunner.RunTests()
    TRunner.ReportAll()
    0 // return an integer exit code
