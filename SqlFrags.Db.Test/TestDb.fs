// Learn more about F# at http://fsharp.org
open TrivialTestRunner
open SqlFrags.Db
open DbConnector
open SqlFrags.Db
open SqlFrags.Db.QueryRunner
open SqlFrags.SqlGen
open System.Data.SQLite

let connector =
    let provider = typeof<System.Data.SQLite.SQLiteConnection>.AssemblyQualifiedName
    Connector(provider, ConnectionString [
        DataSource "testdb.db"
        Version 3
        Compress
    ])

module Introspect =
    module Columns =
        let T = Table "INFORMATION_SCHEMA.COLUMNS"
        let ColName = T?COLUMN_NAME
        let DataType = T?DATA_TYPE
        let MaxLen = T?CHARACTER_MAXIMUM_LENGTH

    module Tables =
        let T = Table "INFORMATION_SCHEMA.TABLES"
        let Name = "TABLE_NAME"
        let Master = Table "sqlite_master"

// alias used in Lab() test
module cols = Introspect.Columns

type Test() =

    [<Case>]
    static member Connect() =
        // replace xx with your own catalog name
        let db = connector.Connect()
        db.Open()
        let res = Query db "select 1"
        Assert.AreEqual(1L, res.Rows.[0].[0] :?> int64)

    [<Case>]    
    static member Insert() =
        let db = connector.Connect()
        db.Open()
        let t1 = Table "two"
        let query =
            [
                Alter.CreateTable t1 [
                    "Name", Text
                    "Address", Text
                ]
            ]|> Frags.Emit SqlSyntax.Any
        QueryRunner.Query db query        
    [<Case>]
    static member RunQueries() =
        let db = connector.Connect()
        db.Open()
        [ Raw <| "Select 1" ] |> Lab.Bench db "Select 1" 500
        [ cols.T.Select
              [ cols.ColName.Right
                |> Funcs.Count
                |> Funcs.Convert "bigint"
                |> Funcs.As "countti"
                cols.DataType.Right |> Funcs.ReplaceQuoted "int" "INTTI" ]
          GroupBy [ cols.DataType.Right ] ]
        |> Lab.Bench db "a" 1
        [ cols.T.SelectC [ cols.ColName; cols.DataType ] ] |> Lab.Bench db "b" 1
        let query =
            [ cols.T.SelectC [ cols.ColName; cols.DataType ]
              Conds.Where <| Conds.And [ Conds.In cols.ColName "(@a)"
                                         Conds.In cols.DataType "(@b,@c)" ] ]
            |> Frags.Emit SqlSyntax.Any

        let result =
            QueryRunner.WithParams db query [ "a", box "ID"
                                              "b", box "varchar"
                                              "c", box "nvarchar" ]

        printfn "result %A" result
        // dumps the benchmark results
        Lab.DumpResults()

[<EntryPoint>]
let main argv =
    TRunner.RunTests()
    
    TRunner.AddTests<Test>()
    TRunner.RunTests()
    TRunner.ReportAll()
    0 // return an integer exit code
                                                                                                    