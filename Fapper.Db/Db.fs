namespace SqlFrags.Db

open System.Data.SqlClient
open System
open System.Data


module DbConnector =
    open System.Diagnostics

    let MsSql = typeof<System.Data.SqlClient.SqlConnection>.AssemblyQualifiedName
    // would need references to the libs so meh
    //let OleDb = typeof<System.Data.OleDb.OleDbConnection>.AssemblyQualifiedName
    //let ODP = typeof<Oracle.ManagedDataAccess.Client.OracleConnection>.AssemblyQualifiedName

    [<DebuggerDisplay("{provider} -> {connectionString}")>]
    type Connector(provider: string, connectionString: string) =
        let createConnection() =
            let typ = Type.GetType(provider)
            System.Activator.CreateInstance(typ, connectionString) :?> IDbConnection

        let lazyConnection = lazy(createConnection())

        // reuse same dbconnection instance.
        member x.Reuse() =
            let conn = lazyConnection.Value
            if conn.State <> ConnectionState.Open then conn.Open()
            conn

        // create new connection. You need to call conn.Open() yourself
        member x.Connect() =
            let conn = createConnection()
            conn

        member x.Show() = sprintf "%s :: %s" (provider.Split(',').[0]) connectionString

    type ConnectionStringFrag =
    | Provider of string
    | ProviderAccess
    | DataSource of string
    | Catalog of string
    | UserId of string
    | Password of string
    | IntegratedSecurity of bool

    with
        member x.Str = match x with
                       | Provider s -> "Provider=" + s
                       | DataSource s -> "Data Source=" + s
                       | Catalog s -> "Initial Catalog=" + s
                       | IntegratedSecurity true -> "Integrated Security=True"
                       | IntegratedSecurity false -> "Integrated Security=no"
                       | ProviderAccess -> "Provider=Microsoft.ACE.OLEDB.12.0"
                       | Password s -> "Password="+s
                       | UserId s -> "User Id=" + s

    let ConnectionString (parts: ConnectionStringFrag seq) =
        parts |> Seq.map (fun f -> f.Str) |> String.concat ";"

    let OracleConnectionString host port service username password =
        sprintf "Data Source=(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=%s)(PORT=%d))(CONNECT_DATA=(SERVICE_NAME=%s)));User Id=%s;Password=%s;" host port service username password

    let mutable DefaultConnector =
        Connector(MsSql, ConnectionString [ DataSource "localhost"; Catalog "xx"; IntegratedSecurity true])


open DbConnector


// data as read from raw db query
type RawTableRec = {
    Name: string
    Header: string[]
    Rows: obj [] []
}
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module RawTableRec =

    let FilterCols (cols: string[]) (table: RawTableRec) =

        let indices = table.Header |> Array.mapi (fun i h -> if Array.contains h cols then Some(i) else None) |> Array.choose id

        seq {
            for row in table.Rows do
                yield (indices |> Array.map (row |> Array.get))
        }

    let ChooseCols f (table: RawTableRec) =
        seq {
            for row in table.Rows do

                let chosen =
                    row
                    |> Array.mapi (fun i col -> (row, table.Header.[i], col))
                    |> Array.choose f
                yield chosen
        }

    let KeyValPairs (trec: RawTableRec) =
        seq {
            for row in trec.Rows do
                yield (trec.Header |> Array.mapi (fun i hname -> (hname, row.[i])))
        } |> Array.ofSeq


type Conn() =
    let readToArr (r: SqlDataReader) =
        [| 0..(r.FieldCount-1) |] |> Array.map r.GetValue

    let connectToDefault() =
        let connector = DefaultConnector
        connector.Connect()

    let conn = connectToDefault()

    member x.Connection = conn

    // not really useful with mssql
    member x.WithObjs sql (values: obj seq) =
        let q = conn.CreateCommand()
        q.CommandText <- sql
        for v in values do

            let p = q.CreateParameter()
            p.Value <- v
            q.Parameters.Add(p) |> ignore
        x.QueryImpl q

    member x.WithParams sql (values: (string*obj) seq) =
        let q = conn.CreateCommand()
        q.CommandText <- sql
        for (k,v) in values do
            let p = q.CreateParameter()
            p.ParameterName <- k
            p.Value <- v
            q.Parameters.Add(p) |> ignore
        x.QueryImpl q

    member x.Query sql =
        let q = conn.CreateCommand()
        q.CommandText <- sql;
        x.QueryImpl q


    // tableName is optional informational argument, set to "" if don't care
    member __.QueryImpl q =
        use reader = q.ExecuteReader()
        let headers = [| 0..(reader.FieldCount-1) |] |> Array.map reader.GetName
        {
            Name = ""
            Header = headers;
            Rows =
                seq {
                    while reader.Read() do
                        let valArray = Array.zeroCreate reader.FieldCount
                        reader.GetValues(valArray) |> ignore
                        yield valArray
                } |> Seq.toArray
        }
