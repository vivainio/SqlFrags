namespace SqlFrags.Db

open System.IO
open SqlFrags.SqlGen
open SqlFrags.Db
open System.Diagnostics

module Lab =
    let WriteTo outFile query =
        File.WriteAllText(outFile, Frags.Emit SqlSyntax.Any query)
    let Bench conn name count frags =
        let sql = Frags.Emit SqlSyntax.Any frags
        let sw = Stopwatch.StartNew()
        for i in [0..count] do
            QueryRunner.Query conn sql |> ignore
        printfn "%s: %dms" name sw.ElapsedMilliseconds











