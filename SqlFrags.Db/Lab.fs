namespace SqlFrags.Db

open System.IO
open SqlFrags.SqlGen
open SqlFrags.Db
open System.Diagnostics

module Lab =
    let Results = ResizeArray<string*string>()

    let DumpResults() =
        Results |> Seq.iter (fun (a,b) -> printfn "%s\t%s" a b)

    let WriteTo outFile query =
        File.WriteAllText(outFile, Frags.Emit SqlSyntax.Any query)
    let Bench conn name count frags =
        let sql = Frags.Emit SqlSyntax.Any frags
        let sw = Stopwatch.StartNew()
        for i in [0..count] do
            QueryRunner.Query conn sql |> ignore
        Results.Add (name, sprintf "%f" ((float sw.ElapsedMilliseconds) / (float count)))

    let Dump conn frags =
        let sql = Frags.Emit SqlSyntax.Any frags
        printfn "%s\n---" sql
        let res = QueryRunner.Query conn sql
        RawTableRec.Dump res


