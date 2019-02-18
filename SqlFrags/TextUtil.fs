namespace SqlFrags

module TextUtil =
    // like string join kinda thing
    let Interleave (joiner : 'a) (parts : 'a seq) =
        parts
        |> Seq.collect (fun part -> [ joiner; part ])
        |> Seq.skip 1

    let Colonize(ss : string seq) = Interleave ", " ss

    let public TextWrap (indent : string) (width : int) (parts : string seq) =
        let mutable accepted = ResizeArray<_>()
        let mutable cur = 0
        let result = ResizeArray<_>()
        for p in parts do
            accepted.Add(p)
            cur <- cur + p.Length
            if cur >= width then
                do result.Add(indent + (String.concat "" accepted))
                   cur <- 0
                   accepted.Clear()
        result.Add(indent + (String.concat "" accepted))
        result
