module Helper

let which (a:'T option) (b:'T option) =
    match (a,b) with
    | Some a, Some b -> failwith "Both parameters were Some"
    | Some a, None   -> a
    | None, Some b   -> b
    | None, None     -> failwith "Both parameters were None"