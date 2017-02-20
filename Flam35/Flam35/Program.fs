module Program

[<EntryPoint>]
let main argv = 
    let commands = CommandLine.parse (List.ofArray argv)
    0 // return an integer exit code
