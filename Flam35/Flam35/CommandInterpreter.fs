module CommandInterpreter
open CommandLine

let executeCommands commands =
    Environment.quality    <- commands.quality
    Environment.resolution <- commands.resolution
    if commands.verbose then Environment.verbose <- true
    if commands.help then printfn "%s" CommandLine.helpString
    if commands.listDevices then
        let count = ManagedCuda.PrimaryContext.GetDeviceCount ()
        printfn "Devices:"
        for i in 0..count-1 do
            printfn "%i: %s" i <| ManagedCuda.PrimaryContext.GetDeviceName i
        printfn ""
    for command in commands.commands do
        match command with
        | RenderFrame (input,output,frameTime) -> ()
        | RenderAnimation (input,output,frameCount,startTime,stopTime) -> ()
        | Convert (input,output) -> ()