module CommandLine

let helpString =
    "
Flam35:

Options:
    (-renderFrame <input> <output> [-frameTime <time>])...
    (-renderAnimation <input> <output> <frameCount> [-startTime <time>] [-stopTime <time>])...
    (-convert <input.flam3> <output.flam35>)...
    -resolution <width> <height>
    -quality <quality>
    --verbose
    --help
"

type command =
    | RenderFrame of
        input       : string *
        output      : string *
        frameTime   : float
    | RenderAnimation of
        input       : string *
        output      : string *
        frameCount  : int *
        startTime   : float *
        stopTime    : float
    | Convert of
        input       : string *
        output      : string

type commands = {
    commands    : command list
    resolution  : int*int
    quality     : float
    isVerbose   : bool
    isHelp      : bool
}

let parse commandLine =
    let rec parse acc commands = 
        try
            match commands with
            | "-renderFrame"::input::output::tail ->
                match tail with 
                | "-frameTime"::time::tail ->
                    let command = RenderFrame (input, output, float time)
                    parse {acc with commands = command::acc.commands} tail
                | _ ->
                    let command = RenderFrame (input,output, 0.0)
                    parse {acc with commands = command::acc.commands} tail 
            | "-renderAnimation"::input::output::frameCount::tail ->
                let rec getDuration commands duration =
                    let start,stop = duration
                    match commands with
                    | "-startTime"::time::tail ->
                        getDuration tail (float time,stop)
                    | "-stopTime"::time::tail ->
                        getDuration tail (start,float time)
                    | _ -> duration
                let start,stop = getDuration tail (0.,System.Double.MaxValue)
                let command = RenderAnimation (input,output,int frameCount,start,stop)
                parse {acc with commands = command::acc.commands} tail
            | "-convert"::input::output::tail ->
                let command = Convert (input,output)
                parse {acc with commands = command::acc.commands} tail
            | "-resolution"::x::y::tail ->
                parse {acc with resolution = (int x, int y)} tail
            | "-quality"::quality::tail ->
                parse {acc with quality = float quality} tail
            | "--verbose"::tail ->
                parse {acc with isVerbose = true} tail
            | "--help"::tail ->
                parse {acc with isHelp = true} tail
            | badCommand::tail ->
                failwith ""
            | _ ->
                acc
        with
            _ -> failwithf "Invalid argument: %s" commands.Head
    let emptyCommands =
        {
            commands = []
            resolution = (1920,1080)
            quality = 1000.
            isVerbose = false
            isHelp = false
        }
    parse emptyCommands commandLine