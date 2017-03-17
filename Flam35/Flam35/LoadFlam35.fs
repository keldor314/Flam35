module LoadFlam35

open System
open System.Text.RegularExpressions
open System.Xml
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

open Flame
open XmlHelper
open FormatHelper


let parseCode (node:XmlNode) =
    let mutable vars = Map.empty
    for node in node==>"var" do
        let parameters = Regex.Split (node@?=>("parameters",""), "\W")
        let code = node.InnerText
        vars <- vars.Add (node@!>"name", {parameters=parameters; code=code})
    vars

let rec findCode (codeMaps:(Map<string,varCode> list)) name =
    match codeMaps with
    | code::tail ->
        match code.TryFind name with
        | Some code -> code
        | None -> findCode tail name
    | _ -> failwithf "Unable to find code for variation: %s" name

let parseVars (node:XmlNode) (codeMaps:(Map<string,varCode> list)) =
    let mutable vars = List.Empty
    for node in node==>"vars" do
        let mutable weight = 0.f
        let mutable parameters = Map.empty
        for parameter in node.Attributes do
            let value = parameter.Value |> Single.Parse
            match node.Name with
            | "weight" -> weight <- value
            | name -> parameters <- parameters.Add (name, value)
        let code = findCode codeMaps node.Name
        vars <- {weight=weight; parameters=parameters; desc=code} :: vars
    vars |> List.rev |> Array.ofList
            
let parseAffine (node:XmlNode) =
    let affineType = node@!>"type"
    match affineType with
    | "2D affine" ->
        let coefs = 
            (node.InnerText, "\W")
            |> Regex.Split
            |> Array.map (fun i -> Single.Parse i)
        let m = matrix [[coefs.[0]; coefs.[1]]
                        [coefs.[3]; coefs.[4]]]
        let o = vector  [coefs.[2]; coefs.[5]]
        Affine2D(m,o)    
    | "3D affine" ->
        let coefs = 
            (node.InnerText, "\W")
            |> Regex.Split
            |> Array.map (fun i -> Single.Parse i)
        let m = matrix [[coefs.[0]; coefs.[1]; coefs.[2] ]
                        [coefs.[4]; coefs.[5]; coefs.[6] ]
                        [coefs.[8]; coefs.[9]; coefs.[10]]]
        let o = vector  [coefs.[3]; coefs.[7]; coefs.[11]]
        Affine3D(m,o)
    | name -> failwithf "Unknown affine type: %s" name

let parseTransformation (node:XmlNode) codemaps =
    let affine = 
        match node=?>"affine" with
        | Some node -> Some <| parseAffine node
        | None -> None
    let vars = 
        match node =?>"vars" with
        | Some vars -> parseVars vars codemaps
        | None -> [||]
    {affine = affine; vars = vars}
      
let parseNodeLinks (node:XmlNode) (nodeDictionary : Map<string,node>) =
    let mutable links = list.Empty
    for target in node.ChildNodes do
        let node = nodeDictionary.[target.Name]
        let weight = target@?=>("weight","1.0") |> Single.Parse
        links <- (weight,node) :: links
    links

//parse <targets> and <continue>
let parseNodeTargets (node:XmlNode) (nodeDictionary : Map<string,node>) (states : Set<string>)=
    let mutable states = states
    let mutable links = list.Empty
    for target in node.ChildNodes do
        let newLinks = 
            match target.Name with
            | "return" ->
                let mutable newLinks = list.Empty
                for target in target.ChildNodes do
                    states <- states.Add target.Name
                    let weight = target@?=>("weight","1.0") |> Single.Parse
                    newLinks <- Return (weight,target.Name) :: newLinks
                newLinks
            | _ ->
                parseNodeLinks target nodeDictionary
                |> match target.Name with
                    | "traverse" ->
                        List.map (fun i -> Traverse i)
                    | "enter" -> 
                        states <- states.Add <| target@!>"state"
                        List.map (fun i -> Enter i)
                    | "leaveTo" -> 
                        states <- states.Add <| target@!>"state"
                        List.map (fun i -> LeaveTo i)
                    | invalid -> failwithf "Invalid target type: <%s>" invalid
        links <- links @ newLinks
    links, states

//parse <state>
let parseStates (node:XmlNode) (states: Set<string>) =
    let mutable states = states
    let mutable setStatesList = List.empty
    for target in node.ChildNodes do
        let state = target.Name
        states <- states.Add state
        let mutable setStates = List.Empty
        for attr in node.Attributes do
            match attr.Name with
            | "opacity" -> 
                let opacity = attr.Value |> Single.Parse
                setStates <- Opacity (opacity, state) :: setStates
            | name -> failwithf "%s is not a valid state target in <state>" name
        setStatesList <- setStatesList @ setStates
    setStatesList, states

let parseNode (node:XmlNode) (transformations:Map<string,transformation>) (states: Set<string>)=
    let name = node.Name
    let transformation = transformations.[node@!>"transformation"]
    let setStates, states = 
        match node=?>"state" with
        | Some stateNode ->
            let stateList, states = parseStates stateNode states
            Array.ofList stateList, states
        | None -> [||], states
    let opacity =
        let num = ref 0.f
        let isSingle = Single.TryParse (node@?=>("opacity","0.0"),num)
        if isSingle then
            Direct !num
        else
            Indirect <| node@!>"opacity"
    let colorIndex = node@?=>("colorIndex","0.0") |> Single.Parse
    let colorSpeed = node@?=>("colorSpeed","0.0") |> Single.Parse
    let targets = Array.empty
    let usePointPool = not (node=?"noPool")
    let continuation = Array.empty
    {
        transformation = transformation
        setStates = setStates
        opacity = opacity
        colorIndex = colorIndex
        colorSpeed = colorSpeed
        targets = targets
        usePointPool = usePointPool
        continuation = continuation
    }, states

let linkNode (node:node) (nodeXml:XmlNode) (nodeDictionary : Map<string,node>) (states : Set<string>) =
    let targets,states = parseNodeTargets (nodeXml=>"targets") nodeDictionary states
    let continuation,states =
        match nodeXml=?>"continue" with
        | Some node ->
            parseNodeTargets node nodeDictionary states
        | None -> List.empty, states
    node.targets <- Array.ofList targets
    node.continuation <- Array.ofList continuation
    node, states

let parseCamera (node:XmlNode) =
    match node.ChildNodes.Count with
    | 1 -> 
        let node = node.ChildNodes.[0]
        let parseLookAt (node:XmlNode) =
            let target = 
                (node@!>"target","\W")
                |> Regex.Split
                |> Array.map (fun i -> Single.Parse i)
                |> List.ofArray
                |> vector
            let radius = node@!>"radius" |> Single.Parse
            let offset =
                (node@!>"offset","\W")
                |> Regex.Split
                |> Array.map (fun i -> Single.Parse i)
                |> List.ofArray
                |> vector
            target,radius,offset
        match node.Name with
        | "lookAtCircle" -> LookAtCircle <| parseLookAt node
        | "lookAtSphere" -> LookAtSphere <| parseLookAt node
        | "affine"       -> Affine       <| parseAffine node
        | name -> failwithf "Unknown camera type in <camera>: <%s>" name
    | _ -> failwith "<camera> must have exactly 1 child node"

let parseGamut (node:XmlNode) =
    {
        brightness  = node@!>"brightness"           |> Single.Parse
        gamma       = node@!>"gamma"                |> Single.Parse
        vibrancy    = node@?=>("vibrancy","1.0")    |> Single.Parse
    }

let parsePalette (node:XmlNode) =
    match node@!>"format" with
    | "RGB plaintext" ->
        let isNormalized =
            match node@!>"normalized" with
            | "true"  -> true
            | "false" -> false
            | _ -> failwith "Error in <palette>: normalized attribute must be boolean"
        (node.InnerText,"\W")
        |> Regex.Split
        |> Seq.ofArray
        |> partitionSeq 3
        |> Seq.map (fun color ->
            color
            |> Array.map (fun colorComponent -> 
                Single.Parse colorComponent
                |> (fun colorComponent -> if isNormalized then colorComponent else colorComponent/255.f)
                )
            |> (fun color -> {r=color.[0]; g=color.[1]; b=color.[2]} ))
        |> Array.ofSeq
    | unknown -> failwithf "Unknown <palette> format: %s" unknown

let parseFlame (node:XmlNode) (code:Map<string,varCode> list) =
    let code = 
        match node =?>"code" with
        | Some codeNode -> 
            (parseCode codeNode) :: code
        | None -> code
    let palette = parsePalette  (node=>"palette")
    let gamut   = parseGamut    (node=>"gamut")
    let camera  = parseCamera   (node=>"camera")
    let transformations =
        let transformationNodes = (node=>"transformations").ChildNodes
        let mutable transformations = Map.empty
        for transformationNode in transformationNodes do
            let transformation = parseTransformation transformationNode code
            transformations <- transformations.Add (transformationNode.Name, transformation)
        transformations
    let nodes =
        let xmlNodes = (node=>"nodes").ChildNodes
        let mutable nodes = Map.empty
        for node in xmlNodes do
            ()
    ()  //INCOMPLETE

//TODO
let parseAnimation node =
    None

let parseFlameDocument (xml:XmlNode) =
    ()

let parseFlames (xml:string) =
    let doc = new XmlDocument ()
    try
        doc.LoadXml xml
    with
        | _ -> failwith "Invalid XML"
    parseFlameDocument doc.DocumentElement