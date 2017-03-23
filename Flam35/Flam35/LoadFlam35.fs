module LoadFlam35

open System
open System.Linq
open System.Text.RegularExpressions
open System.Xml
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

open Flame
open XmlHelper
open FormatHelper


let parseCode (node:XmlNode) =
    (node==>"var").Cast<XmlNode>()
    |> Seq.map (fun node ->
        let name = node@!>"name"
        let parameters = Regex.Split ((node@?=>("parameters","")).Trim(), "\s+") |> Array.filter (fun i -> i.Length<>0)
        let code = node.InnerText
        (node@!>"name", {name=name; parameters=parameters; code=code}))
    |> Map.ofSeq

let rec findCode (codeMaps:(Map<string,varCode> list)) name =
    match codeMaps with
    | code::tail ->
        match code.TryFind name with
        | Some code -> code
        | None -> findCode tail name
    | _ -> failwithf "Unable to find code for variation: %s" name

let parseVars (node:XmlNode) (codeMaps:(Map<string,varCode> list)) =
    node.ChildNodes.Cast<XmlNode>()
    |> Seq.map (fun node ->
        let weight,parameters =
            node.Attributes.Cast<XmlNode>()
            |> Seq.map (fun parameter ->
                let value = parameter.Value |> Single.Parse
                (parameter.Name,value))
            |> List.ofSeq
            |> List.partition (fun (name,_) -> name="weight")
        let weight = snd weight.[0]
        let parameters = parameters |> Map.ofList
        let code = findCode codeMaps node.Name
        {weight=weight; parameters=parameters; desc=code} )
    |> Array.ofSeq
            
let parseAffine (node:XmlNode) =
    let affineType = node@!>"type"
    match affineType with
    | "2D affine" ->
        let coefs = 
            (node.InnerText.Trim(), "\s+")
            |> Regex.Split
            |> Array.map (fun i -> Single.Parse i)
        let m = matrix [[coefs.[0]; coefs.[1]]
                        [coefs.[3]; coefs.[4]]]
        let o = vector  [coefs.[2]; coefs.[5]]
        Affine2D(m,o)    
    | "3D affine" ->
        let coefs = 
            (node.InnerText.Trim(), "\s+")
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
    node.ChildNodes.Cast<XmlNode>()
    |> Seq.map (fun target ->
        let node = nodeDictionary.[target.Name]
        let weight = target@?=>("weight","1.0") |> Single.Parse
        (weight,node))
    |> List.ofSeq

//parse <targets> and <continue>
let parseNodeTargets (node:XmlNode) (nodeDictionary : Map<string,node>) (states : Set<string>)=
    let mutable states = states
    let mutable links = list.Empty
    for target in node.ChildNodes do
        let newLinks = 
            match target.Name with
            | "return" ->
                target.ChildNodes.Cast<XmlNode>()
                |> Seq.map (fun target ->
                    states <- states.Add target.Name
                    let weight = target@?=>("weight","1.0") |> Single.Parse
                    Return (weight,target.Name))
                |> List.ofSeq
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
    for node in node.ChildNodes do
        let state = node.Name
        states <- states.Add state
        let setStates =
            node.Attributes.Cast<XmlNode>()
            |> Seq.map (fun attr ->
                match attr.Name with
                | "opacity" -> 
                    let opacity = attr.Value |> Single.Parse
                    Opacity (opacity, state)
                | name -> failwithf "%s is not a valid state target in <state>" name)
            |> List.ofSeq
        setStatesList <- setStatesList @ setStates
    setStatesList, states

let parseNode (node:XmlNode) (transformations:Map<string,transformation>) (states: Set<string>)=
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
    states

let parseCamera (node:XmlNode) =
    match node.ChildNodes.Count with
    | 1 -> 
        let node = node.ChildNodes.[0]
        let parseLookAt (node:XmlNode) =
            let target = 
                (node@!>"target".Trim(),"\s+")
                |> Regex.Split
                |> Array.map (fun i -> Single.Parse i)
                |> List.ofArray
                |> vector
            let radius = node@!>"radius" |> Single.Parse
            let up =
                (node@!>"up".Trim(),"\s+")
                |> Regex.Split
                |> Array.map (fun i -> Single.Parse i)
                |> List.ofArray
                |> vector
            target,radius,up
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
            | _ -> failwith "Error in <palette>: normalized attribute must be 'true' or 'false'"
        (node.InnerText.Trim(),"\s+")
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
        (node=>"transformations").ChildNodes.Cast<XmlNode>()
        |> Seq.map (fun transformationNode ->
            let transformation = parseTransformation transformationNode code
            (transformationNode.Name, transformation))
        |> Map.ofSeq
    let mutable states = Set.empty
    let nodes =
        let mutable nodes = Map.empty
        (node=>"nodes").ChildNodes.Cast<XmlNode>().ToArray()
        |> Array.map (fun nodeXml ->
            let name = nodeXml.Name
            let node,newStates = parseNode nodeXml transformations states
            nodes <- nodes.Add (name,node)
            states <- newStates
            node,nodeXml)
        |> Array.iter (fun (node,nodeXml) ->
            let newStates = linkNode node nodeXml nodes states
            states <- newStates)
        nodes
    {
        states = states
        nodes = nodes
        transformations = transformations
        camera = camera
        gamut = gamut
        palette = palette
        code = code
    }

//TODO
let parseAnimation (node:XmlNode) =
    None

let parseFlameDocument (node:XmlNode) =
    let code = 
        match node =?>"code" with
        | Some codeNode -> 
            [(parseCode codeNode)]
        | None -> []
    let flames = 
        (node==>"flame").Cast<XmlNode>()
        |> Seq.map (fun node ->
            (parseFlame node code))
        |> Array.ofSeq
    let animation = parseAnimation node
    {
        animation = animation
        flames = flames
    }

let parseFlames (xml:string) =
    let doc = new XmlDocument ()
    try
        doc.LoadXml xml
    with
        | _ -> failwith "Invalid XML"
    parseFlameDocument doc.DocumentElement