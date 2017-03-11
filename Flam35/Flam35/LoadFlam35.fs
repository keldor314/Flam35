module LoadFlam35

open System
open System.Text.RegularExpressions
open System.Xml
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

open Flame
open XmlHelper


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

let parseStates (node:XmlNode) (states: Set<string>) =
    let mutable states = states
    for target in node.ChildNodes do
        let state = target.Name

        ()

let preParseNode (node:XmlNode) (transformations:Map<string,transformation>) =
    let name = node.Name
    let transformaition = transformations.[node@!>"transformation"]

    ()

let parseNode (node:XmlNode) (nodeDictionary : Map<string,node>) =
    ()

let parseFlame node =
    ()

let parseAnimation node =
    ()

let parseFlameDocument (xml:XmlNode) =
    ()

let parseFlames (xml:string) =
    let doc = new XmlDocument ()
    try
        doc.LoadXml xml
    with
        | _ -> failwith "Invalid XML"
    parseFlameDocument doc.DocumentElement