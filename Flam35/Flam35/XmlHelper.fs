module XmlHelper

open System.Xml
open System.Xml.XPath

let (<+) (parent:XmlNode) (child:XmlNode) =
    match child with
    | :? XmlAttribute as child->
        parent.Attributes.Append child :> XmlNode
    | _ ->
        parent.AppendChild child

let (=?>) (node:XmlNode) name =
    match node.SelectSingleNode <| sprintf "child::%s" name with
    | null -> None
    | node -> Some node

let (=>) (node:XmlNode) name =
    match node.SelectSingleNode <| sprintf "child::%s" name with
    | null -> failwithf "<%s> missing child node: <%s>" node.Name name
    | node -> node

let (==>) (node:XmlNode) name =
    node.SelectNodes <| sprintf "child::%s" name

let (=?) (node:XmlNode) name =
    let nodes = node.SelectNodes <| sprintf "child::%s" name
    if nodes.Count > 0 then true else false

let (@?>) (node:XmlNode) (name,defaultVal) =
    match node.Attributes.GetNamedItem(name) with
    | null ->
        defaultVal
    | attr ->
        attr.Value

let (@!>) (node:XmlNode) (name) =
    match node.Attributes.GetNamedItem(name) with
    | null ->
        failwithf "<%s> missing required attribute: %s" node.Name name
    | attr ->
        attr.Value

let (@?) (node:XmlNode) (name) =
    match node.Attributes.GetNamedItem(name) with
    | null ->
        false
    | _ ->
        true