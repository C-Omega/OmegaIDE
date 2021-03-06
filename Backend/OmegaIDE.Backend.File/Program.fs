﻿// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open OmegaIDE.FSharp
open OmegaIDE.FSharp.IPC
open OmegaIDE.FSharp.Project
open OmegaIDE.FSharp.Highlighting
open OmegaIDE.FSharp.Config
open OmegaIDE.FSharp.KVTree
open System.IO
[<EntryPoint>]
let main argv = 
    //{conf = [Branch("branch1",[Leaf("leaf1","value");Leaf("leaf2","value");Branch("branch2",[])])]} 
    //|> side (printfn "%A") |> string |> side (printfn "%A") |> Config.OfString |> printfn "%A"
    printfn "File daemon running"
    let ipc = IPC(ep anyv4 0, ep (ip argv.[0]) (int argv.[1]))
    ipc.Send {parts = [String "bound to"; Bytes(_int32_b ipc.LocalEndpoint.Port)]}
    let mutable config = File.ReadAllText argv.[2] |> Config.OfString
    try
        match ipc.Receive() with
        |{parts = [String "get";String what;String path]} as p ->
            match what with
            |"project"      -> {parts = [String "got"; String "project"    ; String path; String <| File.ReadAllText(path)]}
            |"highlighter"  -> {parts = [String "got"; String "highlighter"; String path; String <| File.ReadAllText(config.["default paths"].["highlighter"].Value + path)]}
            |"config"       -> {parts = [String "got"; String "config"     ; String path; String <| config.ToString()]}
            |s              -> {parts = [String "error"; String "unknown command"] @ p.parts}
            |> ipc.Send
        |{parts = [String "reload"; String "config"]} -> config <- File.ReadAllText argv.[2] |> Config.OfString;
        |e -> failwithf "Didn't match: %A" e
    with
    |e -> {parts = [String "exn"; String (e.ToString())]} |> ipc.Send
    0 // return an integer exit code

