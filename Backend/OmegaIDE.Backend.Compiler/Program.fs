// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open C_Omega
open C_Omega.Helpers
open OmegaIDE
open OmegaIDE.FSharp
open OmegaIDE.FSharp.Compiler
open OmegaIDE.FSharp.Config
open OmegaIDE.FSharp.IPC
open OmegaIDE.FSharp.Project
open OmegaIDE.FSharp.KVFile
open System.Collections.Generic
[<EntryPoint>]
let main argv = 
    printfn "Compiler daemon running"
    let ipc = IPC(ep anyv4 0, ep (ip argv.[0]) (int argv.[1]))
    ipc.Send {parts = [String "bound to"; Bytes(_int32_b ipc.LocalEndpoint.Port)]}
    let mutable config = System.IO.File.ReadAllText argv.[2] |> Config.OfString
    let compilers = new Dictionary<string,Compiler>()
    while true do
        try
            match ipc.Receive().parts with
            |[String "compile";String project] ->
                printfn "Active!"
                let project = project |> KVFile.OfString |> ProjectFile.OfKVFile
                match compilers.TryGetValue project.nodes.Head.language with //evil hack to get the language. I will deal with this later.
                |true,compiler -> 
                    match compiler.CompileProject(project).Value with
                    |{guid = g} -> ipc.Send {parts = [String "compiling"; Bytes <| g.ToByteArray()]}
                |_      -> ipc.Send {parts = [String "bad language"]}
            |[String "load";   String compiler] ->
                printfn "%A" (KVFile.OfString compiler).nodes
                let cf = compiler |> KVFile.OfString |> CompilerFile.FromKVFile config
                let compiler = cf.GetCompiler().Value
                List.iter (fun l -> 
                    try compilers.Remove(l) |> ignore with |_ -> () //If we already have a setup for this language, remove it
                    compilers.Add   (l,compiler) 
                ) cf.langs  
                ipc.Send {parts = [String "loaded"]}
            |[String "reload"; String "config"] -> config <- System.IO.File.ReadAllText argv.[2] |> Config.OfString;
            |e -> failwithf "Didn't match: %A" e
        with
        |e -> {parts = [String "exn"; String (e.ToString())]} |> ipc.Send
    done
    0 // return an integer exit code
