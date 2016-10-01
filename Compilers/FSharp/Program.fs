// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open OmegaIDE.FSharp
open OmegaIDE.FSharp.Compilers
open OmegaIDE.FSharp.IPC
open OmegaIDE.FSharp.ProjectFile
open OmegaIDE.FSharp.Config
open OmegaIDE.FSharp.KVFile
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open System.Collections.Generic
let jobs = new Dictionary<System.Guid,Job>()
[<EntryPoint>]
let main argv = 
    let v = SimpleSourceCodeServices()
    let ipc = IPC()
    ep (ip argv.[0]) (int argv.[1]) |> ipc.Connect
    {parts = [String "bound to";Bytes(_int32_b ipc.LocalEndpoint.Port)]} |> ipc.Send
    let config = Config.OfString <| (ipc.Receive().parts |> function |[String "config";String s] -> s |_ -> failwith "No config")
    while true do
        try
            match ipc.Receive().parts with
            |[String "compile"; String proj] ->
                printfn "Compiling..."
                let guid = System.Guid.NewGuid()
                let progress,output,state = ref 0uy,ref "",ref State.Compiling
                jobs.Add(guid,{get_progress = justreturn progress >> (!); get_output = justreturn output >> (!); get_state = justreturn state >> (!); guid = guid})
                async{
                    printfn "\tJob added"
                    let project = proj |> KVFile.KVFile.OfString |> ProjectFile.OfKVFile
                    let inputs = List.choose (function |{location=s;compilemode="compile"} -> Some s|_ -> None) project.nodes |> Array.ofList
                    let refs   = List.choose (function |{location=s;compilemode="reference"} -> Some ("--reference:" + s)|_ -> None) project.nodes |> Array.ofList 
                    let search = List.choose (function |{location=s;compilemode="search"} -> Some ("--lib:" + s)|_ -> None) project.nodes |> Array.ofList 
                    let additional = [|inputs;refs;search|] |> Array.concat
                    let errs,i = 
                        match project.buildmode with
                        |"library" ->
                            v.Compile(Array.append [|"fsharpc.exe";"-a";project.name|] additional)
                        |"exe" ->
                            v.Compile(Array.append [|"fsharpc.exe";"--target:exe";"-o:"+project.name+".exe"|] additional)
                        |_ -> failwith "Invalid buildmode"
                    printfn "\tFinished compiling"
                    if i <> 0 then
                        printfn "Failed!"
                        printfn "%A" <| Array.map string errs
                        //Something went wrong. Let's work out what
                        errs
                        |> Array.filter (fun i -> i.Severity = FSharpErrorSeverity.Error)
                        |> Array.map(fun i -> i.ErrorNumber)
                        |> Seq.tryPick(function 
                        //Bad inputs:
                            //no inputs
                            | 207
                            //bad ext
                            | 226 -> Some State.BadInput
                        //Bad args:
                            | 243 -> Some State.BadArgs
                        //Internal
                            //bad target
                            |1048 -> Some State.ErrInt

                            |_ -> None
                        )
                        |> defaultArg
                        <| State.ErrInt
                    else 
                        State.Compiled

                    |> (:=) state
                    output := (String.concat "\n" <| Array.Parallel.map string errs)
                    progress := 255uy
                } |> Async.Start
            |_ -> failwith "Bad packet"
        with
        |_ -> ()
    0 // return an integer exit code

