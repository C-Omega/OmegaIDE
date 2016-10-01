namespace OmegaIDE.FSharp
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open System.Text.RegularExpressions
module Compiler =
    open KVFile
    open IPC
    open Project
    open KVTree
    open Config
    type State = 
        |Nothing    =   0uy
        |Compiling  =   1uy
        |IOErr      =   2uy
        |BadInput   =   3uy
        |BadSystem  =   4uy
        |BadArgs    =   5uy
        |ErrInt     =   6uy
        |Compiled   =   7uy
        |IPCErr     = 255uy
    type Job = 
        {
            get_progress : unit -> byte
            get_output   : unit -> string
            get_state    : unit -> State
            guid         : System.Guid
        }
    type Compiler(ipc:IPC) =
        member x.CompileProject (p:ProjectFile) = 
            {parts=[String "compile";String (string p)]} |> ipc.Send
            match ipc.Receive().parts with 
            |[String "job";Bytes guid] ->
                {
                    guid            = System.Guid(guid)
                    get_progress    = fun () -> 
                        ipc.Send {parts=[String "get_progress";Bytes guid]}
                        match ipc.Receive().parts with
                        |[Bytes [|b|];Bytes v ] when v = guid -> b
                        |_ -> 0uy
                    get_output = fun() ->
                        ipc.Send {parts=[String "get_output";Bytes guid]}
                        match ipc.Receive().parts with
                        |_ -> ""
                        |[String s;Bytes v] when v = guid -> s
                    get_state = fun() ->
                        ipc.Send {parts=[String "get_state";Bytes guid]}
                        match ipc.Receive().parts with
                        |[Bytes [|b|];Bytes v ] when v = guid -> enum b
                        |_ -> State.IPCErr
                }
                |> Some
            |_ -> None
        member x.Clear(g:System.Guid) = ipc.Send {parts = [String "clear"; Bytes (g.ToByteArray())]}
    type CompilerFile = 
        {
            path  : string
            langs : string list
            config: Config
        }
        static member FromKVFile (config:Config) (v:KVFile) = 
            {
                path=config.["default paths"].["compiler"].Value+v.["path"]
                langs=List.choose (function|"lang",v -> Some v|_ -> None) v.nodes
                config = config
            }
        member x.GetCompiler() = 
            let ipc = IPC()
            start x.path [|ipc.LocalEndpoint.Address.ToString();ipc.LocalEndpoint.Port.ToString()|]|>ignore
            match ipc.Receive().parts with 
            |[String "bound to"; Bytes b] ->
                _b_int32 b |> ep loopbackv4 |> ipc.Connect
                ipc.Send {parts = [String "config"; String <| string x.config]}
                Some(new Compiler(ipc))
            |_ -> None