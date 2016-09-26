// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open OmegaIDE.FSharp
open OmegaIDE.FSharp.IPC
[<EntryPoint>]
let main argv = 
    let file = IPC(ep loopbackv4 0,ep loopbackv4 0)
    let f = System.Diagnostics.Process.Start(argv.[0],
    file.LocalEndpoint,file.RemoteEndpoint |> printfn "%A"
    printfn "%A" argv
    0 // return an integer exit code

