namespace OmegaIDE.FSharp
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open System.Text.RegularExpressions
module IPC = 
    type Packet = 
        {
            parts : Part list
        }
        static member OfByteArray(b:byte[]) = 
            {parts = 
                List.unfold (function
                    |[||] -> None
                    |arr  -> let p,i = Part.OfByteArray(arr) in Some(p,arr.[i..])
                    ) b
            }
        static member ToByteArray(x) = List.fold(fun acc (elem:Part) -> elem.ToByteArray() :: acc) [] x.parts |> List.rev |> Array.concat
    open System.Net
    open System.Net.Sockets
    type IPC (socket:Socket) = 
        //let socket = new UdpClient(local)
        //do socket.Connect(remote)
        member x.Bind(ep:IPEndPoint) = socket.Bind ep
        member x.Connect(ep:IPEndPoint) = socket.Connect ep
        member x.LocalEndpoint = socket.LocalEndPoint :?> IPEndPoint
        member x.RemoteEndpoint= socket.RemoteEndPoint:?> IPEndPoint
        member x.Send(p:Packet) = let b = Packet.ToByteArray p in socket.Send(b) |> ignore
        member x.Receive() = 
            spinuntil(fun () -> socket.Available > 0) //poll the socket
            //let v = ref x.RemoteEndpoint
            let b = Array.zeroCreate<byte> socket.Available
            b.[..socket.Receive(b)-1]
            |> Packet.OfByteArray
        new(local:IPEndPoint,remote:IPEndPoint) as x = new IPC(local) then x.Connect remote
        new(local:IPEndPoint)                   as x = new IPC(udpv4()) then x.Bind local
        new()                                        = new IPC(ep anyv4 0)