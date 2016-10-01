namespace OmegaIDE.FSharp
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open System.Text.RegularExpressions
[<AutoOpen>]
module Core =
    let checksum (b:byte[]) = using (new System.Security.Cryptography.SHA512Managed()) (fun i -> i.ComputeHash b)
    let tobase64 = System.Convert.ToBase64String
    let ofbase64 = System.Convert.FromBase64String
    let utf8 = System.Text.Encoding.UTF8
    let tostring = utf8.GetString
    let ofstring(s:string) = utf8.GetBytes s
    //let remove_whitespace s = System.Text.RegularExpressions.Regex.Replace(s,"[ \n\t]","")
    let getgroups (c:Match) = c.Groups |> Seq.cast<Group> |> Seq.skip 1 |> Seq.map (fun (c:Group) -> c.Value)
    let regex s = Regex(s,RegexOptions.Singleline)
    let regexremove r s = Regex.Replace(s,r,"",RegexOptions.Singleline)
    let udpv4() = new System.Net.Sockets.Socket(System.Net.Sockets.AddressFamily.InterNetwork,System.Net.Sockets.SocketType.Dgram,System.Net.Sockets.ProtocolType.Udp)
    let start a = 
        function
        |[||] -> System.Diagnostics.Process.Start(a:string)
        |  b  -> System.Diagnostics.Process.Start(a,String.concat " " b)
type PartType = 
    |Bytes      = 0uy
    |String     = 1uy
    |KeyValue   = 2uy
type Part = 
    |Bytes      of byte[]
    |String     of string
    |KeyValue   of string * string
    static member OfByteArray(b:byte[]) = 
        match b.[0] |> enum with
        |PartType.Bytes    -> let i = _b_int32 b.[1..4] in Bytes(b.[5..i+4]),i+5
        |PartType.String   -> let i = Array.findIndex ((=) 0uy) b in String(tostring b.[1..i-1]),i+1
        |PartType.KeyValue -> 
            let i = Array.findIndex ((=) 0uy) b
            let j = Array.findIndex ((=) 0uy) b.[i+1..] + (i+1)
            KeyValue(tostring b.[1..i-1],tostring b.[i+1..j-1]),j+1
        |i                       -> failwithf "Bad part type %x" (deEnum i)
    member x.ToByteArray() = 
            match x with
            |Bytes      b   -> Array.concat [|[|deEnum PartType.Bytes|];_int32_b b.Length;b|]
            |String     s   -> Array.concat [|[|deEnum PartType.String|];s |> ofstring;[|0uy|]|]
            |KeyValue (a,b) -> Array.concat [|[|deEnum PartType.KeyValue|];ofstring a;[|0uy|];ofstring b;[|0uy|]|]
