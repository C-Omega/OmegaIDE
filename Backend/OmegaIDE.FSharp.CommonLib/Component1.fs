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
module ProjectFile = 
    type ProjectFileNode = 
        {location : string; language : string; platforms : string[]; buildmode:string}//checksum : byte[]}
        override x.ToString() = x.location+":"+x.language+"?"+String.concat "," x.platforms+"!"+x.buildmode //+"?"+(tobase64 x.checksum)
        //static member Checksum x = 
            //{location = x.location; language = x.language; checksum = "@"+x.location+":"+x.language |> ofstring |> checksum}
        static member OfString (s:string) = 
            let m = regex(@"^(.*):(.*)\?(.*)!(.*)$").Match(s) |> getgroups |> Array.ofSeq
            {
                location = m.[0];
                language = m.[1];
                platforms = m.[2].Split([|','|],System.StringSplitOptions.RemoveEmptyEntries);
                buildmode = m.[3];
            }
    type ProjectFile = 
        {
            name : string
            nodes : ProjectFileNode[]
        }
        override x.ToString() = "@name@"+x.name+";\n"+(Array.fold (fun acc elem -> acc + "\n@node@" + string elem + ";") "" x.nodes)
        static member OfString (s:string) = 
            //this removes any form of newline (CRLF or LF), and elimits it by semicolons
            //let s = System.Text.RegularExpressions.Regex.Replace(s,"[\r\n]","").Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            let v = regex(@"@(.*)@(.*);").Matches(s) |> Seq.cast<Match> |> Seq.map (getgroups >> Array.ofSeq) |> List.ofSeq
            //a recursive parser
            let rec parse = function
                |[],name,nodes -> name,nodes
                |(a:string[])::b,name,nodes -> 
                    //get the type and the value
                    let x,y = a.[0],a.[1]
                    match x with
                    |"name" -> parse(b,y,nodes)
                    |"node" -> parse(b,name,ProjectFileNode.OfString y :: nodes)
                    |s -> raise (System.ArgumentException(s))
            let name,nodes = parse (v,"",[])
            {name = name; nodes = Array.ofList nodes}
module Graphics = 
    type RGBA = 
        {red:byte;green:byte;blue:byte;alpha:byte}
        member x.ToByteArray() = [|x.red;x.green;x.blue;x.alpha|]
        member x.ToUInt32() = System.BitConverter.ToUInt32(x.ToByteArray(),16)
        override x.ToString() = System.Convert.ToString(x.ToUInt32())
        static member OfByteArray(b:byte[]) = {red=b.[0];green=b.[1];blue=b.[2];alpha=b.[3]}
        static member OfUInt32 (i:uint32) = System.BitConverter.GetBytes i |> Array.rev |> RGBA.OfByteArray
        static member OfString (s:string) = System.Convert.ToUInt32(s,16) |> RGBA.OfUInt32
        static member Red = {red=255uy;green=0uy;blue=0uy;alpha=0uy}
        static member Green = {red=0uy;green=255uy;blue=0uy;alpha=0uy}
        static member Blue = {red=0uy;green=0uy;blue=255uy;alpha=0uy}
        static member Black = {red=0uy;green=0uy;blue=0uy;alpha=0uy}
        static member White = {red=255uy;green=255uy;blue=255uy;alpha=0uy}
        static member (+) (a:RGBA,b) = {red = (a.red+b.red)/2uy;green = (a.green+b.green)/2uy;blue = (a.blue+b.blue)/2uy;alpha = (a.alpha+b.alpha)/2uy}
module Modules =
    open Graphics
    [<System.FlagsAttribute>]
    type Style = 
        |WarnSquiggle   =   0b00000001uy
        |ErrorSquiggle  =   0b00000010uy
        |Bold           =   0b00000100uy
        |Italics        =   0b00001000uy
        |Underline      =   0b00010000uy
    type KeywordType = 
        |Normal
        |MajorKeyword
        |MinorKeyword
        |Literal
        |LiteralString
        |Comment
        |Preprocessor//   =   5uy
    type DescribedString = {s:string; style : Style; keywordtype : KeywordType}
    type GraphicString   = {s:string; foreground : RGBA; background : RGBA; style : Style}
    module Highlighting =
        type SyntaxHighlighter = 
            {update : string -> (KeywordType * string)list}
            static member OfString(s:string) =
                //let s = regex(s,"[\r\n]","").Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
                let rec parse = function
                    |[],c -> c
                    |a::b,c -> 
                        //get the type
                        let x,y = (a:string).IndexOf '@', a.IndexOfAny([|'@';' '|],1)
                        let i = a.IndexOf('@',1)
                        //get the value
                        let z = regex(a.[i+1..])
                        match a.[x+1..y-1].ToLower() with
                        |"majorkeyword" ->  parse(b,(MajorKeyword, z) :: c)
                        |"minorkeyword" ->  parse(b,(MinorKeyword, z) :: c)
                        |"literal" ->       parse(b,(Literal, z) :: c)
                        |"literalstring" -> parse(b,(LiteralString, z) :: c)
                        |"comment" ->       parse(b,(Comment, z) :: c)
                        |s -> raise (System.ArgumentException(s))
                let t = parse(s.Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray,[]) |> Array.ofList
                let c = Array.Parallel.choose (function |Comment, r -> Some r |_ -> None) t
                let l = Array.Parallel.choose (function |Literal, r -> Some r |_ -> None) t
                let r = Array.Parallel.choose (function |Comment,_ |Literal, _ -> None |a,b -> Some(a,b)) t
                let dematch(a:Match) = a.Index,a.Length
                {update = fun str -> 
                    let s,pass1 = 
                        Array.fold (fun (s:string,stuff:(KeywordType*(int*int)) list) (r:Regex) -> 
                            r.Matches(s) 
                            |> Seq.cast<Match>
                            |> Seq.fold(fun (acc:string,stuff:(KeywordType*_) list) (r:Match) ->
                                let start = r.Index-1
                                let stop = r.Index+r.Length
                                (
                                    (if start < 0 then "" else acc.[..r.Index-1]) +
                                    String.replicate (r.Length-1) "\u0002" +
                                    "\u0001" +
                                    (if stop >= acc.Length then "" else acc.[stop..])
                                ),(Comment,dematch r)::stuff) (s,stuff)
                            ) (str,[]) c
                    let s,pass2 =
                        Array.fold (fun (s:string,stuff:(KeywordType*(int*int)) list) (r:Regex) -> 
                            r.Matches(s) 
                            |> Seq.cast<Match>
                            |> Seq.fold(fun (acc:string,stuff:(KeywordType*_) list) (r:Match) ->
                                let start = r.Index-1
                                let stop = r.Index+r.Length
                                (
                                    (if start < 0 then "" else acc.[..r.Index-1]) +
                                    String.replicate (r.Length-1) "\u0002" +
                                    "\u0001" +
                                    (if stop >= acc.Length then "" else acc.[stop+0..])
                                ),(Comment,dematch r)::stuff) (s,stuff) 
                            ) (s,pass1) c
                    let s,pass3 = 
                        Array.fold (fun (s:string,stuff:(KeywordType*(int*int)) list) (kt,r:Regex) -> 
                            r.Matches(s) 
                            |> Seq.cast<Match>
                            |> Seq.fold(fun (acc:string,stuff:(KeywordType*(int*int)) list) (r:Match) ->
                                let start = r.Index-1
                                let stop = r.Index+r.Length
                                (
                                    (if start < 0 then "" else acc.[..r.Index-1]) +
                                    String.replicate (r.Length-1) "\u0002" +
                                    "\u0001" +
                                    (if stop >= acc.Length then "" else acc.[stop..])
                                ),(kt,dematch r)::stuff) (s,stuff)
                            ) (s,pass2) r
                    let pass4 = 
                        let rec inner = function
                            |i,n when i = s.Length -> n
                            |i,n when i = s.Length - 1 -> if s.[i] = '\u0001' then n else (Normal,(i,1))::n
                            |i,n ->
                                if s.[i] = '\u0001' then inner(i+1,n)
                                elif s.[i] = '\u0002' then let j = s.IndexOf('\u0001',i) in inner(j+1,n)
                                else 
                                    let j = s.IndexOfAny([|'\u0001';'\u0002'|],i)
                                    if j = -1 then (Normal,(i,s.Length-i))::n
                                    else let s' = s.[i..j-1] in inner(j,(Normal,(i,s'.Length))::n)
                               
                        inner(0,[])
                    List.map(fun (kt,(i,l)) -> i,(kt,str.[i..i+l-1])) (pass3@pass4)
                    |> List.sortBy fst
                    |> List.map snd
                }
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
    type IPC(local:IPEndPoint,remote:IPEndPoint) = 
        let socket = new UdpClient(local)
        do socket.Connect(remote)
        member x.Bind(ep:IPEndPoint) = socket.Client.Bind ep
        member x.LocalEndpoint = socket.Client.LocalEndPoint :?> IPEndPoint
        member x.RemoteEndpoint= socket.Client.RemoteEndPoint:?> IPEndPoint
        member x.Send(p:Packet) = let b = Packet.ToByteArray p in socket.Send(b,b.Length) |> ignore
        member x.Receive() = 
            //spinuntil(fun () -> socket.Available > 0) //poll the socket
            let v = ref remote
            socket.Receive(v)
            |> Packet.OfByteArray
module Tree = 
    type LabelledTree<'Label,'a when 'Label : equality> = 
        |Branch of 'Label * (LabelledTree<'Label,'a> list )
        |Leaf of 'Label * 'a
        static member GetLabel(x) = match x with |Branch(l,_)|Leaf(l,_) -> l
        member x.Item i = 
            match x with
            |Branch(s,l) -> List.find(LabelledTree<'Label,'a>.GetLabel >> ((=) i)) l
            |_           -> failwith "Not a branch"
module Config =
    open Tree
    type Config = 
        {
            conf : LabelledTree<string,string> list
        }
        static member OfString(s) =
            //recursive regex
            let rec inner s = 
                let branches = 
                    regex(@"\{\((.*)\)(.*)\}").Matches(s)//\((.*)\)(.*)\
                    |> Seq.cast<Match> 
                    |> Seq.map (fun i -> let j = i |> getgroups |> Array.ofSeq in Branch(j.[0],inner j.[1])) 
                    |> List.ofSeq
                let leaves = 
                    regex(@"@(.*)@(.*);").Matches(s) 
                    |> Seq.cast<Match> 
                    |> Seq.map (fun i -> let j = i|> getgroups |> Array.ofSeq in Leaf(j.[0],j.[1]))
                    |> List.ofSeq
                leaves @ branches |> side (printfn "%A")
            {conf = inner s}
        override x.ToString() =
            let rec inner i j = 
                let v = String.replicate i "\t"
                List.map (function 
                    |Branch(l,t) -> String.concat "" [|v; "{("; l; ")\n"; inner (i+1) t; "\n"; v; "}"|]
                    |Leaf(a,b) -> v + "@" + a + "@" + b + ";"
                ) j
                |> String.concat "\n"
            inner 0 x.conf