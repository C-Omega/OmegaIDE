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
module KVFile =
    type KVFile = 
        {nodes : (string * string) list}
        override x.ToString() = List.fold(fun acc (key,value) -> acc+"@"+key+"@"+value+";\n") "" x.nodes
        member x.Item(i) = List.pick(fun (a,b) -> if i = a then Some b else None) x.nodes
        static member OfString (s:string) = 
            {
                nodes = 
                    regex(@"@([^@]*)@([^;]*);").Matches(s) 
                    |> Seq.cast<Match> 
                    |> Seq.map (getgroups >> Array.ofSeq>>function |[|i;j|] -> i,j|_ -> failwith "bad match")
                    |> List.ofSeq
            }
module Threading =
    open System.Threading
    open System.Threading
    open System.Threading.Tasks
    open System.Diagnostics

module ProjectFile = 
    open KVFile
    type ProjectFileNode = 
        {location : string; language : string; platforms : string[]; compilemode:string}//checksum : byte[]}
        override x.ToString() = x.location+":"+x.language+"?"+String.concat "," x.platforms+"!"+x.compilemode //+"?"+(tobase64 x.checksum)
        //static member Checksum x = 
            //{location = x.location; language = x.language; checksum = "@"+x.location+":"+x.language |> ofstring |> checksum}
        static member OfString (s:string) = 
            let m = regex(@"^(.*):(.*)\?(.*)!(.*)$").Match(s) |> getgroups |> Array.ofSeq
            {
                location = m.[0];
                language = m.[1];
                platforms = m.[2].Split([|','|],System.StringSplitOptions.RemoveEmptyEntries);
                compilemode = m.[3];
            }
    type ProjectFile = 
        {
            name      : string
            output    : string
            nodes     : ProjectFileNode list
            buildmode : string
        }
        override x.ToString() = 
            "@name@"+x.name+";\n"+
            "@output@"+x.output+";\n"+
            "@buildmode@"+x.buildmode+";\n"+
            (List.fold (fun acc elem -> acc + "\n@node@" + string elem + ";") "" x.nodes)
        static member OfKVFile (s:KVFile) = 
            //this removes any form of newline (CRLF or LF), and elimits it by semicolons
            //let s = System.Text.RegularExpressions.Regex.Replace(s,"[\r\n]","").Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            let v = s.nodes//regex(@"@(.*)@(.*);").Matches(s) |> Seq.cast<Match> |> Seq.map (getgroups >> Array.ofSeq) |> List.ofSeq
            //a recursive parser
            let rec parse = function
                |[]      ,name,nodes,output,buildmode -> name,nodes,output,buildmode
                |(x,y)::b,name,nodes,output,buildmode -> 
                    //get the type and the value
                    match x with
                    |"name"      -> parse(b,y,nodes,output,buildmode)
                    |"node"      -> parse(b,name,ProjectFileNode.OfString y :: nodes,output,buildmode)
                    |"output"    -> parse(b,name,nodes,y,buildmode)
                    |"buildmode" -> parse(b,name,nodes,output,y)
                    |s -> raise (System.ArgumentException(s))
            let name,nodes,output,buildmode = parse (v,"",[],"","")
            {name = name; nodes = nodes; output = output; buildmode = buildmode}
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
        |Type
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
                        let x = (a:string).IndexOf '@'
                        let y = a.IndexOf('@',x+1)
                        printfn "%A" (x,y)
                        //get the value
                        let z = regex(a.[y+1..])
                        match a.[x+1..y-1].ToLower() with
                        |"majorkeyword" ->  parse(b,(MajorKeyword, z) :: c)
                        |"minorkeyword" ->  parse(b,(MinorKeyword, z) :: c)
                        |"literal" ->       parse(b,(Literal, z) :: c)
                        |"literalstring" -> parse(b,(LiteralString, z) :: c)
                        |"comment" ->       parse(b,(Comment, z) :: c)
                        |"type"          -> parse(b,(Type, z) :: c)
                        |""              -> parse(b,c)
                        |s -> raise (System.ArgumentException(s))
                let t = parse(s.Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray,[]) |> Array.ofList
                let c = Array.Parallel.choose (function |Comment, r -> Some r |_ -> None) t
                let l = Array.Parallel.choose (function |LiteralString, r -> Some r |_ -> None) t
                let r = Array.Parallel.choose (function |Comment,_ -> None |a,b -> Some(a,b)) t
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
                                    (if start < 0 then "" else acc.[..start]) +
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
module Tree = 
    type LabelledTree<'Label,'a when 'Label : equality> = 
        |Branch of 'Label * (LabelledTree<'Label,'a> list )
        |Leaf of 'Label * 'a
        static member GetLabel(x) = match x with |Branch(l,_)|Leaf(l,_) -> l
        member x.Value = 
            match x with
            |Leaf(_,v) -> v
            |_         -> failwith "Not a leaf"
        member x.Item i = 
            match x with
            |Branch(s,l) -> List.find(LabelledTree<'Label,'a>.GetLabel >> ((=) i)) l
            |_           -> failwith "Not a branch"
    type KVTree = 
        |Branch of label:string * (KVTree list)
        |Root   of KVTree list
        |Leaf of label:string * value:string
        static member GetLabel(x) = match x with |Branch(l,_)|Leaf(l,_) -> l|_ ->failwith "No label"
        member x.Value = 
            match x with
            |Leaf(_,v) -> v
            |_         -> failwith "Not a leaf"
        member x.Item i =
                match x with
                |Branch(_,l) 
                |Root     l  -> List.find(KVTree.GetLabel >> ((=) i)) l
                |_           -> failwith "Not a branch"
        override x.ToString() =
            let rec inner i j = 
                let v = String.replicate i "\t"
                List.map (function 
                    |Branch(l,t) -> String.concat "" [|v; "{;"; l; ";\n"; inner (i+1) t; "\n"; v; "};"|]
                    |Root(l)     -> inner i l
                    |Leaf(a,b) -> v + "@" + a + "@" + b + ";"
                ) j
                |> String.concat "\n"
            inner 0 (match x with |Branch(_,l)|Root l -> l |i -> [i])
        static member OfString(s:string) =
            //recursive regex
            let rec inner : string list * _ list -> string list * _ list = function
                |[],acc -> [],acc
                |a::n::l,acc when a.Contains("{") -> let a,b = inner(l,[]) in inner(a,Branch(n,b)::acc)
                |a::l,acc when a.Contains("}")-> l,(List.rev acc)
                |v::l,acc -> let j = regex(@"@(.*)@(.*)").Match(v)|> getgroups |> Array.ofSeq in inner(l,Leaf(j.[0],j.[1])::acc)
            Root(inner((regexremove @"\n|\r|\t|    " s).Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries)|>List.ofArray,[]) |> snd)
        static member ToKVFile x = 
            match x with 
            |Branch(_,l)
            |Root(l) -> {KVFile.KVFile.nodes=List.choose(function |Leaf(l,v) -> Some(l,v)|_ -> None) l}
            |Leaf(k,v) -> {KVFile.KVFile.nodes=[k,v]}
        static member OfKVFile (y:KVFile.KVFile) = Root(y.nodes|>List.map Leaf)
        static member RootToBranch k x = match x with |Root l -> Branch(k,l)|>Some |_ -> None
        static member (+) (x, y) =
            match x,y with 
            |Root(v)    ,Branch(_,v')
            |Root(v)    ,Root     v'  -> Root  (  v@v')
            |Branch(l,v),Branch(_,v')   
            |Branch(l,v),Root     v'  -> Branch(l,v@v')
            |Leaf _ as a,(Leaf (_,_) as b) -> Root([a;b])
            |Root(v)    ,(Leaf (_,_) as b) -> Root(b::v)
            |Branch(l,v),(Leaf (_,_) as b) -> Branch(l,b::v)
            |a          ,b                 -> failwithf "Cannot add %A and %A" a b
module Config =
    open Tree
    type Config = 
        {
            conf : KVTree list
        }
        member x.Item i = List.find(function |Branch(l,_) when l = i -> true |_ -> false) x.conf
        override x.ToString() =
            let rec inner i j = 
                let v = String.replicate i "\t"
                List.map (function 
                    |Root   l    -> List.fold(fun acc elem -> acc+"\n"+string elem) "" l
                    |Branch(l,t) -> String.concat "" [|v; "{;"; l; ";\n"; inner (i+1) t; "\n"; v; "};"|]
                    |Leaf  (a,b) -> v + "@" + a + "@" + b + ";"
                ) j
                |> String.concat "\n"
            inner 0 x.conf
        static member OfString(s:string) =
            //recursive regex
            let rec inner : string list * _ list -> string list * _ list = function
                |[],acc -> [],acc
                |a::n::l,acc when a.Contains("{") -> let a,b = inner(l,[]) in inner(a,Branch(n,b)::acc)
                |a::l,acc when a.Contains("}")-> l,(List.rev acc)
                |v::l,acc -> let j = regex(@"@(.*)@(.*)").Match(v)|> getgroups |> Array.ofSeq in inner(l,Leaf(j.[0],j.[1])::acc)
            {conf = inner((regexremove @"\n|\r|\t|    " s).Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries)|>List.ofArray,[]) |> snd}
                (* 
                let branches = 
                    regex(@"\{\((.* )\)(.* )\}").Matches(s)//\((.* )\)(.* )\
                    |> Seq.cast<Match> 
                    |> Seq.map (fun i -> let j = i |> getgroups |> Array.ofSeq in Branch(j.[0],inner j.[1])) 
                    |> List.ofSeq
                let leaves = 
                    regex(@"@(.* )@(.* );").Matches(s) 
                    |> Seq.cast<Match> 
                    |> Seq.map (fun i -> let j = i|> getgroups |> Array.ofSeq in Leaf(j.[0],j.[1]))
                    |> List.ofSeq
                leaves @ branches |> side (printfn "%A")
            {conf = inner s}
        o*)
module Compilers =
    open KVFile
    open IPC
    open ProjectFile
    open Tree
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