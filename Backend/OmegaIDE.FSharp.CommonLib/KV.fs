namespace OmegaIDE.FSharp
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open System.Text.RegularExpressions
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
module KVTree = 
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