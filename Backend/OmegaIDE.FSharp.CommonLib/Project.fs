namespace OmegaIDE.FSharp
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open System.Text.RegularExpressions
module Project = 
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