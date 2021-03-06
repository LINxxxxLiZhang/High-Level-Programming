﻿namespace VisualInterface

    

module VisualInterface = 

    open System.IO
    open MBrace.FsPickler
    
    /// configuration data for running VisUAL
    type Params = {
        Cached: bool // If true use a cache to deliver instant results on repeat simulations
        VisualPath: string // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir: string // the directory in which both temporary files and the persistent cache file are put
        MemDataStart: int // start of VisUAL data section Memory
        MemLocs: int list // memory locations to be traced and data returned
    }



    type Out = 
        | R of int
        | Mem of int


    type RunState = 
        | RunOK
        | RunTimeErr of string
        | SyntaxErr of string

 // ------------ Lizhang Modified ------------
    type Result = 
        { 
          Codes : string
          ResOut : Map<Out, int>
          State : RunState 
        }



    [<Literal>]
    let XmlSample1 = """
        <VisUAL xmlns="http://bitbucket.org/salmanarif/visual">
            <syntax-error line="2">Invalid source.</syntax-error>
            <syntax-error line="3">Invalid source.</syntax-error>
            <line id="2">
                <code>MOV R2, #10</code>
                <register name="R0">0x0</register>
                <register name="R1">0x5</register>
                <register name="R2">0xA</register>
                <register name="R3">0x0</register>
                <register name="R4">0x0</register>
                <register name="R5">0x0</register>
                <register name="R6">0x0</register>
                <register name="R7">0x0</register>
                <register name="R8">0x0</register>
                <register name="R9">0x0</register>
                <register name="R10">0x0</register>
                <register name="R11">0x0</register>
                <register name="R12">0x0</register>
                <register name="R13">0xFF000000</register>
                <register name="R14">0x0</register>
                <register name="R15">0x8014</register>
                <word address="0x140">0x7D0</word>
                <word address="0x144">0x0</word>
                <cycles>1</cycles>
            </line>
            <line id="3">
                <code>"MOV R2, #10"</code>
                <word address="0x140">0x7D0</word>
                <cycles>2</cycles>
            </line>
            <runtime-error line="3">Self-branching instruction detected.</runtime-error>
            <runtime-error line="3">Self-branching instruction detected.</runtime-error>
        </VisUAL>
        """

    let VisualOpts = "--cycles --regs --syntax --runtime --mode:all "
    let VisualOptsMem memsize locs =
        let locshex = String.concat "," (List.map (sprintf "0x%x") locs)
        VisualOpts + (sprintf " --meminstsize:0x%x --custom:%s "  memsize locshex)
    
    type VisualLog = FSharp.Data.XmlProvider<XmlSample1>

    // ------------ Lizhang Modified ------------
    let getVisualState paras (line: VisualLog.Line) =
                    let regs = 
                        line.Registers
                        |> Array.map (fun r -> R( int  <| r.Name.[1 .. ]), int r.Value)
                        |> List.ofArray
                    let codes = line.Code.ToString()
                    let words =
                        line.Words
                        |> Array.map (fun w -> 
                                    let x = Mem ( int w.Address - paras.MemDataStart), int w.Value 
                                    //printfn "mem is %A " x
                                    x
                                    )
                        |> List.ofArray
                    { Codes = codes ; ResOut = Map.ofList (regs @ words); State = RunOK }

    // ------------ Lizhang Modified ------------
    let getVisualStateRunTimeError paras (err:VisualLog.RuntimeError) (line: VisualLog.Line) =
                    let regs = 
                        line.Registers
                        |> Array.map (fun r -> R( int  <| r.Name.[1 .. ]), int r.Value)
                        |> List.ofArray
                    let codes = line.Code.ToString()
                    let words =
                        line.Words
                        |> Array.map (fun w -> 
                                        let x = Mem ( int w.Address - paras.MemDataStart), int w.Value
                                        //printfn "failed . mem is %A" x
                                        x
                                        )
                        |> List.ofArray
                    { Codes = codes; ResOut = Map.ofList (regs @ words); State = RunTimeErr(err.Value) }

    let RunVisualBase (paras: Params) src : Result[] = 
        let visDir = paras.WorkFileDir
        let visualJarExec vp = vp + @"jre\bin\java -jar "
        Directory.CreateDirectory visDir |> ignore
        let runOpts = 
            match paras.MemLocs with 
            | [] -> VisualOpts
            | _ -> VisualOptsMem paras.MemDataStart paras.MemLocs
        let visualHeadlessExec srcFile outputFile opts = 
            (visualJarExec paras.VisualPath) + paras.VisualPath + @"content\visual_headless.jar --headless " + srcFile + " " + outputFile 
            + " " + opts
        let addQuotes s = sprintf "\"%s\"" s
        let srcF = Path.Combine(visDir, "source.s")
        File.WriteAllText(srcF, src)
        let outputF = Path.Combine(visDir, "visoutput.log")
        let cmdArgs = "/C " + (addQuotes <| visualHeadlessExec srcF outputF runOpts)
        File.WriteAllText(visDir + "comstr.txt", cmdArgs)
        try 
            let proc = System.Diagnostics.Process.Start("cmd", cmdArgs)
            proc.WaitForExit()
        with e -> printfn "%s" e.Message
        let outXml = VisualLog.Load(outputF)
        let recordError() = 
            let mess = sprintf "\n>>---SOURCE:\n%s\n-------\n\n>>>>>>>>>>>>>>>>>>>>>>\n\n%A\n----------------------\n" src outXml
            File.AppendAllText( visDir+"\\VisualErrors", mess)
        let lines = outXml.Lines
        match outXml.SyntaxErrors |> Array.toList with
        | _ :: _ -> recordError(); [|{ Codes = ""; ResOut = Map.ofList [] ; State = SyntaxErr (sprintf "Syntax Error: %A\n" outXml) }|]
        | _ -> 
            match outXml.RuntimeErrors |> Array.toList with
            | err :: _ -> 
            // ------------ Lizhang Modified ------------
                lines
                |> Array.map (getVisualStateRunTimeError paras err)
            | _ -> 
            // ------------ Lizhang Modified ------------
                lines
                |> Array.map (getVisualState paras)

               
    
    let mutable Cache: Map<string, Result[]> = Map.empty

    let BinarySerializer = FsPickler.CreateBinarySerializer()

    let InitCache visDir =
        let cacheFName = Path.Combine(visDir, "Cache")
        if File.Exists cacheFName then
            try
                Cache <- BinarySerializer.UnPickle<Map<string,Result[]>> (File.ReadAllBytes cacheFName)
            with
                | _ -> 
                    File.Delete cacheFName
    
    let SaveCache paras =
        let cacheFName = Path.Combine(paras.WorkFileDir, "Cache")
        File.WriteAllBytes( cacheFName,  BinarySerializer.Pickle Cache)

    let RunVisualCached (paras:Params) (src:string) : Result[] =
        if Cache.ContainsKey(src) then 
            Cache.[src]
        else 
            let visResults = RunVisualBase paras src
            Cache <- Cache.Add(src, visResults)
            SaveCache paras
            visResults 

    let RunVisual (paras: Params) src =
        Directory.CreateDirectory paras.WorkFileDir |> ignore
        match paras.Cached with
        | true -> RunVisualCached paras src
        | false -> RunVisualBase paras src
