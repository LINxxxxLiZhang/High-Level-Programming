namespace VisualInterface

module Program=

    open VisualInterface
    open Expecto


    let defaultParas = {
            Cached = true                   // true if results are stored in a cache on disk and reused to speed 
                                            // up future repeat simulations
            VisualPath =  @"..\..\..\visualapp\visual\" // the directory in which the downloaded VisUAL.exe can be found
            WorkFileDir = @"..\..\..\visualWork\"      // the directory in which both temporary files and the persistent cache file are put
            MemDataStart = 0x100            // start of VisUAL data section Memory
            MemLocs = []        // memory locations to be traced and data returned
        }

    type Flags = 
        {
            FN: bool
            FZ: bool
            FC: bool
            FV: bool
        }
   
    //let defParasWithLocs locs = {defaultParas with MemLocs = locs}
    
    /// Adds postlude to assembly code to detect flags values.
    /// Returns registers (before flag detection code) * flags
    let RunVisualWithFlagsOut paras asm mode =
        let trace = VisualInterface.RunVisual paras asm
        //printfn "trace is \n\n\n\n %A \n \n \n \n" trace
        if Array.length trace < 5 then failwithf "Error: Trace \n%A\nfrom\n%s\n has length %d < 5." trace asm (Array.length trace)
        
        /// get current instruction
        let getCode idx = trace.[idx].Codes
        /// get registers
        let getRegs idx = 
            [0..15] 
            |> List.map (fun n -> trace.[idx].ResOut.[R n]) // get reg values
            |> List.toArray 

        //The machineState of after each instruction is executed
        let getMachineStateWithMem = 
            
                [0..trace.Length-1]
                |> List.map 
                    (fun idx -> 
                        let code = getCode idx
                        // get memory whose content is not zero
                        let nonZeroMemory = 
                            paras.MemLocs
                            |> List.map (fun addr -> addr , trace.[idx].ResOut.[Mem (addr-paras.MemDataStart)])
                            |> List.filter (fun (addr, v) -> v<>0)
                            |> Map.ofList
                        let regs = getRegs idx
                        code,regs,nonZeroMemory,{ 
                                FN = false
                                FZ = false
                                FC = false
                                FV = false
                             } 
                    )
        let getMachineStateWithoutMem =
            [1..(trace.Length/6)]
            |> List.map 
                (fun idx -> 
                    let idx' = (idx-1)*6
                    let code = trace.[idx'].Codes
                    let memory = Map.empty 
                    let regs = getRegs idx'
                    let flagsInt = trace.[idx*6-1].ResOut.[R 5] //Postlude code sets R5(3:0) equal to NZCV
                    let flagBool n = (flagsInt &&& (1 <<< n)) > 0
                    code,regs,memory,{ 
                            FN = flagBool 3
                            FZ = flagBool 2
                            FC = flagBool 1
                            FV = flagBool 0
                            } 
                )
        match mode with
            |"MEM" -> getMachineStateWithMem
            |_ -> getMachineStateWithoutMem
        

    /// Run Visual with specified source code and list of memory locations to trace
    /// src - source code
    /// memLocs - list of memory locations to trace
    let RunVisualWithFlagsOutLocs memLocs src mode =
        let defaultParas = {
            Cached = true                  // true if results are stored in a cache on disk and reused to speed 
                                            // up future repeat simulations
            VisualPath =  @"..\..\..\HighLevelProgrammingFinalProject\visualInterface\visual\visualapp\visual\" // the directory in which the downloaded VisUAL.exe can be found
            WorkFileDir = @"..\..\..\HighLevelProgrammingFinalProject\visualInterface\visual\visualWork\"      // the directory in which both temporary files and the persistent cache file are put
            MemDataStart = 0x100            // start of VisUAL data section Memory
            MemLocs = []        // memory locations to be traced and data returned
        }
        RunVisualWithFlagsOut {defaultParas with MemLocs = memLocs} src mode

    
    
    let VisualUnitTest src memLocs mode :(string* int array*Map<int,int>*(bool*bool*bool*bool)) list = 
        ///convert flags to tuple for testing
        let flagsToTuple (f:Flags) = f.FN, f.FZ, f.FC, f.FV

        RunVisualWithFlagsOutLocs memLocs src mode
        |> List.map (fun (code,r,mem,f) -> code, r, mem, f|>flagsToTuple)