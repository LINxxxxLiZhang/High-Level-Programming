// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace TestEnvt



module Program = 
    type VName = 
        | XX
        | Y
        | Z
        | ZU
    
    let vStr = 
        function 
        | XX -> "xx"
        | Y -> "y"
        | Z -> "z"
        | ZU -> "Z"
    
    type CommandSpec = 
        | Read of VName
        | Write of VName * int
        | Assign of VName * VName
    
    let vCom = 
        function 
        | Read v -> Model.Types.Read(vStr v)
        | Write(v, n) -> Model.Types.Write((vStr v), n)
        | Assign(v1, v2) -> Model.Types.Assign((vStr v1), (vStr v2))
    
    /// Generate list of string representing words for cmd
    let makeWordList cmd = 
        match cmd with
        | Read v -> 
            [ "READ"
              vStr v ]
        | Write(v, n) -> 
            [ "WRITE"
              vStr v
              n.ToString() ]
        | Assign(v1, v2) -> 
            [ "ASSIGN"
              vStr v1
              vStr v2 ]
    
    /// Generate command string for Parse from cmd
    let makeParseCmd = 
        makeWordList
        >> String.concat " "
        >> Model.Types.Parse
    
    /// Test environment envt with comlist commands
    /// A new envt is constructed, given commands
    /// results and final var values are compared
    /// with the model envt
    let runTest trans comLst evt = 
        let allNamesUsed cLst = 
            List.collect (function 
                | Write(v, n) -> [ v ]
                | Assign(v1, v2) -> [ v1 ]
                | _ -> []) cLst
        
        let readAllNames cLst = 
            cLst
            |> allNamesUsed
            |> List.map Read
        
        let runCmds comTranslate evt = 
            let e = evt()
            (comLst @ readAllNames comLst) |> List.map (comTranslate >> e)
        
        runCmds trans evt
    
    open Expecto
    open Model.Model

    let fsConfig = { FsCheck.Config.Default with MaxTest = 2000 } // adjust number of tests according to wishes
    
    /// runs commandLst using runtestFun on both testCode and modelCode and compares outputs
    /// returns false if outputs differ, also printing out the input and outputs
    let shouldBeEqual commandLst runTestFun testCode modelCode = 
        let test = runTestFun testCode
        let model = runTestFun modelCode
        Expect.sequenceEqual test model "results should match"
    
    let properties = 
        testList "FsCheck" 
            [ testPropertyWithConfig fsConfig "Commands are the same as Model" 
              <| fun cmdLst -> 
                    shouldBeEqual cmdLst (runTest vCom cmdLst) Envt.makeEnvironment Model.Model.makeEnvironment
              
              testPropertyWithConfig fsConfig "Parsing is the same as Model" 
              <| fun cmdLst -> 
                    shouldBeEqual cmdLst (runTest makeParseCmd cmdLst) Envt.makeEnvironment Model.Model.makeEnvironment 
            ]
    
    [<EntryPoint>]
    let main argv = 
        let retCode = runTests defaultConfig properties
        System.Console.ReadKey() |> ignore // wait for key to display test results
        retCode // return test result (0 if all passed) to OS for neatness
