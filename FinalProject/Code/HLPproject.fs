namespace HLP

open common
open expectoparsertest
open readASM
open executeInstructionTestVisual
open executeInstruction

module HLPproject =

    [<EntryPoint>]
    let main argv =
        RASMExpectoTest()
        //executeInstructionTestResult() 
        //executeInstructionTestVisualResult() 
        System.Console.ReadKey()|>ignore
        0

