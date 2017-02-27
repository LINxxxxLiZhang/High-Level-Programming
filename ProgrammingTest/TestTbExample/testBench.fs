namespace TestCode

open Types
module TestBench = 
    
    open Expecto


    let fsConfig = { FsCheck.Config.Default with MaxTest = 2000 } // adjust number of tests according to wishes
    
    /// the correct answer
    let expected = Model.Model.modelAnswer

    /// the current answer to test
    let actual = Answer.theAnswer
    
    

    let properties = 
        testList "FsCheck" 
            [ 
              testPropertyWithConfig fsConfig "Q1 answer" 
              <| fun n -> 
                    Expect.equal (expected.testQ1() n) (actual.testQ1() n) "Answer is wrong"

              testPropertyWithConfig fsConfig "Q2 answer" 
              <| fun (a:FsCheck.NormalFloat) (b:FsCheck.NormalFloat) -> 
                    let a' = float a
                    let b' = float b
                    if abs a' < 1e10 && abs b' < 1e10 then 
                        Expect.floatClose Accuracy.veryHigh (expected.testQ2() a' b') (actual.testQ2() a' b') "Answer is wrong"

              testPropertyWithConfig fsConfig "Q3 answer" 
              <| fun (x:int) -> 
                    let inc n= n+1
                    let squ n = n*n
                    Expect.equal (actual.testQ3() inc x) (expected.testQ3() inc x) "Answer is wrong with increment"
                    Expect.equal (actual.testQ3() squ x) (expected.testQ3() squ x) "Answer is wrong with square"

              
            ]
    
    [<EntryPoint>]
    let main argv = 
        let retCode = runTests defaultConfig properties
        System.Console.ReadKey() |> ignore // wait for key to display test results
        retCode // return test result (0 if all passed) to OS for neatness
