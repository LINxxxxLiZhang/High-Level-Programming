namespace TestCode



module TestBench = 
    
    open Expecto

    open Model.Types

    /// the correct answer
    let expected = Model.Model.theAnswer

    /// the current answer to test
    let actual = Answer.theAnswer

    type Years =
        | Y1
        | Y2
        | Y3
        | Y4

    type Days =
        | D1
        | D2
        | D3
        | D4

    type Temps =
        | T1
        | T2
        | T3
        | T4


    let yList = [ Y1,2000 ; Y2,2001; Y3,2004; Y4,2005]

    let dList = [ D1,1 ; D2,10; D3,12; D4,27]

    let tempList = [T1,Some 10.0; T2,Some 12.5; T3,Some 13.0; T4, None]

    let makeConv lst =
        let m = lst |> Map.ofList
        fun x -> m.[x]

    let cYear = makeConv yList

    let cDay = makeConv dList

    let cTempList = makeConv tempList
    
    let cTime (d,m,y) :Time = { day = cDay d; month=m ; year=cYear y}

    let cWD (temp, tim, stat) = { temperature= cTempList temp; time = cTime tim; station = stat}

    /// configuration for FsCheck
    let fsConfig = { FsCheck.Config.Default with MaxTest = 500 ; QuietOnSuccess=false} // adjust number of tests according to wishes
   
    /// configuration for Expecto
    let eConfig = { Expecto.Tests.defaultConfig with verbosity = Logging.LogLevel.Info ;  parallel = false; printer = Impl.TestPrinters.defaultPrinter}

    /// test function with one parameter
    /// numTests = number of tests to run
    /// f = function to test
    /// p1c = conversion function for parameter randomised by FsCheck
    let testProperty1 pName f p1c =
        testPropertyWithConfig fsConfig pName 
        <| fun p1 -> 
            Expect.equal (f actual () (p1c p1)) (f expected () (p1c p1)) "Answer is wrong"
            
    /// test function with two parameters
    /// f = function to test
    /// p1c,p2c = conversion functions for parameters randomised by FsCheck              
    let testProperty2 pName f p1c p2c =
        testPropertyWithConfig fsConfig pName 
        <| fun (p1,p2)-> 
            Expect.equal (f actual () (p1c p1) (p2c p2)) (f expected () (p1c p1) (p2c p2)) "Answer is wrong"
            
    /// test function with three parameters
    /// f = function to test
    /// p1c,p2c,p3c = conversion functions for parameters randomised by FsCheck                           
    let testProperty3 pName f p1c p2c p3c =
        testPropertyWithConfig fsConfig  pName 
        <| fun (p1,p2,p3)-> 
            Expect.equal (f actual () (p1c p1) (p2c p2) (p3c p3)) (f expected () (p1c p1) (p2c p2) (p3c p3)) "Answer is wrong"
            
    

    let testWithSizedArray<'T> n =
        let fixedArrayGen = FsCheck.GenExtensions.ArrayOf(FsCheck.Arb.generate<'T> , n)
        FsCheck.Prop.forAll (FsCheck.Arb.fromGen fixedArrayGen)
    
    /// n - lenth of array of random data
    /// arrayFun - function to test    
    let testSizedArrayProperty n pName arrayFun =
        testPropertyWithConfig {fsConfig with MaxTest=1} pName 
        <| testWithSizedArray n arrayFun

        

    let properties = 
        testList "FsCheck" 
            [   // comment out lines in this list whose tests you do not want to run for simpler test output
                testProperty1 "Q1a answer" (fun r -> r.Q1amInt) id
                testProperty2 "Q1b answer" (fun r -> r.Q1bdaysOfMonth) cYear id
                testProperty1 "Q2 answer" (fun r -> r.Q2timeToYearEnd) cTime
                testProperty1 "Q3 answer" (fun r -> r.Q3daysInYear) cYear
                testProperty3 "Q4a answer" (fun r -> r.Q4atimeOrder) id id (fun x -> fun () -> x)
                testProperty2 "Q4b answer" (fun r -> r.Q4btimeCompare) cTime cTime
                testProperty2 "Q5 answer" (fun r -> r.Q5timeDiff) cTime cTime
                testProperty1 "Q6a answer" (fun r -> r.Q6astationReadings) (List.map cWD)
                testProperty1 "Q6b answer" (fun r -> r.Q6byearReadings) (List.map cWD)
                testProperty1 "Q6c answer" (fun r -> r.Q6cmonthReadings) (List.map cWD)
                testProperty1 "Q7 answer variable sized list" (fun r -> r.Q7maxDiffByMonthYearStation) (List.map cWD) 
                (* ------ uncomment after Q7 is implemented for better testing of Q7 --------
                testSizedArrayProperty 1000  "Q7 answer long list" <| 
                    fun (arr: WeatherData[]) ->
                        let lst = List.ofArray arr
                        Expect.equal (actual.Q7maxDiffByMonthYearStation() lst) (expected.Q7maxDiffByMonthYearStation() lst)
                *)            
            ]
    
    [<EntryPoint>]
    let main argv = 
        let retCode = runTests eConfig properties
        System.Console.ReadKey() |> ignore // wait for key to display test results
        retCode // return test result (0 if all passed) to OS for neatness
