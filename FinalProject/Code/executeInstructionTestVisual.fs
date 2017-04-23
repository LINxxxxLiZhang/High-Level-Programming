namespace HLP

open Expecto
open common
open executeInstruction
open testingcommon
open VisualInterface.Program

module executeInstructionTestVisual =

// -------------------------------------------------------------------------------------
// ----parameters that can be adjusted to increase the complexity of test --------------
// -------------------------------------------------------------------------------------
    let testArraySize = 1000        // the number of instructions in each test
    let testLoopLst = [10;30;50]    // 10 would initialise register states with very small numbers, most of which are within [-256, 255]
                                    // 30 would initialise register states with numbers that are slightly larger
                                    // 50 would initialise register states with extremely large numbers
    let setFlag = true              // set flag if true (note that shift is tested as a default setting)
    let verbose = true              // show the machinestates after each instruction in commandline
    let maxTest = 10                 

// -------------------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------ExecuteInstruction Test----------------------------------------------------------------
//-----------------------------------------------ExecuteInstruction Test----------------------------------------------------------------
    /// exhaustive test of single instructions
    let TestExecuteComplex (arr:TestType []) (inst2:string) (setFlagAndCond:bool) (n:int) :(Instruction *(int array)*Flag) list =
        let createInst=
            (fun (i,(datatype:TestDataType,cond:Conditional,f:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,s:TestShift,rand:RandNum,idxing:Indexing)) ->
                match i % n with
                | x when x>=0 && x <= n/3 -> (i,MOV(NoCond,f,r1.convt,rand.convt,Shift.NoShift))
                | x when x>n/3 && x<=2*n/3 -> (i,MOV(NoCond,f,r1.convt,r2.convt,s.convt))
                | _ -> 
                    let shift = 
                        if r3 = R1 || r3 = R2 || r3 = R3 || r3 = R4 then s
                        else NoShift
                    match inst2 with 
                    // ----------------------------------  instructions with 1 operand -------------------------------------------
                    // ----------------------------------  instructions with 1 operand -------------------------------------------
                    // ----- have shift option --------
                    |"AND" ->
                        match setFlagAndCond with
                        |true -> i,AND(cond, f, r1.convt, r2.convt, r3.convt, shift.convt)
                        |false-> i,AND(NoCond, NoFlag, r1.convt, r2.convt, r3.convt, shift.convt)
                    |"ORR" ->
                        match setFlagAndCond with
                        |true -> i,ORR(cond, f, r1.convt, r2.convt, r3.convt, shift.convt)
                        |false-> i,ORR(NoCond, NoFlag, r1.convt, r2.convt, r3.convt, shift.convt)
                    |"EOR" ->
                        match setFlagAndCond with
                        |true -> i,EOR(cond, f, r1.convt, r2.convt, r3.convt, shift.convt)
                        |false-> i,EOR(NoCond, NoFlag, r1.convt, r2.convt, r3.convt, shift.convt)
                    |"BIC" ->
                        match setFlagAndCond with
                        |true -> i,BIC(cond, f, r1.convt, r2.convt, r3.convt, shift.convt)
                        |false-> i,BIC(NoCond, NoFlag, r1.convt, r2.convt, r3.convt, shift.convt)
                    |"MOV" -> 
                        match setFlagAndCond with
                        |true -> i,MOV(cond, f, r1.convt, r3.convt, shift.convt)
                        |false-> i,MOV(NoCond, NoFlag, r1.convt, r3.convt, shift.convt)
                    |"MVN" -> 
                        match setFlagAndCond with
                        |true -> i,MVN(cond, f, r1.convt, r3.convt, shift.convt)
                        |false-> i,MVN(NoCond, NoFlag, r1.convt, r3.convt, shift.convt)
                    |"CMP" ->
                        match setFlagAndCond with
                        |true -> i,CMP(cond, r1.convt, r3.convt, shift.convt)
                        |false-> i,CMP(NoCond, r1.convt, r3.convt, shift.convt)
                    |"CMN" ->
                        match setFlagAndCond with
                        |true -> i,CMN(cond, r1.convt, r3.convt, shift.convt)
                        |false-> i,CMN(NoCond, r1.convt, r3.convt, shift.convt)
                    |"TST" ->
                        match setFlagAndCond with
                        |true -> i,TST(cond, r1.convt, r3.convt, shift.convt)
                        |false-> i,TST(NoCond, r1.convt, r3.convt, shift.convt)
                    |"TEQ" ->
                        match setFlagAndCond with
                        |true -> i,TEQ(cond, r1.convt, r3.convt, shift.convt)
                        |false-> i,TEQ(NoCond, r1.convt, r3.convt, shift.convt)

                    // ----- doesn't have shift option --------
                    |"RRX" -> 
                        match setFlagAndCond with
                        |true -> i,RRXInst(cond, f, r1.convt, r2.convt)
                        |false-> i,RRXInst(NoCond, NoFlag, r1.convt, r2.convt)
            
                    // ----------------------------------  instructions with 2 operand -------------------------------------------
                    // ----------------------------------  instructions with 2 operands -------------------------------------------
                    // ----- have shift option --------
                    |"SUB" -> 
                        match setFlagAndCond with
                        |true -> i,SUB(cond, f, r1.convt, r2.convt, r3.convt, shift.convt)
                        |false-> i,SUB(NoCond, NoFlag, r1.convt, r2.convt, r3.convt, shift.convt)
                    |"SBC" -> 
                        match setFlagAndCond with
                        |true -> i,SBC(cond, f, r1.convt, r2.convt, r3.convt, shift.convt)
                        |false-> i,SBC(NoCond, NoFlag, r1.convt, r2.convt, r3.convt, shift.convt)
                    |"RSB" -> 
                        match setFlagAndCond with
                        |true -> i,RSB(cond, f, r1.convt, r2.convt, r3.convt, shift.convt)
                        |false-> i,RSB(NoCond, NoFlag, r1.convt, r2.convt, r3.convt, shift.convt)
                    |"RSC" ->
                        match setFlagAndCond with
                        |true -> i,RSC(cond, f, r1.convt, r2.convt, r3.convt, shift.convt)
                        |false-> i,RSC(NoCond, NoFlag, r1.convt, r2.convt, r3.convt, shift.convt)
                    |"ADD" -> 
                        match setFlagAndCond with
                        |true -> i,ADD(cond, f, r1.convt, r2.convt, r3.convt, shift.convt)
                        |false-> i,ADD(NoCond, NoFlag, r1.convt, r2.convt, r3.convt, shift.convt)
                    |"ADC" -> 
                        match setFlagAndCond with
                        |true -> i,ADC(cond, f, r1.convt, r2.convt, r3.convt, shift.convt)
                        |false-> i,ADC(NoCond, NoFlag, r1.convt, r2.convt, r3.convt, shift.convt)
                    
                    // ----- doesn't have shift option --------
                    |"LSL" -> 
                        match setFlagAndCond with
                        |true -> i,LSLInst(cond, f, r1.convt, r2.convt, r3.convt)
                        |false-> i,LSLInst(NoCond, NoFlag, r1.convt, r2.convt, r3.convt)
                    |"LSR" -> 
                        match setFlagAndCond with
                        |true -> i,LSRInst(cond, f, r1.convt, r2.convt, r3.convt)
                        |false-> i,LSRInst(NoCond, NoFlag, r1.convt, r2.convt, r3.convt)
                    |"ASR" -> 
                        match setFlagAndCond with
                        |true -> i,ASRInst(cond, f, r1.convt, r2.convt, r3.convt)
                        |false-> i,ASRInst(NoCond, NoFlag, r1.convt, r2.convt, r3.convt)
                    |"ROR" -> 
                        match setFlagAndCond with
                        |true -> i,RORInst(cond, f, r1.convt, r2.convt, r3.convt)
                        |false-> i,RORInst(NoCond, NoFlag, r1.convt, r2.convt, r3.convt)
                    |_ -> failwith "not implemented yet"
            )
        /// get instruction map
        let insMap = 
            let arrNew = arr |> Array.indexed
            let arrInst = 
                arrNew
                |> Array.map createInst
                |> Array.filter (fun (_,inst) -> inst <> ParseError)
                |> Array.map snd
            arrInst
            |> Array.indexed
            |> Map.ofArray

        /// get a list of machinestate after each step of execution
        let allMS = getRecord insMap 
        allMS 
        |> List.indexed
        |> List.map (fun (n, (regs,_,_,flag,errormsg)) -> (insMap|>Map.find n), regs.[1..4], flag)

    /// testing memory instructions in executeInstruction
    let TestExecuteMEM (inst:Instruction list) =
        let insMap =
            inst
            |> List.indexed
            |> Map.ofList
        
        let allMS = getRecord insMap
        allMS 
        |> List.indexed 
        |> List.map (fun (n, (regs,mem,_,flag,errormsg)) -> (insMap|>Map.find n), regs.[1..4], mem)

//-----------------------------------------------Visual Test----------------------------------------------------------------
//-----------------------------------------------Visual Test----------------------------------------------------------------
//-----------------------------------------------Visual Test----------------------------------------------------------------
    /// testing memory instructions in Visual
    let TestVisualMEM (inst:Instruction list) = 
        let instLst = 
            inst
            |> List.map 
                (fun x-> 
                    match x with
                    |MOV(cond, _, dest, op1, _) -> "MOV" + " " + dest.toStr + "," + op1.toStr + "\n" 
                    |STR(datatype,cond,dest,op1,op2,shift,idxing) -> 
                        match idxing, op2, shift with
                        |Immediate, NoReg, _ -> "STR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "] \n" 
                        
                        |Pre , Reg(_), Shift.NoShift -> "STR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr + "]! \n" 
                        |Post , Reg(_), Shift.NoShift-> "STR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "]" + "," + op2.toStr + "\n" 
                        |Immediate , Reg(_), Shift.NoShift -> "STR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr + "] \n" 
                        
                        |Pre , Reg(_) , _ -> "STR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr + "," + shift.toStr + "]! \n" 
                        |Post , Reg(_) , _-> "STR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "]" + "," + op2.toStr + "," + shift.toStr + "\n" 
                        |Immediate , Reg(_), _ -> "STR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr + "," + shift.toStr + "] \n" 

                        |Pre , Value(_), _ -> "STR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr  + shift.toStr + "]! \n" 
                        |Post , Value(_), _-> "STR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "]" + "," + op2.toStr + shift.toStr + "\n" 
                        |Immediate , Value(_), _ -> "STR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr + shift.toStr + "] \n" 
                    
                        |_ -> failwith "syntax error"
                    |LDR(datatype,cond,dest,op1,op2,shift,idxing) -> 
                        match idxing, op2, shift with
                        |Immediate, NoReg, _ -> "LDR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "] \n" 
                        
                        |Pre , Reg(_), Shift.NoShift -> "LDR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr + "]! \n" 
                        |Post , Reg(_), Shift.NoShift-> "LDR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "]" + "," + op2.toStr + "\n" 
                        |Immediate , Reg(_), Shift.NoShift -> "LDR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr + "] \n"                         
                        |Pre , Reg(_) , _ -> "LDR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr + "," + shift.toStr + "]! \n" 
                        |Post , Reg(_) , _-> "LDR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "]" + "," + op2.toStr + "," + shift.toStr + "\n" 
                        |Immediate , Reg(_), _ -> "LDR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr + "," + shift.toStr + "] \n"

                        |Pre , Value(_) ,_-> "LDR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr  + shift.toStr + "]! \n"
                        |Post , Value(_), _-> "LDR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "]" + "," + op2.toStr + shift.toStr + "\n" 
                        |Immediate , Value(_), _ -> "LDR" + datatype.toStr + cond.toStr + " " + dest.toStr  + ", " + "[" + op1.toStr + "," + op2.toStr + shift.toStr + "] \n" 
                
                        |_ -> failwith "syntax error"
                    |STM(cond,stacktype,spUpdate,pointer,regLst) ->
                        let regLstStr = 
                            [regLst.Head.toStr] 
                            |> List.append (regLst |> List.tail |> List.map (fun x -> x.toStr + ","))
                            |> List.reduce (+)
                        "STM" + stacktype.toStr + cond.toStr + " " + pointer.toStr + spUpdate.toStr + ", " + "{" + regLstStr + "}" + "\n"
                    |LDM(cond,stacktype,spUpdate,pointer,regLst) ->
                        let regLstStr = 
                            [regLst.Head.toStr] 
                            |> List.append (regLst |> List.tail |> List.map (fun x -> x.toStr + ","))
                            |> List.reduce (+)
                        "LDM" + stacktype.toStr + cond.toStr + " " + pointer.toStr + spUpdate.toStr + ", " + "{" + regLstStr + "}" + "\n"

                    |_ -> failwith "not implemented yet"
                )
            |> System.String.Concat
        let traceMEMLocs = [256..4..4256] //memory locations to be traced
        let mode = "MEM"
        VisualUnitTest instLst traceMEMLocs mode
        |> List.map (fun (code, regs, memory, flag) -> code, regs.[1..4], memory)

    /// exhaustively test single instructions in Visual
    let TestVisualComplex (arr:TestType []) (inst2:string) (setFlagAndCond:bool) (n:int) :(string * (int array) * Flag) list=
        let creatInst =
            (fun (i, (datatype:TestDataType, cond:Conditional,f:Setflags,dest:TestRegs,op1:TestRegs,op2:TestRegOrVal,s:TestShift, rand:RandNum,idxing:Indexing)) -> 
                match i % n with 
                | x when x>=0 && x <= n/3 -> "MOV"  + f.toStr + " " + dest.toStr + ", " + rand.toStr + " \n" + NZCVToR5
                | x when x>n/3 && x <= 2*n/3 -> "MOV"  + f.toStr + " " + dest.toStr + ", " + op1.toStr + s.toStr + " \n" + NZCVToR5
                | _ ->
                    let shift = 
                        if op2 = R1 || op2 = R2 || op2 = R3 || op2 = R4 then s
                        else NoShift
                    match inst2 with
                    // ----------------------------------------------  instructions with 2 operands -------------------------------------------
                    // ----------------------------------------------  instructions with 2 operands -------------------------------------------
                    // does not allow shifting
                    |"LSL" |"LSR"|"ASR"|"ROR"->
                        match setFlagAndCond with
                        |true -> inst2 + f.toStr + cond.toStr + " " + dest.toStr + ", " + op1.toStr + ", " + op2.toStr + " \n" + NZCVToR5
                        |false -> inst2 + " " + dest.toStr + ", " + op1.toStr + ", " + op2.toStr + " \n" + NZCVToR5
                    // does allow shifting
                    |"ADD" |"SUB" |"ADC" |"SBC" |"RSB" |"RSC" |"AND" |"ORR" |"EOR" |"BIC" ->
                        match setFlagAndCond with
                        |true -> inst2 + f.toStr + cond.toStr + " " + dest.toStr + ", " + op1.toStr + ", " + op2.toStr + shift.toStr + " \n" + NZCVToR5
                        |false -> inst2 + " " + dest.toStr + ", " + op1.toStr + ", " + op2.toStr + shift.toStr + " \n" + NZCVToR5
                    // ----------------------------------------------  instructions with 1 operand -------------------------------------------
                    // ----------------------------------------------  instructions with 1 operand -------------------------------------------
                    // operand 1 can be a reg or value
                    |"MOV" |"MVN" -> 
                        match setFlagAndCond with
                        |true -> inst2 + f.toStr + cond.toStr + " " + dest.toStr + ", " + op2.toStr + shift.toStr + " \n" + NZCVToR5
                        |false -> inst2 + " " + dest.toStr + ", " + op2.toStr + shift.toStr + " \n" + NZCVToR5
                    // Flag is set on anyway
                    |"CMP" |"CMN" |"TST" |"TEQ" ->
                        match setFlagAndCond with
                        |true -> inst2 + cond.toStr + " " + dest.toStr + ", " + op2.toStr + shift.toStr + " \n" + NZCVToR5
                        |false -> inst2 + " " + dest.toStr + ", " + op2.toStr + shift.toStr + " \n" + NZCVToR5
                    // operand 1 must be a reg and no shift
                    |"RRX" ->
                        match setFlagAndCond with
                        |true -> inst2 + f.toStr + cond.toStr + " " + dest.toStr + ", " + op1.toStr + " \n" + NZCVToR5
                        |false -> inst2 + " " + dest.toStr + ", " + op1.toStr + " \n" + NZCVToR5
                    | _ -> failwith "not implemented yet"
            )
        let instLst = 
            arr 
            |> Array.indexed
            |> Array.map creatInst
            |> System.String.Concat
        VisualUnitTest instLst [] " "
        |> List.map (fun (code, r, mem, f) -> code, r.[1..4], (f |> ConvVisualFlags))


// --------------------------------------------------------------------------------------------------------------------------
//--------------------------------------------------test data generator---------------------------------------------------
//--------------------------------------------------test data generator---------------------------------------------------
//--------------------------------------------------test data generator---------------------------------------------------
// --------------------------------------------------------------------------------------------------------------------------
    

    
        
    
    let fsConfig = { FsCheck.Config.Default with MaxTest = maxTest ; QuietOnSuccess=false} // adjust number of tests according to wishes

    /// generate an array of size n
    let testWithSizedArray n = 
        let fixedArrayGen = FsCheck.GenExtensions.ArrayOf(FsCheck.Arb.generate, n)
        FsCheck.Prop.forAll (FsCheck.Arb.fromGen fixedArrayGen)

    let ComplexTest n pName arrayFun = 
        testPropertyWithConfig fsConfig pName
        <| testWithSizedArray n arrayFun


    /// Randomly initialise machine state.
    let testGenComplex visualFun exeFun inst n =
        fun (arr: TestType []) -> 
            
            // ------ results from executeInstruction ------
            let exe = exeFun arr inst setFlag n
            let exeInsts,exeRegs,exeFlags = exe |> List.unzip3
            let exeResults = exeRegs |> List.zip exeFlags
            // ------ results from VISUAL ------
            let vis = visualFun arr inst setFlag n
            let visInsts,visRegs,visFlags = vis |> List.unzip3
            let visResults = visRegs |> List.zip visFlags
            
            // test
            for j in [0 .. exeResults.Length-1] do
                let failedStateInfo i = sprintf "\n Failed instruction: \n \n Index is \n  %A,\n Instruction is \n    %A,\n expected machineState is \n%A,\n%A\n\n actual machineState is \n%A,\n%A\n\n\n\n\n\n\n\n\n" i visInsts.[i] visRegs.[i] visFlags.[i] exeRegs.[i] exeFlags.[i]
                let previousStateInfo i = sprintf "\n \n\n\n\n\n\n \n Last successful instruction: \n \n Index is \n  %A,\n Instruction is \n    %A,\n last VISUAL machineState is \n%A,\n%A\n last exeInstruction machineState is \n%A,\n%A\n " (i-1) visInsts.[i-1] visRegs.[i-1] visFlags.[i-1] exeRegs.[i-1] exeFlags.[i-1]
                let testInfo i =
                    if i=0 then failedStateInfo i
                    else previousStateInfo i + failedStateInfo i
                match verbose with
                |true -> printf "%A MachineState is %A \n" (j+1) (failedStateInfo j)
                |false -> ()
                Expect.equal exeResults.[j] visResults.[j] (testInfo j)


    /// unit testing LDR and STR
    let MEMTest name visualFun exeFun inst =
        testCase name <| fun () ->
            // ------ results from VISUAL ------
            let visInsts,visRegs,visMem = inst |> visualFun |> List.unzip3
            let visResults = visRegs |> List.zip visMem
            
            // ------ results from executeInstruction ------
            let exeInsts,exeRegs,exeMem = inst |> exeFun |> List.unzip3
            let exeResults = exeRegs |> List.zip exeMem

            for n in [0 .. exeResults.Length-1] do
                let failedStateInfo i = sprintf "\n current instruction: \n \n Index is \n  %A,\n Instruction is \n    %A,\n expected machineState is \n%A,\n%A\n\n actual machineState is \n%A,\n%A\n\n\n\n\n\n\n\n\n" i visInsts.[i] visRegs.[i] visMem.[i] exeRegs.[i] exeMem.[i]
                let previousStateInfo i = sprintf "\n \n\n\n\n\n\n \n Last successful instruction: \n \n Index is \n  %A,\n Instruction is \n    %A,\n last VISUAL machineState is \n%A,\n%A\n last exeInstruction machineState is \n%A,\n%A\n " (i-1) visInsts.[i-1] visRegs.[i-1] visMem.[i-1] exeRegs.[i-1] exeMem.[i-1]
                let testInfo i =
                    if i=0 then failedStateInfo i
                    else previousStateInfo i + failedStateInfo i
                match verbose with
                |true -> printf "%A MachineState is %A \n" (n+1) (failedStateInfo n)
                |false -> ()
                Expect.equal exeResults.[n] visResults.[n] (testInfo n)




    //----------------------------------------------------------------------
    //------- the list of memory related instructions to be tested ---------
    //----------------------------------------------------------------------
    let LDMSTMTestLst = 
        [
        //initialisation
        MOV(NoCond,NoFlag,Reg(2),Value(400),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(1),Value(412),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(3),Value(408),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(4),Value(404),Shift.NoShift)
        //STM(NoCond,IA,Update,Reg(1),[Reg(1);Reg(2);Reg(3)]) //this should fail because pointer = one of the registers in the list
        
        // test IA/IB/DA/DB
        STM(NoCond,IA,NoUpdate,Reg(1),[Reg(2);Reg(4);Reg(3)])
        STM(NoCond,IB,NoUpdate,Reg(2),[Reg(4);Reg(1);Reg(3)])
        STM(NoCond,DA,NoUpdate,Reg(3),[Reg(2);Reg(4);Reg(1)])
        STM(NoCond,DB,NoUpdate,Reg(4),[Reg(2);Reg(1);Reg(3)])

        LDM(NoCond,IA,NoUpdate,Reg(1),[Reg(2);Reg(4);Reg(3)])
        MOV(NoCond,NoFlag,Reg(1),Value(412),Shift.NoShift)
        
        LDM(NoCond,IB,NoUpdate,Reg(2),[Reg(4);Reg(1);Reg(3)])
        MOV(NoCond,NoFlag,Reg(2),Value(400),Shift.NoShift)
        
        LDM(NoCond,DA,NoUpdate,Reg(3),[Reg(2);Reg(4);Reg(1)])
        MOV(NoCond,NoFlag,Reg(3),Value(408),Shift.NoShift)
        
        LDM(NoCond,DB,NoUpdate,Reg(4),[Reg(2);Reg(1);Reg(3)])
        MOV(NoCond,NoFlag,Reg(4),Value(404),Shift.NoShift)
        // STM update pointer
        MOV(NoCond,NoFlag,Reg(2),Value(400),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(1),Value(412),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(3),Value(408),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(4),Value(404),Shift.NoShift)

        STM(NoCond,IB,Update,Reg(1),[Reg(2);Reg(4);Reg(3)])
        MOV(NoCond,NoFlag,Reg(1),Value(400),Shift.NoShift)

        STM(NoCond,IA,Update,Reg(2),[Reg(4);Reg(1);Reg(3)])
        MOV(NoCond,NoFlag,Reg(2),Value(408),Shift.NoShift)

        STM(NoCond,DB,Update,Reg(4),[Reg(2);Reg(1);Reg(3)])
        MOV(NoCond,NoFlag,Reg(4),Value(404),Shift.NoShift)

        STM(NoCond,DA,Update,Reg(3),[Reg(2);Reg(4);Reg(1)])
        MOV(NoCond,NoFlag,Reg(3),Value(412),Shift.NoShift)

        // LDM update pointer
        MOV(NoCond,NoFlag,Reg(2),Value(400),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(1),Value(412),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(3),Value(408),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(4),Value(404),Shift.NoShift)

        LDM(NoCond,IB,Update,Reg(1),[Reg(2);Reg(4);Reg(3)])
        MOV(NoCond,NoFlag,Reg(1),Value(400),Shift.NoShift)

        LDM(NoCond,IA,Update,Reg(2),[Reg(4);Reg(1);Reg(3)])
        MOV(NoCond,NoFlag,Reg(2),Value(408),Shift.NoShift)

        LDM(NoCond,DB,Update,Reg(4),[Reg(2);Reg(1);Reg(3)])
        MOV(NoCond,NoFlag,Reg(4),Value(404),Shift.NoShift)

        LDM(NoCond,DA,Update,Reg(3),[Reg(2);Reg(4);Reg(1)])
        MOV(NoCond,NoFlag,Reg(3),Value(412),Shift.NoShift)
        ]

    let LDRSTRTestLst = 
        [
        //initialisation
        MOV(NoCond,NoFlag,Reg(2),Value(300),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(1),Value(324),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(3),Value(500),Shift.NoShift)
        // ------------- immediate offset STR/LDR --------------------
        // STR test immediate offset
        STR(DataType.By, NoCond,Reg(2),Reg(1),NoReg,Shift.NoShift,Immediate)
        STR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.NoShift,Immediate)
        // STR test immediate offset with logic shift
        STR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.LSL(Value(1)),Immediate)

        // LDR test immediate offset
        LDR(DataType.By, NoCond,Reg(2),Reg(1),NoReg,Shift.NoShift,Immediate)
        LDR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.NoShift,Immediate)
        // LDR test immediate offset with logic shift
        LDR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.LSL(Value(1)),Immediate)

        // ------------ indexed offset STR ------------------------
        //initialisation
        MOV(NoCond,NoFlag,Reg(2),Value(300),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(1),Value(324),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(3),Value(500),Shift.NoShift)
        //STR(DataType.By, NoCond,Reg(2),Reg(1),NoReg,Shift.NoShift,Pre) //Pre-indexing must have offset. This should fail
        // Pre-indexing
        STR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.NoShift,Pre)
        // with Shift
        STR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.LSL(Value(1)),Pre)
        // Post-indexing
        STR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.NoShift,Post)
        // with SHift
        STR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.LSL(Value(1)),Post)
                
        // ------------ indexed offset LDR ------------------------
        //initialisation
        MOV(NoCond,NoFlag,Reg(2),Value(300),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(1),Value(324),Shift.NoShift)
        MOV(NoCond,NoFlag,Reg(3),Value(500),Shift.NoShift)
        //STR(DataType.By, NoCond,Reg(2),Reg(1),NoReg,Shift.NoShift,Pre) //Pre-indexing must have offset. This should fail
        // Pre-indexing
        LDR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.NoShift,Pre)
        // with Shift
        LDR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.LSL(Value(1)),Pre)
        // Post-indexing
        LDR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.NoShift,Post)
        // with SHift
        LDR(DataType.By, NoCond,Reg(2),Reg(1),Reg(3),Shift.LSL(Value(1)),Post)

        ]

    //-------------------------------------------------------
    //------- the list of instructions to be tested ---------
    //-------------------------------------------------------
    let insLst =
        [
        "MOV"
        "SUB"   // ***Debugging finding*****   
                // when Rm is arithmatically logic shifted (ASR), 
                // the destination register state read from visoutput.txt is always 1 value 
                // less than that displayed in Visual APP. 
        "SBC"
        "RSB"
        "CMP"
        "CMN"
        "TST"
        "TEQ"
        "RSC"
        "ADD"
        "ADC"
        "MVN"
        "LSL"
        "LSR"
        "ROR"
        "RRX"
        "ASR"
        "AND"
        "EOR"
        "BIC"
        "ORR"
        ]



// -------------------------------------------------------------------------------------
// ---------------------------------- generating tests -----------------------------
// -------------------------------------------------------------------------------------
    let testCreateBasic inst testLoop =
        ComplexTest testArraySize (sprintf "\n%A test. \nTesting parameters: setFlag = %A, testLoop = %A, verbose = %A, maximumTest = %A, Number Of tested Instructions in one test = %A" inst setFlag testLoop verbose maxTest testArraySize)
        <| testGenComplex TestVisualComplex TestExecuteComplex inst testLoop 

    let testCreateLoops (inst:string) = 
        testLoopLst
        |> List.map (fun testLoop -> testCreateBasic inst testLoop)
        
    let testInsLst = 
        insLst
        |> List.collect (fun inst -> testCreateLoops inst ) 

    let memoryTestLst =
        [
            MEMTest "LDM/STM test" TestVisualMEM TestExecuteMEM LDMSTMTestLst
            MEMTest "LDR/STR test" TestVisualMEM TestExecuteMEM LDRSTRTestLst
        ]
    let allTestLst = testInsLst @ memoryTestLst
    let allTest = testList "FsCheck" allTestLst

    let seqConfig = {Expecto.Tests.defaultConfig with verbosity = Logging.LogLevel.Info ; parallel = false ;printer = Impl.TestPrinters.defaultPrinter}
    let executeInstructionTestVisualResult = fun () -> 
        //runTests seqConfig properties
        runTests seqConfig allTest