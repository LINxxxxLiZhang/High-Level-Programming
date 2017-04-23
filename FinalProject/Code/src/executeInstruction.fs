namespace HLP
open common
open executeInstructionCommon
open LizhangInstructions
open WuzhengInstructions
open TianciInstructions
module executeInstruction =

    let executeInstruction (input:Machinestate) = 

// ------------------------------------------------------- main function goes here -------------------------------------------------------------
        ///start executing instruction list cell by cell
        let execute input = 
            // get the registers, PC. memory states and instruction list
            let ((regs: int array), ms: (Map<int, int>), insLst: Map<int,Instruction>,flags,errormsg) = input 
            // check if there is any errormsg
            match errormsg with
            |Some(_) -> (regs,ms,insLst,flags,errormsg)
            |None ->
                // check if PC is divisible by 4.
                match regs.[15] % 4 <> 0 with
                |true -> 
                    let errormsg = Some (sprintf "there is no instruction with the address %A" regs.[15] )
                    (regs,ms,insLst,flags,errormsg)
                |false ->
                    let insIndex = regs.[15]/4
                    match insLst |> Map.toList |> List.length with
                    |tmp when tmp > insIndex ->match insLst.[insIndex] with
                                                | MOV(cond, setf, dest, op1, shift) -> movORmvn (cond,setf,dest,op1,shift) input "MOV"
                                                | MVN(cond, setf, dest, op1, shift) -> movORmvn (cond,setf,dest,op1,shift) input "MVN"
                                                | ADD(cond, setf, dest, op1, op2, shift) -> ARInst (cond, setf, dest, op1, op2, shift) input "ADD"
                                                | ADC(cond, setf, dest, op1, op2, shift) -> ARInst (cond, setf, dest, op1, op2, shift) input "ADC"
                                                | SUB(cond, setf, dest, op1, op2, shift) -> ARInst (cond, setf, dest, op1, op2, shift) input "SUB"
                                                | SBC(cond, setf, dest, op1, op2, shift) -> ARInst (cond, setf, dest, op1, op2, shift) input "SBC"
                                                | RSB(cond, setf, dest, op1, op2, shift) -> ARInst (cond, setf, dest, op1, op2, shift) input "RSB"
                                                | RSC(cond, setf, dest, op1, op2, shift) -> ARInst (cond, setf, dest, op1, op2, shift) input "RSC"
                                                | CMP(cond, op1, op2, shift) -> cmp (cond, op1, op2, shift) input
                                                | CMN(cond, op1, op2, shift) -> cmn (cond, op1, op2, shift) input
                                                | TST(cond, op1, op2, shift) -> tst (cond, op1, op2, shift) input
                                                | TEQ(cond, op1, op2, shift) -> teq (cond, op1, op2, shift) input
                                                | AND(cond, setf, dest, op1, op2, shift) -> bitwiseInst (cond, setf, dest, op1, op2, shift) input "AND"
                                                | ORR(cond, setf, dest, op1, op2, shift) -> bitwiseInst (cond, setf, dest, op1, op2, shift) input "ORR"
                                                | EOR(cond, setf, dest, op1, op2, shift) -> bitwiseInst (cond, setf, dest, op1, op2, shift) input "EOR"
                                                | BIC(cond, setf, dest, op1, op2, shift) -> bitwiseInst (cond, setf, dest, op1, op2, shift) input "BIC"
                                                | LSLInst(cond, setf, dest, op1, op2) -> LogicShiftInsts (cond, setf, dest, op1 ,op2) input "LSL"
                                                | LSRInst(cond, setf, dest, op1, op2) -> LogicShiftInsts (cond, setf, dest, op1 ,op2) input "LSR"
                                                | ASRInst(cond, setf, dest, op1, op2) -> LogicShiftInsts (cond, setf, dest, op1 ,op2) input "ASR"
                                                | RORInst(cond, setf, dest, op1, op2) -> LogicShiftInsts (cond, setf, dest, op1 ,op2) input "ROR"
                                                | RRXInst(cond, setf, dest, op1) -> LogicShiftRRX (cond, setf, dest, op1) input
                                                | STR(dataType,cond,source,dest,offset,shift,idxing) -> LDRorSTR (dataType,cond,source,dest,offset,shift,idxing) input "STR"
                                                | LDR(dataType, cond, dest, source, offset, shift, idxing) -> LDRorSTR (dataType,cond,dest,source,offset,shift,idxing) input "LDR"
                                                | B(cond, label) -> branch (cond,label) input "B"
                                                | BL(cond, label) -> branch (cond,label) input "BL"
                                                | STM(cond,stackType,updateP,pointer,reglst) -> LDMorSTM (cond,stackType,updateP,pointer,reglst) input "STM"
                                                | LDM(cond,stackType,updateP,pointer,reglst) -> LDMorSTM (cond,stackType,updateP,pointer,reglst) input "LDM"
                                                | END(cond) -> End cond input
                                                |_ -> 
                                                    let errormsg = Some "The given Instruction is not implemented yet"
                                                    (regs, ms, insLst, flags,errormsg)
                        |_ -> (regs,ms,insLst,flags,errormsg)
        execute input



    /// recursively execute all instructions and returns the final machine state
    let rec executeInstructionAll (input:Machinestate) = 
        let nextMS = executeInstruction input 
        let regs,mem,insMap,flags, errormsg = nextMS
        let isTheEndOfInstruction = insMap |> Map.tryFindKey (fun k v -> k = regs.[15]/4)
        match isTheEndOfInstruction, errormsg with
        |Some(_),None -> executeInstructionAll nextMS
        |_ -> nextMS


// ------------------ optimised getRecord func -----------------------
    /// interface with executeInstruction's step through function
    /// return a list of machinestates with the machinestate after first instruction being the head of the list
    let getRecord insMap =
        let regMap = Array.zeroCreate 16
        let memoryMap = Map<int,int> [] //Map.empty
        /// initial value for flags
        let initFlags:Flag = 
            {
                N = false
                Z = false
                C = false
                V = false
            }
        let initErrorMsg = None
        let input = (regMap, memoryMap, insMap, initFlags, initErrorMsg)
        let rec getRecord' inp acc =
            let (regs:int array,mem,insMap,flags,errormsg) = inp
            let isTheEndOfInstruction = insMap |> Map.tryFindKey (fun k v -> k = regs.[15]/4)
            // copy register values which are mutable
            let regs' = regs |> Array.copy
            let inp' = regs',mem,insMap,flags,errormsg
            /// next machine state
            let nxtMS = executeInstruction inp'
            match isTheEndOfInstruction, errormsg with
            |Some(_), None-> getRecord' nxtMS (acc @ [nxtMS]) // tail recursive
            |_-> acc 

        getRecord' input []