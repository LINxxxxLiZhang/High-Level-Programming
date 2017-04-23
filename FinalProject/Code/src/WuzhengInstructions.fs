namespace HLP
open common
open executeInstructionCommon
module WuzhengInstructions =


// --------------------------------------------------- Beginning of CMP/CMN/TEQ/TST -------------------------------------------------------------
// ------------------------------------------------------- Created by Wuzheng ------------------------------------------------

// ------------------------------------------------------- CMP -------------------------------------------------------------
    let executeCMP (op1, op2, shift) input =
        let setFlagOnDefault = Flag
        let ((regs: int array), ms, insLst,flags,errormsg) = input
        match op1, op2 with
        | Reg(x1), Reg(x2) ->
            let fo = regs.[x1]
            let so,errormsg = (shifter x2 shift flags regs)
            match errormsg with
            |Some(_) -> (regs,ms,insLst, flags,errormsg)
            |None -> (regs,ms,insLst, (ALUsetflag (fo, so, setFlagOnDefault) input "SUB"),errormsg)
        | Reg(x1), Value(x2) ->
            let fo = regs.[x1]
            let so = x2
            (regs,ms,insLst, (ALUsetflag (fo, so, setFlagOnDefault) input "SUB"),errormsg)
        | _ ->
            let errormsg = Some ("Operands are not in the correct format (must be a register or a number)")
            (regs, ms, insLst,flags, errormsg)  

    let cmp (cond, op1, op2, shift) input =
        let ((regs: int array), ms, insLst,flags, errormsg) = input
        if matchCond(cond, input) then
            regs.[15] <- regs.[15] + 4
            executeCMP(op1, op2, shift) input
        else
            regs.[15] <- regs.[15] + 4
            (regs, ms, insLst,flags, errormsg)  

// ------------------------------------------------------- CMN -------------------------------------------------------------
    let executeCMN (op1, op2,shift) input =
        let setFlagOnDefault = Flag
        let ((regs: int array), ms, insLst,flags, errormsg) = input
        match op1, op2 with
        | Reg(x1), Reg(x2) when x2 = 13 || x2 = 15 -> 
            let errormsg = Some ("second operand cannot be R13 or R15")
            (regs, ms, insLst,flags, errormsg)  
        | Reg(x1), Reg(x2) ->
            let fo = regs.[x1]
            let so, errormsg = (shifter x2 shift flags regs)
            match errormsg with
            |Some(_) -> (regs,ms,insLst, flags, errormsg)
            |None -> (regs,ms,insLst, (ALUsetflag (fo, so, setFlagOnDefault) input "ADD"),errormsg)
        | Reg(x1), Value(x2) ->
            let fo = regs.[x1]
            let so = x2
            (regs,ms,insLst, (ALUsetflag (fo, so, setFlagOnDefault) input "ADD"),errormsg)
        | _ -> 
            let errormsg = Some ("Operands are not in the correct format (must be a register or a number)")
            (regs, ms, insLst,flags, errormsg)  

    let cmn (cond, op1, op2, shift) input =
        let ((regs: int array), ms, insLst,flags, errormsg) = input
        if matchCond(cond, input) then
            regs.[15] <- regs.[15] + 4
            executeCMN(op1, op2, shift) input
        else
            regs.[15] <- regs.[15] + 4
            (regs, ms, insLst,flags, errormsg)    

// ------------------------------------------------------- TST --------------------------------------------------------------------------
    let executeTST (op1, op2,shift) input =
        let setFlagOnDefault = Flag
        let ((regs: int array), ms, insLst,flags, errormsg) = input
        match op1, op2 with
        | Reg(x1), Reg(x2) ->
            let newflags1 = movUpdateCFlag (setFlagOnDefault,shift,x1,x2) input
            let fo = regs.[x1]
            let so, errormsg = (shifter x2 shift flags regs)
            match errormsg with
            |Some(_) -> (regs,ms,insLst,flags, errormsg)
            |None ->
                let tempResult = int32(fo &&& so)
                let updateN = if tempResult < 0 then true else false
                let updateZ = if tempResult = 0 then true else false
                let newflags2 = {newflags1 with N = updateN; Z = updateZ}
                (regs,ms,insLst,newflags2, errormsg)
        | Reg(x1), Value(x2) ->
            let fo = regs.[x1]
            let so = x2
            (regs,ms,insLst, (ALUsetflag (fo, so, setFlagOnDefault) input "AND"),errormsg)
        | _ -> 
            let errormsg = Some "Operands are not in the correct format (must be a register or a number)"
            (regs, ms, insLst,flags, errormsg)  

    let tst (cond, op1, op2, shift) input =
        let ((regs: int array), ms, insLst,flags, errormsg) = input
        if matchCond(cond, input) then
            regs.[15] <- regs.[15] + 4
            executeTST(op1, op2, shift) input
        else
            regs.[15] <- regs.[15] + 4
            (regs, ms, insLst,flags, errormsg) 

// ------------------------------------------------------- TEQ ----------------------------------------------------------------------------------- 
    let executeTEQ (op1, op2,shift) input =
        let setFlagOnDefault = Flag
        let ((regs: int array), ms, insLst,flags, errormsg) = input
        match op1, op2 with
        | Reg(x1), Reg(x2) ->
            let newflags1 = movUpdateCFlag (setFlagOnDefault,shift,x1,x2) input
            let fo = regs.[x1]
            let so, errormsg = (shifter x2 shift flags regs)
            match errormsg with
            |Some(_) -> (regs,ms,insLst,flags, errormsg)
            |None -> 
                let tempResult = int32(fo ^^^ so)
                let updateN = if tempResult < 0 then true else false
                let updateZ = if tempResult = 0 then true else false
                let newflags2 = {newflags1 with N = updateN; Z = updateZ}
                (regs,ms,insLst,newflags2,errormsg)
        | Reg(x1), Value(x2) ->
            let fo = regs.[x1]
            let so = x2
            (regs,ms,insLst, (ALUsetflag (fo, so, setFlagOnDefault) input "EOR"),errormsg)
        | _ -> 
            let errormsg = Some "Operands are not in the correct format (must be a register or a number)"
            (regs, ms, insLst,flags, errormsg)  

    let teq (cond, op1, op2, shift) input =
        let ((regs: int array), ms, insLst,flags,errormsg) = input
        if matchCond(cond, input) then
            regs.[15] <- regs.[15] + 4
            executeTEQ(op1, op2, shift) input
        else
            regs.[15] <- regs.[15] + 4
            (regs, ms, insLst,flags, errormsg) 

// ------------------------------------------------------- End of CMP/TEQ/CMN/TST ------------------------------------------------
