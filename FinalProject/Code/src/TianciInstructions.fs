namespace HLP
open common
open executeInstructionCommon
module TianciInstructions =



//  ------------------------------------------------------- Begining of ALU -------------------------------------------------------------
// ------------------------------------------------------- Created by Tianci/Wuzheng ------------------------------------------------

// ------------------------------------------------------- add -------------------------------------------------------------
    let executeADD ((regs: int array), reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input =
        regs.[reg] <- regs.[x1] + so
        (regs,ms,insLst, (ALUsetflag (fo, so, setf) input "ADD"),None)
  
//    // ------------------------------------------------------- adc -------------------------------------------------------------
//        let executeADC ((regs: int array), reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) =
//            regs.[reg] <- regs.[x1] + so + carry
//            (regs,ms,insLst, (ALUsetflag (fo, so + carry, setf) input "ADD"),None)

    let executeADC ((regs: int array), reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input =
        regs.[reg] <- regs.[x1] + so + carry
        if flags.C = false then
            (regs,ms,insLst, (ALUsetflag (fo, so, setf) input "ADD"),None)
        else 
            (regs,ms,insLst, (ALUsetflag (fo, so, setf) input "ADC"),None)
            


// ------------------------------------------------------- SUB -------------------------------------------------------------
    let executeSUB ((regs: int array), reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input =
        regs.[reg] <- regs.[x1] - so
        (regs,ms,insLst, (ALUsetflag (fo, so, setf) input "SUB"),None)

     
// ------------------------------------------------------- SBC -------------------------------------------------------------
    let executeSBC ((regs: int array), reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input =
        regs.[reg] <- regs.[x1] - soAfter
        if flags.C = false then
            (regs,ms,insLst, (ALUsetflag (fo, so, setf) input "SBC"), None)
        else 
            (regs,ms,insLst, (ALUsetflag (fo, so, setf) input "SUB"), None)

// ------------------------------------------------------- RSB -------------------------------------------------------------
    let executeRSB ((regs: int array), reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input =
        regs.[reg] <- so - regs.[x1]
        (regs,ms,insLst, (ALUsetflag (so, fo, setf) input "SUB"), None)

// ------------------------------------------------------- RSC -------------------------------------------------------------
    let executeRSC ((regs: int array), reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input =
        regs.[reg] <- soAfter - regs.[x1]
        if flags.C = false then
            (regs,ms,insLst, (ALUsetflag (so, fo, setf) input "SBC"),None)
        else
            (regs,ms,insLst, (ALUsetflag (so, fo, setf) input "SUB"),None)

    let executeAR(setf, dest, op1, op2,shift) input instName =
            let ((regs: int array), ms, insLst,flags, errormsg) = input
            match dest with
            | Reg(reg) ->
                match op1, op2 with
                | Reg(x1), Reg(x2) ->
                    let fo = regs.[x1]
                    let carry = if (instName = "SBC" || instName = "ADC") then (if flags.C then 1 else 0) else (if flags.C then 0 else 1)
                    let so,errormsg = (shifter x2 shift flags regs)
                    match errormsg with
                    |Some(_) -> (regs,ms,insLst, flags,errormsg)
                    |None ->
                        let soAfter = (so - carry)
                        match instName with
                        |"RSC" -> executeRSC (regs, reg, x1, fo, so, (so - carry), carry, ms, insLst, setf, flags) input
                        |"RSB" -> executeRSB (regs, reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input
                        |"SBC" -> executeSBC (regs, reg, x1, fo, so, (so + 1 - carry), carry, ms, insLst, setf, flags) input
                        |"SUB" -> executeSUB (regs, reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input
                        |"ADC" -> executeADC (regs, reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input
                        |"ADD" -> executeADD (regs, reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input
                | Reg(x1), Value(x2) ->
                    let fo = regs.[x1]
                    let carry = if (instName = "SBC" || instName = "ADC") then (if flags.C then 1 else 0) else (if flags.C then 0 else 1)
                    let so = x2
                    let soAfter = (so - carry)
                    match instName with
                    |"RSC" -> executeRSC (regs, reg, x1, fo, so, (so - carry), carry, ms, insLst, setf, flags) input
                    |"RSB" -> executeRSB (regs, reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input
                    |"SBC" -> executeSBC (regs, reg, x1, fo, so, (so + 1 - carry), carry, ms, insLst, setf, flags) input
                    |"SUB" -> executeSUB (regs, reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input
                    |"ADC" -> executeADC (regs, reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input
                    |"ADD" -> executeADD (regs, reg, x1, fo, so, soAfter, carry, ms, insLst, setf, flags) input
                    |_ -> failwithf "not implemented"
                | _ -> 
                    let errormsg = Some ("Operands are not in the correct format (must be a register or a number)")
                    (regs,ms,insLst,flags, errormsg)
            | _ -> 
                let errormsg = Some "Destination of BIC must be a register"
                (regs,ms,insLst,flags, errormsg)

    let ARInst (cond, setf, dest, op1, op2, shift) input instName=
        let ((regs: int array), ms, insLst,flags, errormsg) = input
        if matchCond(cond, input) then
            regs.[15] <- regs.[15] + 4
            executeAR(setf, dest, op1, op2, shift) input instName
        else
            regs.[15] <- regs.[15] + 4
            (regs, ms, insLst,flags,errormsg) 
 


// ----------------------------------------------------- Beginning of BITWISE OPERATOR ---------------------------------------------------
// ------------------------------------------------------- Created by Tianci ------------------------------------------------

    let executeBITWISE(setf, dest, op1, op2,shift) input instName =
            let ((regs: int array), ms, insLst,flags,errormsg) = input
            match dest with
            | Reg(reg) ->
                match op1, op2 with
                | Reg(x1), Reg(x2) ->
                    let newflags1 = movUpdateCFlag (setf,shift,dest,x2) input
                    let fo = regs.[x1]
                    let so, errormsg = (shifter x2 shift flags regs)
                    match errormsg with
                    |Some(_) -> (regs,ms,insLst,flags, errormsg)
                    |None ->
                        match instName with
                        |"AND" ->
                            regs.[reg] <- int32(fo &&& so)
                        |"ORR" ->
                            regs.[reg] <- int32(fo ||| so)
                        |"EOR" ->
                            regs.[reg] <- int32(fo ^^^ so)
                        |"BIC" -> 
                            regs.[reg] <- int32(fo &&& (~~~so))
                        |_ -> failwithf "not implemented"
                        match setf with
                        |Flag ->
                            let updateN = if regs.[reg] < 0 then true else false
                            let updateZ = if regs.[reg] = 0 then true else false
                            let newflags2 = {newflags1 with N = updateN; Z = updateZ}
                            (regs,ms,insLst,newflags2,errormsg)
                        |NoFlag -> (regs,ms,insLst,flags,errormsg)
                | Reg(x1), Value(x2) ->
                    let fo = regs.[x1]
                    let so = x2
                    match instName with
                    |"AND" ->
                            regs.[reg] <- int32(fo &&& so)
                            (regs,ms,insLst, (ALUsetflag (fo, so, setf) input "AND"),errormsg)
                    |"ORR" ->
                            regs.[reg] <- int32(fo ||| so)
                            (regs,ms,insLst, (ALUsetflag (fo, so, setf) input "ORR"),errormsg)
                    |"EOR" ->
                            regs.[reg] <- int32(fo ^^^ so)
                            (regs,ms,insLst, (ALUsetflag (fo, so, setf) input "EOR"),errormsg)
                    |"BIC" -> 
                            regs.[reg] <- int32(fo &&& (~~~so))
                            (regs,ms,insLst, (ALUsetflag (fo, so, setf) input "BIC"),errormsg)
                    |_ -> failwithf "not implemented"
                | _ -> 
                    let errormsg = Some "Operands are not in the correct format (must be a register or a number)"
                    (regs, ms, insLst,flags, errormsg)  
            | _ -> 
                let errormsg = Some "Destination of BIC must be a register"
                (regs, ms, insLst,flags, errormsg)  

    let bitwiseInst (cond, setf, dest, op1, op2, shift) input instName=
        let ((regs: int array), ms, insLst,flags,errormsg) = input
        if matchCond(cond, input) then
            regs.[15] <- regs.[15] + 4
            executeBITWISE(setf, dest, op1, op2, shift) input instName
        else
            regs.[15] <- regs.[15] + 4
            (regs, ms, insLst,flags,errormsg) 

// ------------------------------------------------------- End of Bitwise Operator ------------------------------------------------
   