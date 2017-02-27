namespace HLP

module execuateInstruction =

    open commons

    let rec execuateInstruction (input:Machinestate) = 

        let mov dest op1 input = 
            //e.g. MOV R1, #4
            //e.g. MOV R1, R2
            //e.g. MOV R1, R2, LSL #3
            let (regs, ms, insLst) = input
            let pc = regs.[15]
            match dest with
            | Reg(reg) ->  //need to define Reg in Common 
                match op1 with
                | Int(x) -> 
                    regs.[reg] <- x //update register
                    pc <- pc + 4 
                    execuateInstruction (regs, ms, insLst)
                | Reg(x) ->
                    regs.[reg] <- regs.[x]
                    pc <- pc + 4 
                    execuateInstruction (regs, ms, insLst)
            | _ -> failwith "Destination of MOV must be a register"

        // add is similar
        let add dest op1 op2 input =
            //e.g. ADD R1,R2,R3
            //e.g. ADD R1,R2,10
            //e.g. ADD R1,R2,R3,LSL #5 --------- ADD with logic shift will probably be handled in a separate special case. 
                                     //--------- This probably needs to be decisded in the next meeting. 
            let (regs, ms, insLst) = input
            let pc = regs.[15]
            match dest with
            | Reg(reg) ->
                match op1,op2 with
                | Reg(x1), Reg(x2) ->
                    regs.[reg] <- regs.[x1]+regs.[x2]
                    pc <- pc + 4 
                    execuateInstruction (regs, ms, insLst)
                | Reg(x1), Int(x2) ->
                    regs.[reg] <- regs.[x1]+x2
                    pc <- pc + 4
                    execuateInstruction (regs, ms, insLst)
            | _ -> failwith "Destination of ADD must be a register"
        ///start executing instruction list cell by cell
        let execuate input = 
            // get the registers, PC. memory states and instruction list
            let (regs, ms, insLst) = input 
            let pc = regs.[15]
            match insList.[pc] with
            | MOV(dest, op1) -> mov dest op1 input

            | ADD(dest, op1, op2) -> add dest op1 op2 input
        execuate input



