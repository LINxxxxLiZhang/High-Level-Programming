namespace HLP

open Expecto
open common
open testingcommon

module expectoparsertest =
    
    ///////////////////////////////////////////////// VARIOUS TYPE DEFS ///////////////////////////////////////////////
    type Link = Lnk | NoLnk
    type Address = Label1 | Label2 | Label3
        with member x.toStr = sprintf "%A" x

    type RegistersforMult = R1 | R2 | R3 | R4 | R5 | R6 
                            | R1toR3 | R2toR6 | R3toR4
        with 
        member x.toStr =
            match x with
            | R1 -> "r1"
            | R2 -> "r2"
            | R3 -> "r3"
            | R4 -> "r4"
            | R5 -> "r5"
            | R6 -> "r6"
            | R1toR3 -> "r1-r3"
            | R2toR6 -> "r2-r6"
            | R3toR4 -> "r3-r4"

        member x.getReg =
            match x with
            | R1 -> [Reg 1]
            | R2 -> [Reg 2]
            | R3 -> [Reg 3]
            | R4 -> [Reg 4]
            | R5 -> [Reg 5]
            | R6 -> [Reg 6]
            | R1toR3 -> [Reg 1;Reg 2;Reg 3]
            | R2toR6 -> [Reg 2;Reg 3;Reg 4;Reg 5;Reg 6]
            | R3toR4 -> [Reg 3;Reg 4]    

//////////////////////////////////////////// BUILDER FUNCTIONS /////////////////////////////////////:


    let runMultRegMatch (reglst:RegistersforMult list) = 
        let makeMultstr (reglst:RegistersforMult list) =
            let regstrlst = List.map (fun (c:RegistersforMult) -> c.toStr) reglst
            ["{";regstrlst.Head]
            @(regstrlst.Tail |> List.map (fun c -> [",";c]) |> List.collect (id))
            @["}"]

        match reglst with
        |[] -> []    
        |_ -> match reglst |> makeMultstr with
              |readASM.MultRegMatch(registers) -> registers
              |_ -> []

    let refBuilderMultReg (reglst:RegistersforMult list) =
        reglst |> List.map (fun (c:RegistersforMult) -> c.getReg) |> List.collect (id)
    
    let mov (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegOrVal,shift:TestShift) = 
        let tmp = "MOV" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr
        match r2 with
        |Val(_)|SmallVal(_) -> (tmp,MOV(cond,flag,r1.convt,r2.convt,Shift.NoShift))
        |_-> (tmp + shift.toStr,MOV(cond,flag,r1.convt,r2.convt,shift.convt))

    let add (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        let tmp = "ADD" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr
        match r3 with
        |Val(_)|SmallVal(_) -> (tmp,ADD(cond,flag,r1.convt,r2.convt,r3.convt,Shift.NoShift))
        |_->(tmp + shift.toStr,ADD(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))

    let adc (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        let tmp = "ADC" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr
        match r3 with
        |Val(_)|SmallVal(_) -> (tmp,ADC(cond,flag,r1.convt,r2.convt,r3.convt,Shift.NoShift))
        |_->(tmp + shift.toStr,ADC(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))

    let sub (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        let tmp = "SUB" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr
        match r3 with
        |Val(_)|SmallVal(_) -> (tmp,SUB(cond,flag,r1.convt,r2.convt,r3.convt,Shift.NoShift))
        |_->(tmp + shift.toStr,SUB(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))

    let sbc (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        let tmp = "SBC" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr
        match r3 with
        |Val(_)|SmallVal(_) -> (tmp,SBC(cond,flag,r1.convt,r2.convt,r3.convt,Shift.NoShift))
        |_->(tmp + shift.toStr,SBC(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))

    let rsb (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        let tmp = "RSB" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr
        match r3 with
        |Val(_)|SmallVal(_) -> (tmp,RSB(cond,flag,r1.convt,r2.convt,r3.convt,Shift.NoShift))
        |_->(tmp + shift.toStr,RSB(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))

    let rsc (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        let tmp = "RSC" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr
        match r3 with
        |Val(_)|SmallVal(_) -> (tmp,RSC(cond,flag,r1.convt,r2.convt,r3.convt,Shift.NoShift))
        |_->(tmp + shift.toStr,RSC(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))


    let mul (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegs,shift:TestShift) = 
        ("MUL" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr + shift.toStr,
         MUL(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))

    let mvn (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegOrVal,shift:TestShift) = 
        let tmp = "MVN" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr
        match r2 with
        |Val(_)|SmallVal(_) -> (tmp,MVN(cond,flag,r1.convt,r2.convt,Shift.NoShift))
        |_-> (tmp + shift.toStr,MVN(cond,flag,r1.convt,r2.convt,shift.convt))


    let leftshift (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        ("LSL" + flag.toStr + cond.toStr + " "  + r1.toStr + "," + r2.toStr + "," + r3.toStr,
         LSLInst(cond,flag,r1.convt,r2.convt,r3.convt))

    let rightshift (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        ("LSR" + flag.toStr + cond.toStr + " "  + r1.toStr + "," + r2.toStr + "," + r3.toStr,
         LSRInst(cond,flag,r1.convt,r2.convt,r3.convt))

    let arithright (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        ("ASR" + flag.toStr + cond.toStr + " "  + r1.toStr + "," + r2.toStr + "," + r3.toStr,
         ASRInst(cond,flag,r1.convt,r2.convt,r3.convt))

    let roright (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        ("ROR" + flag.toStr + cond.toStr + " "  + r1.toStr + "," + r2.toStr + "," + r3.toStr,
         RORInst(cond,flag,r1.convt,r2.convt,r3.convt))

    let rrxshift (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,shift:TestShift) = 
        ("RRX" + flag.toStr + cond.toStr + " "  + r1.toStr + "," + r2.toStr,
          RRXInst(cond,flag,r1.convt,r2.convt))

    let andl (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        let tmp = "AND" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr
        match r3 with
        |Val(_)|SmallVal(_) -> (tmp,AND(cond,flag,r1.convt,r2.convt,r3.convt,Shift.NoShift))
        |_->(tmp + shift.toStr,AND(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))

    let eorl (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        let tmp = "EOR" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr
        match r3 with
        |Val(_)|SmallVal(_) -> (tmp,EOR(cond,flag,r1.convt,r2.convt,r3.convt,Shift.NoShift))
        |_->(tmp + shift.toStr,EOR(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))

    let bicl (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        let tmp = "BIC" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr
        match r3 with
        |Val(_)|SmallVal(_) -> (tmp,BIC(cond,flag,r1.convt,r2.convt,r3.convt,Shift.NoShift))
        |_->(tmp + shift.toStr,BIC(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))


    let orrl (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift) = 
        let tmp = "ORR" + flag.toStr + cond.toStr + " " + r1.toStr + "," + r2.toStr + "," + r3.toStr
        match r3 with
        |Val(_)|SmallVal(_) -> (tmp,ORR(cond,flag,r1.convt,r2.convt,r3.convt,Shift.NoShift))
        |_->(tmp + shift.toStr,ORR(cond,flag,r1.convt,r2.convt,r3.convt,shift.convt))

    let cmn (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegOrVal,shift:TestShift) = 
        let tmp = "CMN" + cond.toStr + " "  + r1.toStr + "," + r2.toStr
        match r2 with
        |Val(_)|SmallVal(_) -> (tmp,CMN(cond,r1.convt,r2.convt,Shift.NoShift))
        |_-> (tmp + shift.toStr,CMN(cond,r1.convt,r2.convt,shift.convt))

    let cmp (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegOrVal,shift:TestShift) = 
        let tmp = "CMP" + cond.toStr + " "  + r1.toStr + "," + r2.toStr
        match r2 with
        |Val(_)|SmallVal(_) -> (tmp,CMP(cond,r1.convt,r2.convt,Shift.NoShift))
        |_-> (tmp + shift.toStr,CMP(cond,r1.convt,r2.convt,shift.convt))

    let tst (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegOrVal,shift:TestShift) = 
        let tmp = "tst" + cond.toStr + " "  + r1.toStr + "," + r2.toStr
        match r2 with
        |Val(_)|SmallVal(_) -> (tmp,TST(cond,r1.convt,r2.convt,Shift.NoShift))
        |_-> (tmp + shift.toStr,TST(cond,r1.convt,r2.convt,shift.convt))

    let teq (cond:Conditional,flag:Setflags,r1:TestRegs,r2:TestRegOrVal,shift:TestShift) = 
        let tmp = "teq" + cond.toStr + " "  + r1.toStr + "," + r2.toStr
        match r2 with
        |Val(_)|SmallVal(_) -> (tmp,TEQ(cond,r1.convt,r2.convt,Shift.NoShift))
        |_-> (tmp + shift.toStr,TEQ(cond,r1.convt,r2.convt,shift.convt))



    let ldr (dt:DataType,cond:Conditional,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift,indexing:Indexing) = 
        let tmp = "LDR" + dt.toStr + cond.toStr + " " + r1.toStr + ",[" + r2.toStr
        let shiftstr = 
            match r3 with
            |Val(_)|SmallVal(_) -> ""
            |_ -> shift.toStr

        let curshift =
            match r3 with
            |Val(_)|SmallVal(_) -> Shift.NoShift
            |_ -> shift.convt

        match indexing with
        |Immediate -> (tmp + "," + r3.toStr + shiftstr + "]",LDR(dt,cond,r1.convt,r2.convt,r3.convt,curshift,Immediate))
        |Post -> (tmp + "]"+  "," + r3.toStr + shiftstr,LDR(dt,cond,r1.convt,r2.convt,r3.convt,curshift,Post))
        |Pre -> (tmp +  "," + r3.toStr + shiftstr + "]!",LDR(dt,cond,r1.convt,r2.convt,r3.convt,curshift,Pre))

    let str (dt:DataType,cond:Conditional,r1:TestRegs,r2:TestRegs,r3:TestRegOrVal,shift:TestShift,indexing:Indexing) = 
        let tmp = "STR" + dt.toStr + cond.toStr + " " + r1.toStr + ",[" + r2.toStr
        let shiftstr = 
            match r3 with
            |Val(_)|SmallVal(_) -> ""
            |_ -> shift.toStr

        let curshift =
            match r3 with
            |Val(_)|SmallVal(_) -> Shift.NoShift
            |_ -> shift.convt

        match indexing with
        |Immediate -> (tmp + "," + r3.toStr + shiftstr + "]",STR(dt,cond,r1.convt,r2.convt,r3.convt,curshift,Immediate))
        |Post -> (tmp + "]"+  "," + r3.toStr + shiftstr,STR(dt,cond,r1.convt,r2.convt,r3.convt,curshift,Post))
        |Pre -> (tmp +  "," + r3.toStr + shiftstr + "]!",STR(dt,cond,r1.convt,r2.convt,r3.convt,curshift,Pre))

    type LdrPseudoTest = Addr of Address | Num of int

    let ldrpseudo (cond:Conditional,r1:TestRegs,a:LdrPseudoTest) =
        let tmp = "ldr" + cond.toStr + " " + r1.toStr + ",="
        match a with
        |Addr(addr) -> (addr.toStr + " " + tmp + addr.toStr,LDRPseudo(cond,r1.convt,Address 0))
        |Num(number) -> (tmp + number.ToString(),LDRPseudo(cond,r1.convt,ILDRPseudoArg.Val number))
    
    let converttoTestReg (reg:TestRegs) =
        match reg with
        |TestRegs.R1 -> RegistersforMult.R1
        |TestRegs.R2 -> RegistersforMult.R2
        |TestRegs.R3 -> RegistersforMult.R3
        |TestRegs.R4 -> RegistersforMult.R4

    let ldm (cond:Conditional,mode:STLDMmode,update:SPupdate,r1:TestRegs,r2:TestRegs,reglst:RegistersforMult list) =

        let makeMultstr (reglst:RegistersforMult list) =
            let regstrlst = List.map (fun (c:RegistersforMult) -> c.toStr) reglst
            "{" + regstrlst.Head + (regstrlst.Tail 
                                    |> List.map (fun c -> [",";c]) 
                                    |> List.collect (id) 
                                    |> String.concat "") + "}" 
        let updatestr =
            match update with
            |Update -> "!"
            |NoUpdate -> ""

        let allregs = (r2 |> converttoTestReg) :: reglst //to always have at least one element in the {}
        let str = "ldm" + cond.toStr + mode.toStr + " " + r1.toStr + updatestr + "," + (allregs |> makeMultstr)
        (str,
         LDM(cond,mode,update,r1.convt,allregs |> refBuilderMultReg))


    let stm (cond:Conditional,mode:STLDMmode,update:SPupdate,r1:TestRegs,r2:TestRegs,reglst:RegistersforMult list) =

        let makeMultstr (reglst:RegistersforMult list) =
            let regstrlst = List.map (fun (c:RegistersforMult) -> c.toStr) reglst
            "{" + regstrlst.Head + (regstrlst.Tail 
                                    |> List.map (fun c -> [",";c]) 
                                    |> List.collect (id) 
                                    |> String.concat "") + "}" 
        let updatestr =
            match update with
            |Update -> "!"
            |NoUpdate -> ""

        let allregs = (r2 |> converttoTestReg) :: reglst
        let str = "stm" + cond.toStr + mode.toStr + " " + r1.toStr + updatestr + "," + (allregs |> makeMultstr)
        (str,
         STM(cond,mode,update,r1.convt,allregs |> refBuilderMultReg))
    let branch (cond:Conditional,address:Address,link:Link) =
        match link with
        |Lnk -> (address.toStr + " bl" + cond.toStr + " " + address.toStr,BL(cond,0))
        |NoLnk -> (address.toStr + " b" + cond.toStr + " " + address.toStr,B(cond,0))


//////////////////////////////////// PROPERTY AND TEST FUNCTIONS ////////////////////////////////

    let fsConfig = { FsCheck.Config.Default with MaxTest = 1000 ; QuietOnSuccess=false}

    //run readASM
    let test input =
        let (_,_,instrmap,_,_) = [input] |> readASM.getMachineState
        instrmap
        |> Map.toSeq 
        |> List.ofSeq 
        |> List.map (fun (_,x) -> x)
        |> List.head

    let addNoise ((str:string),instr) =
        let rnd = System.Random()
        let ind1 = rnd.Next(0,str.Length)
        let ind2 = rnd.Next(0,str.Length)
        let ind3 = rnd.Next(0,str.Length)
        let output = str.ToCharArray()
        output.[ind1] <- '@'
        if ind2 <> ind1 then output.[ind2] <- '/'
        if ind3 <> ind2 && ind3 <> ind1 then output.[ind3] <- ' '
        (output |> System.String,ParseError)

    //1 input
    let testProperty1 pName instr1 instr2 =
        testPropertyWithConfig fsConfig pName 
        <| fun p1 -> 
            Expect.equal (p1 |> instr1) (p1 |> instr2)
    //3 inputs
    let testProperty3 pName instr =
        testPropertyWithConfig fsConfig pName 
        <| fun (p1,p2,p3) -> 
            let build = (p1,p2,p3) |> instr
            Expect.equal (build |> fst |> test) (build |> snd) (build|>fst) 

    //5 inputs
    let testProperty5 pName instr =
        testPropertyWithConfig fsConfig pName 
        <| fun (cond,flag,p1,p2,shift) -> 
            let build = (cond,flag,p1,p2,shift) |> instr 
            Expect.equal (build |> fst |> test) (build |> snd) (build|>fst) 

    //6 inputs
    let testProperty6 pName instr =
        testPropertyWithConfig fsConfig pName 
        <| fun (cond,flag,p1,p2,p3,shift) -> 
            let build = (cond,flag,p1,p2,p3,shift) |> instr
            Expect.equal ((build |> fst) |> test) (build |> snd) (build|>fst) 

    //7 inputs

    let testProperty7 pName instr =
        testPropertyWithConfig fsConfig pName 
        <| fun (dt,cond,p1,p2,p3,shift,indexing) -> 
            let build = (dt,cond,p1,p2,p3,shift,indexing) |> instr
            Expect.equal ((build |> fst) |> test) (build |> snd) (build|>fst) 

    
/////////////////////////////////////////////// PROPERTY DEFINITIONS ///////////////////////////////////////////////////:

    let propertiesValid =
        testList "FsCheck"
            [
                testProperty5 "mov test" mov
                testProperty5 "mvn test" mvn
                testProperty6 "add test" add
                testProperty6 "adc test" adc
                testProperty6 "sub test" sub
                testProperty6 "sbc test" sbc
                testProperty6 "rsb test" rsb
                testProperty6 "rsc test" rsc
                testProperty6 "mul test" mul
                testProperty6 "lsl test" leftshift
                testProperty6 "lsr test" rightshift
                testProperty6 "asr test" arithright
                testProperty6 "ror test" roright
                testProperty5 "rrx test" rrxshift
                testProperty7 "ldr test" ldr
                testProperty7 "str test" str
                testProperty5 "cmp test" cmp
                testProperty5 "cmn test" cmn
                testProperty5 "tst test" tst
                testProperty5 "teq test" teq
                testProperty6 "and test" andl
                testProperty6 "orr test" orrl
                testProperty6 "eor test" eorl
                testProperty6 "bic test" bicl
                //testProperty1 "mult reg test for stm and ldm" runMultRegMatch refBuilderMultReg
                testProperty6 "ldm test" ldm
                testProperty6 "stm test" stm
                testProperty3 "b&bl test" branch 
                testProperty3 "ldrpseudo test" ldrpseudo
            ]

    let propertiesInvalid =
        testList "FsCheck"
            [
                testProperty5 "mov invalid test" (mov >> addNoise)
                testProperty5 "mvn invalid test" (mvn >> addNoise)
                testProperty6 "add invalid test" (add >> addNoise)
                testProperty6 "adc invalid test" (adc >> addNoise)
                testProperty6 "sub invalid test" (sub >> addNoise)
                testProperty6 "sbc invalid test" (sbc >> addNoise)
                testProperty6 "rsb invalid test" (rsb >> addNoise)
                testProperty6 "rsc invalid test" (rsc >> addNoise)
                testProperty6 "mul invalid test" (mul >> addNoise)
                testProperty6 "lsl invalid test" (leftshift >> addNoise)
                testProperty6 "lsr invalid test" (rightshift >> addNoise)
                testProperty6 "asr invalid test" (arithright >> addNoise)
                testProperty6 "ror invalid test" (roright >> addNoise)
                testProperty5 "rrx invalid test" (rrxshift >> addNoise)
                testProperty7 "ldr invalid test" (ldr >> addNoise)
                testProperty7 "str invalid test" (str >> addNoise)
                testProperty5 "cmp invalid test" (cmp >> addNoise)
                testProperty5 "cmn invalid test" (cmn >> addNoise)
                testProperty5 "tst invalid test" (tst >> addNoise)
                testProperty5 "teq invalid test" (teq >> addNoise)
                testProperty6 "and invalid test" (andl >> addNoise)
                testProperty6 "orr invalid test" (orrl >> addNoise)
                testProperty6 "eor invalid test" (eorl >> addNoise)
                testProperty6 "bic invalid test" (bicl >> addNoise)
                testProperty6 "ldm invalid test" (ldm >> addNoise)
                testProperty6 "stm invalid test" (stm >> addNoise)
                //testProperty3 "b&bl invalid test" (branch >> addNoise) 
            ]
    let config = Expecto.Tests.defaultConfig //parallel is on by default
    let RASMExpectoTest = fun () -> runTests config propertiesValid |> ignore
                                    runTests config propertiesInvalid |> ignore
