namespace HLP
open common

module testingcommon = 

    /// registers to be tested
    type TestRegs =
        |R1
        |R2
        |R3
        |R4
        with
        /// get register index
        /// e.g. R1 -> 1
        member r.getIdx = 
            match r with
            |R1 -> 1
            |R2 -> 2
            |R3 -> 3
            |R4 -> 4
        /// convert to string
        /// e.g. R1 -> "R1"
        member r.toStr =
            match r with
            |_ -> sprintf "%A" r
        /// convert TestRegs D.U type to Register.Reg type used in common.fs
        member r.convt =
            match r.getIdx with
            |idx -> Reg (idx) 

    /// for testing purpose
    /// generate either a register or a value
    type TestRegOrVal = 
        |R1
        |R2
        |R3
        |R4
        |Val of uint8 // used for immediate value
        |SmallVal of uint8 // used for testing shifts with values in the range of 0-32
        with
//        member v.getValue = 
//            match v with
//            |Val (v) -> v 
//            |SmallVal (v) -> v
//            |_ -> failwith "regs shouldn't have value"
        
        /// get register index
        member r.getIdx =
            match r with
            |R1 -> 1
            |R2 -> 2
            |R3 -> 3
            |R4 -> 4 
            |Val (_) -> failwith "expecting a reg instead of number" 
            |SmallVal (_) -> failwith "expecting a reg instead of number" 
        
        /// convert to string
        /// e.g. 1 -> "#1"
        /// ---- R1 -> "R1"
        member r.toStr =
            match r with
            |Val (r) -> "#" + r.ToString()
            |SmallVal (r) -> "#" + (r>>>3).ToString()
            |_ -> sprintf "%A" r

        /// convert TestRegOrVal D.U type to Register Type used in common.fs
        member r.convt =
            match r with
            |Val (r) -> Value (int32 r)
            |SmallVal (r) -> Value (int32 (r>>>3))
            |_ -> Reg (r.getIdx)


    /// for test purpose
    /// generate data type similar to common.Shift
    type TestShift =
        |LSL of TestRegOrVal
        |LSR of TestRegOrVal
        |ASR of TestRegOrVal
        |ROR of TestRegOrVal
        |RRX 
        |NoShift
        with 
        ///convert TestShift D.U type to Shift D.U used in common.fs
        member m.convt = 
            match m with
            |LSL (m) -> Shift.LSL (m.convt)
            |LSR (m) -> Shift.LSR (m.convt)
            |ASR (m) -> Shift.ASR (m.convt)
            |ROR (m) -> Shift.ROR (m.convt)
            |RRX -> Shift.RRX
            |NoShift -> Shift.NoShift
        /// convert to string.
        /// e.g. LSL R1 -> ", LSL R1"
        /// ---- LSL Value(1) -> ", LSL #1"
        /// ---- NoShift -> ""
        member m.toStr =
            match m with
            |LSL (r) -> "," + "LSL" + " " + r.toStr 
            |LSR (r) -> "," + "LSR" + " " + r.toStr
            |ASR (r) -> "," + "ASR" + " " + r.toStr
            |ROR (r) -> "," + "ROR" + " " + r.toStr
            |RRX -> ",RRX"
            |NoShift -> ""

    
    /// generate a 8 bit signed int
    type RandNum =
        |Rand of int8
        with
        /// e.g. #-1
        member m.toStr = 
            match m with 
            |Rand (x) -> "#" + x.ToString()
        member m.convt = 
            match m with
            |Rand (x) -> Value (int32 x)


    type TestDataType =
        |By
        |W
        with 
        /// "B" or ""
        member m.toStr = 
            match m with
            |By -> "B"
            |W -> ""
        member m.convt = 
            match m with
            |By -> DataType.By
            |W -> DataType.W
          
    /// This is used to get the status of flags NZCV from Visual
    /// postlude which sets R5 bits to status bit values
    let NZCVToR5 =
       "
          MOV R5, #0
          ADDMI R5, R5, #8
          ADDEQ R5, R5, #4
          ADDCS R5, R5, #2
          ADDVS R5, R5, #1
       " 
    /// data types of test instructions 
    type TestType = TestDataType*Conditional*Setflags*TestRegs*TestRegs*TestRegOrVal*TestShift * RandNum * Indexing


    /// initial value for flags
    let initFlags:Flag = 
        {
            N = false
            Z = false
            C = false
            V = false
        }
    
    /// convert flags output of Visual to the type system that is used for our Flags 
    let ConvVisualFlags (flags:bool*bool*bool*bool) :Flag = 
        let n,z,c,v = flags
        {
            N = n
            Z = z
            C = c
            V = v
        }
