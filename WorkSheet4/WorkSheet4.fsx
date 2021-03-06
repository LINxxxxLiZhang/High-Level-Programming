type Command = 
    | Read of string 
    | Write of (string*int) 
    | Assign of (string*string)
    | Parse of (string)


type Response =
    | VarValue of int // for commands that return data
    | ParseError // if command string is invalid
    | DataError // if the required data does not exist
    | OK // for valid commads that return no data




let makeEnvironment ()= 

    let mutable state = Map.empty

    ///read operation. Return the value of file if file exists, otherwise, return DataError.
    let readFile fileName = 
        match state |> Map.tryFind fileName with
        | Some(f) -> VarValue f
        | None -> DataError

    ///write operation. If file exist, update the file. If file doesn't exist, add the file. Always return OK.
    let writeFile (fileName:string,writeValue:int) = 
        state <- (state.Add(fileName,writeValue))
        OK

    ///assign the value of file2 to file1. Return OK if file2 exist, otherwise return DataError
    let assignFile (file1,file2) = 
        match state |> Map.tryFind file2 with
        | Some(file2Value) -> 
            state <- state.Add(file1, file2Value)
            OK
        | None -> DataError
    
    /// returns true if string is all alphabetic chars
    let strIsAlpha (str:string) = 
        let isAlpha (c:char) = List.contains c (['a'..'z'] @ ['A'..'Z'])
        str |> List.ofSeq |> List.forall isAlpha
               
    /// matches integer string returning the integer
    let (|IsInt|_|) s =
        try
            Some (int s)
        with _ -> None

    ///parse operation. Return ParseError if input is invalid, otherwise do corresponding correct operations
    let parseInstruction (str:string) =
        /// tokenise the input string into a list of string commands 
        let splitStr = 
            str.Split [|' ';'\n';'\f';'\t';'\r'|] 
            |> Array.filter ((<>) "") //deleting empty strings generated by default .Split function
            |> List.ofArray
        /// parse the command string list
        let parse (strLst: string list) =
            match strLst with
            | ["READ";f] when strIsAlpha f -> readFile f
            | ["WRITE";f;IsInt n] when strIsAlpha f -> writeFile (f,n) 
            | ["ASSIGN";f1;f2] when strIsAlpha f1 && strIsAlpha f2 -> assignFile (f1,f2)
            | _ -> ParseError
        splitStr |> parse

    /// environment function, executes command
    let environment (command:Command) = 
        match command with
        | Read (fileName) -> readFile fileName
        | Write (fileName, writeValue) -> writeFile (fileName,writeValue)
        | Assign (file1,file2) -> assignFile (file1,file2)
        | Parse (instruction) -> parseInstruction instruction
     

    environment 




