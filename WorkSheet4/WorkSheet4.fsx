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
    //.Add("James",1).Add("Dave",2).Add("Kevin",3)

    let rec environment (command:Command) = 

        ///return true if file exist
        let fileExist fileName = Map.containsKey fileName state

        //
        // ***************************************************** readFile **********************************************
        //
        ///read operation. Return the value of file if file exists, otherwise, return DataError.
        let readFile fileName = 
            match (fileExist fileName) with
            | true -> VarValue (Map.find fileName state)
            | false -> DataError

        //
        // **************************************************** writeFile **********************************************
        //
        ///write operation. If file exist, update the file. If file doesn't exist, add the file. Always return OK.
        let writeFile (fileName:string,writeValue:int) = 

            let addState fileName writeValue =
                state <- (state.Add(fileName,writeValue))

            let updateState fileName writeValue =
                state <- (state.Remove(fileName).Add(fileName,writeValue))

            let (|WriteFile|_|) (fileName,writeValue) = 
                match Map.tryFindKey (fun key value -> key = fileName) state with
                | None -> Some(addState fileName writeValue)
                | Some(x) -> Some(updateState fileName writeValue)

            match (fileName,writeValue) with
            | WriteFile(y) -> OK
            | _ -> OK

        //
        // ************************************************* assignFile ************************************************
        //       
        ///assign operation. Return OK if file2 exist, otherwise return DataError
        let assignFile (file1,file2) = 

            let updateState file1 file2Value = 
                state <- state.Remove(file1).Add(file1,file2Value)

            let (|AssignFile|_|) file2 = 
                match Map.tryFind file2 state with
                | Some(file2Value) -> Some(updateState file1 file2Value)
                | None -> None

            match file2 with
            | AssignFile(y) -> OK
            | _ -> DataError
        
        //
        // ******************************************** parseInstruction ***********************************************
        //
        ///parse operation. Return ParseError if input is invalid, otherwise do corresponding correct operations
        let parseInstruction str =
            ///converting string into char list
            let stringToCharList str = str |> List.ofSeq

            /// check if all the characters of the input string are digits, alphabets, or invalid. 
            let (|AllDigit|AllAlpha|Invalid|) (str : string) = 
                let charList = stringToCharList str
                ///Check and return either AllAlpha or Invalid     
                let rec checkAlpha lst =
                    match lst with
                    | ch :: tail when List.contains ch (['a'..'z'] @ ['A'..'Z']) -> checkAlpha tail
                    | ch :: tail -> Invalid
                    | [] -> AllAlpha
                ///Check and return either AllDigit or Invalid
                let rec checkDigit lst =
                    match lst with
                    | ch :: tail when List.contains ch ['0'..'9'] -> checkDigit tail
                    | ch :: tail -> Invalid
                    | [] -> AllDigit
                ///Check everything
                let check lst =
                    match lst with
                    | ch :: tail when List.contains ch (['a'..'z'] @ ['A'..'Z']) -> checkAlpha tail
                    | ch :: tail when List.contains ch ['0'..'9'] -> checkDigit tail
                    | ch :: tail -> Invalid
                    | [] -> Invalid
                check charList

            ///split input string into array of lines
            let splitStr (str:string) = str.Split([|'\n';'\f';'\t';'\r';' '|])|>List.ofArray 

            ///tokenise instruction
            let tokenise str =
                let strList = splitStr str
                let rec tokenise' strList =
                    match strList with
                    | empty :: tail when empty = "" -> tokenise' tail
                    | anyStr :: tail -> anyStr :: (tokenise' tail)
                    | [] -> []
                tokenise' strList

            ///Parse the tokens. Either return Parse("Invalid") or other Command data type including READ, WRITE and ASSIGN
            let parse (strList: string list) = 

                ///check the 2nd argument of READ
                let checkRead (strList: string list) = 
                    match strList.[1] with
                    | AllAlpha(fileName) -> Read(strList.[1])
                    | _ -> Parse("invalid")
                    
                ///check the 2nd & 3rd arguments of WRITE
                let checkWrite (strList: string list) = 
                    match (strList.[1], strList.[2]) with
                    | (AllAlpha(fileName),AllDigit(fileValue)) -> Write((strList.[1], strList.[2] |> int))
                    | _ -> Parse("invalid")

                ///check the 2nd & 3rd arguments of ASSIGN
                let checkAssign (strList: string list) = 
                    match (strList.[1], strList.[2]) with
                    | (AllAlpha(file1),AllAlpha(file2)) -> Assign((strList.[1], strList.[2]))
                    | _ -> Parse("invalid")
                       
                match strList with
                | "READ" :: _ when strList.Length = 2 -> checkRead strList
                | "WRITE" :: _ when strList.Length = 3 -> checkWrite strList
                | "ASSIGN" :: _ when strList.Length = 3 -> checkAssign strList
                | _ :: _ -> Parse("invalid")
                | [] -> Parse("invalid")
            
            ///Evaluate the parsed expression
            let evaluate parseResult = 
                match parseResult with
                | Parse(error) -> ParseError
                | Read(command) -> environment (Read(command)) 
                | Write(command) -> environment (Write(command)) 
                | Assign(command) -> environment (Assign(command))

            tokenise str |> parse |> evaluate



        //
        // ************************************************************************
        //
        match command with
        | Read (fileName) -> readFile fileName
        | Write (fileName, writeValue) -> writeFile (fileName,writeValue)
        | Assign (file1,file2) -> assignFile (file1,file2)
        | Parse (instruction) -> parseInstruction instruction
     

    //environment 


    printfn "Read('James') = %A, \n state = %A" (environment (Read("James"))) state
    printfn "Write('James',10) = %A, \n state = %A" (environment (Write("James",10))) state
    printfn "Read('James') = %A, \n state = %A" (environment (Read("James"))) state
    printfn "Write('James2',5) = %A, \n state = %A" (environment (Write("James2",5))) state
    printfn "Assign James2 to James = %A, \n state = %A" (environment (Assign("James","James2"))) state
    printfn "Read('James') = %A, \n state = %A" (environment (Read("James"))) state
    printfn "Parse (Read) = %A, \n state = %A" (environment (Parse("\t \n  READ \n   James \t"))) state
    printfn "Parse (Write) = %A, \n state = %A" (environment (Parse("\t \n  WRITE \n   Kevin 10 \t"))) state
    printfn "Parse (Write) = %A, \n state = %A" (environment (Parse("\t \n  WRITE \n   Kev2in 10 \t"))) state
    printfn "Parse (Write) = %A, \n state = %A" (environment (Parse("\t \n  WRITE \n   Kevin James \t"))) state



makeEnvironment ()




