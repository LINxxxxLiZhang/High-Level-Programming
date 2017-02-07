namespace Expr
module Tokeniser =

    open Common

    //
    // Tokenise Module
    //
    //
    // Functions to convert between string and char list types
    //

    /// convert string into char list
    let explode (str : string) = str |> List.ofSeq

    /// convert char list into string
    let implode (x : char list) = x |> System.String.Concat

    let opList = [ "+" ; "-" ; "/" ; "*" ; "(" ; ")"]

    //
    // Useful predicates on characters
    //

    /// character is white sppace
    let isWhiteSpace (c : char) = List.contains c [ ' '; '\n'; '\t'; '\r'; '\f' ]

    /// charater is new line
    let isNewLine (c : char) = List.contains c [ '\n'; '\f'; '\r' ]

    /// character is alphabetic
    let isAlpha (c : char) = List.contains c ([ 'a'..'z' ] @ [ 'A'..'Z' ])

    /// character is a decimal digit
    let isDigit (c : char) = List.contains c [ '0'..'9' ]

    /// character is alphanumeic (allowed in symbol)
    let isAlphaNum (c : char) = isAlpha c || isDigit c

    /// character op is in list lst
    let isOpInList (op: char) lst = List.contains op lst

    /// true if all the characters in string str match a starting prefix of character list x
    let rec charListStartsWith (x : char list) (str : string) = 
        let removeFirstChar (s:string) = 
            seq { for n in [1..s.Length-1] do yield str.[n]} 
            |> System.String.Concat
        match x with
        | _ when str = "" -> true // empty string will match anything
        | ch :: r when str.[0] = ch -> charListStartsWith r (removeFirstChar str) // if first char matches check the rest
        | _ -> false // This must be a mismatch

    //
    // Tokeniser
    // 
    let (|OpMatch|_|) cLst = 
        List.tryFind (charListStartsWith cLst) opList 
        |> Option.map (fun op -> TokSpec op, List.skip op.Length cLst)

    // return array of lines of lists of tokens, given lines in src
    let tokenise trace (src:string) = 
        let trTok tok = (trace KToken <| sprintf "Token %A" tok) ; tok // trace output for token
        let trInp s = (trace KString <| sprintf "Input %s" src) ; s // trace output for input line
        let trComment() = trace KComment "Skipping Comment" // trace output for comment
        let rec tokenise1 lst =
            trace KCharacter <| sprintf "%A" lst         
            match lst with
            | ch :: r when isWhiteSpace ch -> tokenise1 r
            | [] -> []
            | '/' :: '/' :: _ -> trComment() ; tokenise1 (List.skipWhile (isNewLine >> not) lst)
            | OpMatch(t, r) -> (trTok t) :: tokenise1 r
            | ch :: _ as toks -> failwithf "Unrecognised character '%A' found in tokenize input%A" ch toks
        src.Split([| '\n'; '\r' ; '\f'|]) // split input string into array of lines
        |> Array.map (trInp >> explode >> tokenise1)

