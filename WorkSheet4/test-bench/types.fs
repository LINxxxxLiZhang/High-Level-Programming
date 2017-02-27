namespace TestEnvt

module Types =
    type Command = 
        | Read of string 
        | Write of (string * int) 
        | Assign of (string * string)
        | Parse of (string)

    type Response =
        | VarValue of int // for commands that return data
        | ParseError // if command string is invalid
        | DataError // if the required data does not exist
        | OK // for valid commads that return no data

