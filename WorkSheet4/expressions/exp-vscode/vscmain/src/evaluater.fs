namespace Expr
module Evaluater =

    open Common

    //
    // Types representing a program as a mutable graph structure
    //
    // An data value
    type Data = 
        | DInt of int
        | DNull

    let failwithTrace tFun str = 
        tFun KError str
        failwithf "%s" str

    /// evaluate an AST returning the numeric value
    let rec eval trace ast =

        /// evaluate ast and return the value unpacked as an integer
        let evalInt ast = 
           match eval trace ast with
           | DInt n -> n
           | _ -> failwithTrace trace <| sprintf "%A found when numeric value expected in AST" ast

        /// active pattern matches operator opStr and returns opFun transformed so its two parameters are converted to int
        let (|BinOp|_|) opStr opFun  = function
           | Name x when x = opStr -> Some (fun a b -> opFun (evalInt a) (evalInt b))
           | _ -> None

        match ast with
        | Literal (TokIntLit n) -> DInt n
        | Apply [op ; e1 ; e2 ] -> 
            match op with
            | BinOp "+" (+) f | BinOp "-" (-) f | BinOp "*" (*) f | BinOp "/" (/) f -> DInt (f e1 e2)
            | _ -> failwithTrace trace <| sprintf "Unexpected binary operation %A" ast
        | _ -> failwithf "Unimplemented ast node %A" ast

    let evaluate trace x = 
        match eval trace x with 
        | DInt n -> n 
        | x -> failwithf "Evaluate results is not a number: %A" x