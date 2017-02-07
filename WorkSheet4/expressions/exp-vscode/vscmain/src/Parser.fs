namespace Expr
module Parser =

    open Common

    //
    //-----------------------------------Parser----------------------------------------/
    //

    /// parse tokens using trace for tracing and returnung the AST
    let parse trace (tokens : Token list) =

        /// Match and return a bracketed expression paired with remaining tokens
        let rec (|PITEM|_|) (tokens : Token list) =
            match tokens with
            | TokSpec "(" :: PMULTERM (term, (TokSpec ")" :: r)) -> Some (term, r)
            | TokIntLit n :: r  -> Some ((Literal (TokIntLit n)), r)
            | _ -> None

        /// match and return a multiplicative expression paired with remaining tokens
        and (|PMULTERM|_|) (tokens : Token list) =
            match tokens with
            | PITEM (leftItem, r1) ->
                match r1 with
                | TokSpec op :: r2 when List.contains op ["/" ; "*"] -> 
                    match r2 with
                    | PMULTERM (rightExp,r3) -> Some (Apply [ Name op ; leftItem ; rightExp],r3)
                    | _ -> failwithf "Multiplicative expression expected at %A" r2
                | r -> Some(leftItem , r)
            | r -> failwithf "No item found when one expected at %A" r

        match tokens with
        | PMULTERM (exp,[]) -> exp
        | r -> failwithf "parse failed %A" r
        
