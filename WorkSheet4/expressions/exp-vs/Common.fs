namespace Expr
module Common =

    open System

    //
    // Types of Tokens and AST expressions
    //
    type Token = 
        | TokName of string // alphanumeric name: Func1
        | TokStrLit of string // string literal "Hello world"
        | TokIntLit of int // integer literal 1234 (only allow positive literals but unary - can be used to make them negative)
        | TokSpec of string // operators like "+", "*", "::", "NOT" but also keywords like "LET", "IF".
        | TokNull // representation of a Null value used for empty lists: a :: b :: c :: Null (like [] in F#)

    type AST = 
        | Apply of AST list // Apply[f;a;b;c] -> f a b c
        | Name of string // Built-in Combinator or let-defined function name
        | Literal of Token // 
        | NullExp
        | FNames of AST list // list of symbols found in LET definition: function + arg names
        | Lambda of Func : AST * Args: AST list * Body : AST * InExp : AST // Let definition and associated expression


    //
    // Coloured printing to a windows console
    //

    /// coloured printf (use e.g. c = System.ConsoleColor.Blue)
    let cprintf c fmt = 
        Printf.kprintf
            (fun s ->
                let old = Console.ForegroundColor
                try
                  Console.ForegroundColor <- c;
                  Console.Write s
                finally
                  Console.ForegroundColor <- old)
            fmt


    //
    // Code to implement semi-functional tracing
    //

    /// D.U. specifying all classes of trace printout
    type TraceKey =  
        | KString
        | KCharacter
        | KToken
        | KComment
        | KError

    /// Single message stored with key for later printing
    type TraceItem = TraceKey * string

    /// all trace messages so far
    type TraceInfo = TraceItem list

    let keyPrintColor = function
        | KString -> ConsoleColor.Cyan
        | KCharacter -> ConsoleColor.Blue
        | KToken -> ConsoleColor.Yellow
        | KComment -> ConsoleColor.Green
        | KError -> ConsoleColor.Red


    let formatTrace (ti: TraceInfo) =
        ti
        |> List.map (fun (k, inf) -> sprintf "%A:%s" k inf)
        |> String.concat "\n"

    let printTrace (ti:TraceInfo) =
        ti
        |> List.rev
        |> List.iter (fun (k, inf) ->
                         cprintf (keyPrintColor k) "%A:" k
                         printf "%s\n" inf)


    /// Wraps pFun providing traceFunc to it as 1st parameter to do tracing 
    let traceWrap pFun (pIn, traceInfo) =
        let mutable newTraceItems: TraceItem list = [] // accumulates trace output
        let traceOutput key str =
            newTraceItems <- (key,str) :: newTraceItems
        try // catch any exceptions raised by the wrapped code
           let pOut = pFun traceOutput pIn
           (pOut, List.append newTraceItems traceInfo) // normal return
        with e -> // if there is an exception print all trace info
            let tr = List.append newTraceItems traceInfo
            cprintf System.ConsoleColor.Red "Unexpected error:\n"
            cprintf System.ConsoleColor.Red "%s\n" (e.ToString())
            printTrace tr
            cprintf ConsoleColor.Red "Press any key to exit"
            System.Console.ReadKey() |> ignore // wait before exit
            reraise()      

    /// return traceItem messages as filtered by keyList
    let ppT keyList ( _, itemList) =
        List.filter (fun (k,_) -> List.contains k keyList) itemList


    let rec listPair = function
        | a :: b :: t -> (a,b) :: listPair t
        | [a] -> [(a,a)]
        | [] -> []
