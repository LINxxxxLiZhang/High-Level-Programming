namespace Expr

open Common

module Program =

    let src1 = "* / + ( // comment"
    let src2 = "11 * 120/(3*4)"



    let tokens = traceWrap Tokeniser.tokenise

    let parse = traceWrap Parser.parse

    let eval = traceWrap Evaluater.evaluate

    let testProgram src =
        (src, [])
        |> tokens
        |> traceWrap (Parser.parse >> Array.map)
        |> traceWrap (Evaluater.evaluate >> Array.map)

    [<EntryPoint>]

    let main argv =
        testProgram src2 |> (fun (v,t)-> printTrace t;  printfn "Value is:\n %A" v)
        printfn "Press any key to EXIT"
        System.Console.ReadKey() |> ignore
        0 // return an integer exit code
