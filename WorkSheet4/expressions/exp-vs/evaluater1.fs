namespace Expr

module Evaluater1 =
    open Common

    // A graph node
    type Data = 
        | DInt of int
        | DName of string
        | DCell of Hd : Loc * Tl : Loc

    // a mutable cell used so that graphs can be mutated during beta reduction
    and Loc = Data ref

    let DApply x y = ref (DCell(x, y))

    let (|R|) x = !x // active pattern which returns the contents of a Loc

    let rec makeHeap ast =
       match ast with
       | Literal (TokIntLit n) -> ref (DInt n)
       | Apply [ Name op ; lh ; rh ] -> DApply ( DApply (ref (DName op)) (makeHeap lh)) (makeHeap rh)

    let rec reduce g =
        match g with
        | R(DInt n) -> g
        | R(DCell(R (DCell(R op, x)) ,y)) -> 
            let (x1,y1) = !(reduce x), !(reduce y)
            let res = match op, x1, y1 with
                      | DName "*", DInt a, DInt b -> a * b
                      | DName "/", DInt a, DInt b -> a / b
                      | DName "+", DInt a, DInt b -> a + b
                      | DName "-", DInt a, DInt b -> a - b
                      | _ -> failwithf "Unexpected graph during reduction: %A" g
            g := res |> DInt // overwrite the op a b graph with the result
            g

    let evaluate ast =
        ast 
        |> makeHeap
        |> reduce
        |> function 
           | R (DInt n) -> n 
           | x -> failwithf "Reduced value is not a number: %A" x
    
    

