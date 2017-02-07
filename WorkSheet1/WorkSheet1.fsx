//Q3
//let pairs = [ (1,2) ; (1,3) ; (2,3); (3,4) ; (4,7) ]  
//let pythagTriples = 
// let makeTriple (a,b) = 
//  let square x=x*x
//  let p = square a - square b
//  let q = 2*a*b
//  let r = square a + square b
//  (p,q,r)
// List.map makeTriple pairs


//printfn "Triples are: %A" pythagTriples   //%A will print out any type in a sensible printable form.
//System.Console.ReadLine() // prevent the program from terminating


//exercise 1.2
//let intList = [1..5]
//let makePairs a0 = List.map (fun b -> (a0,b)) intList
//let printPair = makePairs 1
//printfn "Triples are: %A" printPair  //%A will print out any type in a sensible printable form.
//System.Console.ReadLine() // prevent the program from terminating


//let b1 = List.map (fun a -> [a ; a+1 ; a+2]) [10 ; 20] // [[10; 11; 12] ; [20; 21; 22]]
//let b2 = List.collect (fun a -> [a ; a+1 ; a+2]) [10 ; 20] // [10; 11; 12; 20; 21; 22]
//printfn "Triples are: %A" b1   //%A will print out any type in a sensible printable form.
//printfn "Triples are: %A" b2   //%A will print out any type in a sensible printable form. 


//let retainPositive lst = List.collect (fun a -> if a > 0 then [a] else []) lst
//let input = [ 1; -2; -4; 0; 11]
//let output = retainPositive input // should return [1; 11]
//printfn "result of : %A  is : %A" input output   //%A will print out any type in a sensible printable form.


//let intList = [1..5]
//let makePairs a0 = List.map (fun b -> (a0,b)) intList 
//let pairList = List.collect makePairs intList
//let a = makePairs 1
//printfn "makePairs : %A" a
//printfn "pairList : %A" pairList


// ... generalise the above code
//let makePairs a0 = fun lst -> List.map (fun b -> (a0,b)) lst
//let pairList = fun lst -> List.collect (fun a0 -> makePairs a0 lst) lst
//let a = makePairs 1 [1..5]
//let b = pairList [1..5]
//printfn "makePairs : %A" a
//printfn "pairList : %A" b


//.. the above function has only one parameter.. now write a function of two parameters
//let makePairs lst a0 = List.map (fun b -> (a0,b) ) lst
//let pairList lst = List.collect (makePairs lst) lst //let pairList lst = List.collect (fun a0 -> makePairs lst a0) lst
//let a = makePairs [1..5] 1
//let b = pairList [1..3]
//printfn "makePairs: %A" a
//printfn "pairList : %A" b


//.. alternatively
//.. Note how, because the lst parameter is partially applied to makePairs inside pairList, 
//.. it must be written as the first parameter to makePairs. 
//.. Swapping these two parameters is possible but makes pairList longer:
//let makePairs a0 lst = List.map (fun b -> (a0,b) ) lst
//let pairList lst = List.collect (fun a0 -> makePairs a0 lst) lst
//let a = makePairs [1..5] 1
//let b = pairList [1..3]
//printfn "makePairs: %A" a
//printfn "pairList : %A" b
//.. The lesson here, when writing multiple parameter functions, is to think about which parameter is 
//.. most likely to be partially applied and write this first in the definition.


//let intList = [1..5]
//let makePairs lst = fun a -> List.map (fun x -> (a,x)) lst
//let pairList = List.collect (makePairs intList) intList //change List.collect to List.map would result in change of type 
//                                                        //from list of tuples to list of list of tuples
////let makeTriple (a,b) = ... //as before
////let tripleList = List.map makeTriple pairList
//let out1 = makePairs [1..5] 1
//let out2 = pairList
//printfn "makePairs: %A" out1
//printfn "pairList: %A" out2


// this code is clever but not as easy to read as when makePairs is named
//let pairListFn = fun lst -> List.collect (fun a -> List.map (fun x -> (a,x)) lst) lst
//let makeTriple (a,b) =
//    let sq x = x*x
//    (sq a - sq b, 2*a*b, sq a + sq b)
//let tripleList = List.map makeTriple (pairListFn [1..5])
//printfn "Triples are:%A" tripleList
//System.Console.ReadLine() // prevent the program from terminating


//Q10. Rewrite let makePairs a b = (a,b) without changing its meaning as let makePairs1 =... 
//by using an anonymous function. Check that makePairs1 works as expected.
//let makePairs1 =  fun a0 -> fun lst -> List.map (fun b -> (a0,b) ) lst 
//let out1 = makePairs1 1 [1..5] 
//printfn "makePairs1 : %A" out1


//Q11
//let pairListFn = fun lst -> List.collect (fun a -> List.map (fun x -> (a,x)) lst) lst
////Q12 delete duplicated pairs
//let deleteBadPairs x = List.filter (fun (a,b)-> a < b) x
//let pairList = deleteBadPairs (pairListFn [1..5])
//printfn "pairList : %A" pairList
//let makeTriple (a,b) =
//    let sq x = x*x
//    (sq a - sq b, 2*a*b, sq a + sq b)
//let multiList pl = List.map (fun p -> (p, makeTriple p)) pl
//let printMultiList (p,tr) = printfn "Pair %A generates triple %A" p tr
//List.map printMultiList (multiList pairList)




////Q13 rewrite a program into pipeline format
//let pairList = List.collect (fun a -> List.map (fun b -> (a,b)) [1..5]) [1..5]
//let makeTriple (a,b) =
//    let sq x = x*x
//    (sq a - sq b, 2*a*b, sq a + sq b)
//let tripleList = List.map makeTriple pairList
//printfn "Triples are:%A" tripleList

//let PrintTripleList =
//    let makeTriple (a,b) =
//        let sq x = x*x
//        (sq a - sq b, 2*a*b, sq a + sq b)
//    [1..2]
//    |> List.collect (fun a -> List.map (fun b -> (a,b)) [1..5])
//    |> List.map makeTriple
//    |> List.map (fun tLst -> printfn "Triples are:%A" tLst)



// exercise 1.4
// Q14
let raggedList n = [1.0..(float n)] |> List.map (fun i -> [1.0..i])
printfn "raggedList : %A" (raggedList 5)



//Q15 
//write a function that returns a 3-tuple (average of x, variance of x, x) when give a list x.
//test this by applying it to every row of raggedList 5.
//let stats lst =
//    let n = float (List.length lst)
//    let av = (List.reduce (+) lst) / n
//    let lstSquareSum = lst |> List.map (fun x->x*x) |> List.reduce (+)
//    ( av, lstSquareSum/n - av*av, lst)

//5 |> raggedList |> List.map stats |> printfn "results : %A"
//method 2
//let stats x = 
//    let count = float (List.length x)
//    let average = (List.reduce (+) x) / count
//    let var = x |> List.map (fun x->(x-average)*(x-average)) |> List.reduce (+)
//    (average, var/count, x)

//5 |> raggedList |> List.map stats |> printfn "results: %A"



//%Q16
// simplify Q15 stats using List.sumBy
//let stats lst =
//    let n = float (List.length lst)
//    let av = (List.reduce (+) lst) / n
//    let lstSquareSum = lst |> List.sumBy (fun x->x*x)
//    ( av, lstSquareSum/n - av*av, lst)

//5 |> raggedList |> List.map stats |> printfn "results : %A"
//method 2
let stats x = 
    let count = float (List.length x)
    let average = (List.reduce (+) x) / count
    let var = x |> List.sumBy (fun x->(x-average)*(x-average))
    (average, var/count, x)

5 |> raggedList |> List.map stats |> printfn "results: %A"