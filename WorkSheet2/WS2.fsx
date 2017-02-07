let lis = [1..20000]
let f = fun x -> x*x


// ************************** recursion that needs lots of stack memory **************************
let rec map1 f lis= 
 match lis with
 | [] -> []
 | h::tl->(f h)::map1 f tl
let a = map1 f lis
printfn "map1 lis: %A" a


// ************************** tail recursion **************************
let map2a f lis=
  let rec map2a' ret lis =
    match lis with
    | [] -> ret
    | h :: tl -> map2a' (f h :: ret) tl
  map2a' [] lis
printfn "map2a lis: %A" (map2a f lis)


// ************************** tail recursion with output in correct order **************************
let map2b f lis =
   let rec map2c' ret lis =
      match lis with
      | [] -> ret
      | h :: tl -> map2c' (List.append ret [f h]) tl 
      //map2b Θ(n2). Each list element must be appended to the end of the list taking Θ(n). 
      //There are n list items. Hence overall time is Θ(n2).
   map2c' [] lis
printfn "map2b lis: %A" (map2b f lis)


// ************************** a better way **************************
let map2c f lis =
   let rec map2c' ret lis =
      match lis with
      | [] -> List.rev ret //returns elements in reverse order
      | h :: tl -> map2c' (f h :: ret) tl
      //map2c Θ(n). the List.rev function is Θ(n)
      //and this happens once for the whole list, therefore not affecting overall speed.
   map2c' [] lis
printfn "map2c lis: %A" (map2c f lis)


// ************************** compare time order **************************
//0.011 sec
#time "on"
let lis = [1..20000]
let f = fun x -> x*x
let rec map1 f lis= 
 match lis with
 | [] -> []
 | h::tl -> (f h)::map1 f tl
printfn "map1 lis: %A" (map1 f lis)

//0.041 sec because List.rev ret. without that is 0.002 sec
#time "on"
let lis = [1..20000]
let f = fun x -> x*x
let mapTailRecRev f lis = 
  let rec map2c' ret lis =
    match lis with
    | [] -> List.rev ret 
    | h :: tl -> map2c' (f h :: ret) tl
  map2c' [] lis
printfn "mapTailRecRev lis: %A" (mapTailRecRev f lis)

// 5.741 sec
#time "on"
let lis = [1..20000]
let f = fun x -> x*x
let mapTailRecAppend f lis = 
  let rec map2a' ret lis =
    match lis with
    | [] -> ret
    | h :: tl -> map2a' (List.append ret [f h]) tl
  map2a' [] lis
printfn "mapTailRecAppend lis: %A" (mapTailRecAppend f lis)


// ************************** List.fold **************************
//List.fold f init [a1;a2;a3...an] equivalent to List.reduce f [a1;a2..an] + f init
// e.g.
let plus = (+) // NB this is a "functionised operator" 
let adder1 = List.fold plus 0
let adder2 = List.reduce plus


// ************************** closure & local variable n **************************
let n = 17
let makeIncrementer x =
  let n = 2*x
  let incr i = i+n
  incr // incr is a closure with bound n
let f1 = makeIncrementer 10
let f2 = makeIncrementer 20
printfn "n1 = %d" n //print 17
let n = 3
printfn "f1 1 = %d" (f1 1) //print 21
printfn "f2 3 = %d" (f2 3) //print 43
printfn "n2 = %d" n //print 3


//******** Implement a function using List.fold that returns the length of a list. (Equivalent to List.length). ******
let lst = [1..10]
printfn"list.length = %A" (List.fold (fun n x -> n+1) 0 lst)


//************************** Implement list reverse using List.fold 
//and ***the add the head onto the front of an accumulator parameter** method used in rev1 in (@?rev).**************
let lst = [1..10]
printfn"list.rev = %A" (List.fold (fun n l-> l :: n) [] lst)


//*** Implement a function listSumOfSquares using List.fold that returns the sum of squares of elements in an int list***
let lst = [1..10]
printfn"ListSunOfSquares = %A" (List.fold (fun x l -> x + l*l) 0 lst) 
printfn"ListSunOfSquares = %A" (List.sumBy (fun x->x*x) lst)
//************************* What needs to change for it to work with float lists?**********************
let lst = [1.0..10.0]
printfn"ListSunOfSquares = %A" (List.fold (fun x l -> x + l*l) 0.0 lst) 


// **************************** implement List.filter using List.collect ******************************
let f x = x<10
let lst = [1..10]
printfn "List.filter x=x*x = %A" (List.collect (fun x -> if (f x) then [x] else []) lst)


// ******************* Using List.indexed and List.filter and the remainder operator % *********************
// ******************* write a function listOdd that returns the odd elements of a list *******************
let lst = [1..10]
printfn "List.indexed = %A" (List.indexed lst)
let listOdd lst = 
    lst 
    |> List.indexed
    |> List.filter (fun (n,_) -> n % 2 = 0)
    |> List.map (fun (n,x) -> x)
printfn "listOdd = %A" (listOdd lst)
// ******************* write a function listEven *******************
let listEven = List.indexed >> List.collect (fun (n,x) -> if n % 2 = 1 then [x] else [])
printfn "listEven = %A" (listEven lst)


// ******************** Implement ListOdd using List.chunkBySize ********************
// this method avoids using complicated indexing as shown above
let lst = [1..9]
let listOdd lst=
  lst
  |> List.chunkBySize 2
  |> List.collect (fun chunk ->
        match chunk with
        | [a;b] -> [b]
        | [a] -> []
        | x -> failwithf "zero size chunk should never happen! %A" x ) 
        //Never leave "pattern match incomplete" warnings without an informative error message
printfn "listOdd = %A" (listOdd lst)


// ******************** Using listOdd and listEven and List.zip write a function listPair ********************
// List.zip Combines the two lists into a list of pairs. The two lists must have equal lengths.
let listOdd = List.indexed >> List.collect (fun (n,x) -> if n % 2 = 0 then [x] else [])
let listEven = List.indexed >> List.collect (fun (n,x) -> if n % 2 = 1 then [x] else [])
let listPair lst = 
  let a = listOdd lst
  let b = 
      match (List.length lst) % 2 = 0 with
      |true -> listEven lst
      |false -> listEven lst @ [List.last lst]
  List.zip a b
let lst = [1..9]
printfn "listPair = %A" (listPair lst)

// *************************** Implement listPair using List.chunkBySize, List.map ***************************
// ******************* match patterns can be fixed length lists, tuples, or contain wildcards: ******************* 
let listPair x = 
    x 
    |> List.chunkBySize 2
    |> List.map (fun chunk ->
        match chunk with
        | [a;b] -> (a,b)
        | [a] -> (a,a)
        | x -> failwithf "zero size chunk should never happen! %A" x )
let lst1 = [1..9]
let lst2 = [(1,2);(3,4);(5,6)]
let lst3 = [(1,2)]
printfn "listPair lst1 = %A" (listPair lst1)
printfn "listPair lst2 = %A" (listPair lst2)
printfn "listPair lst3 = %A" (listPair lst3)


// ************ Implement listPair using a recursive function (which need not be tail recursive) ***************
let rec listPair x = //note to add rec for recursive func
  match x with
  | a::b::tl -> (a,b) :: (listPair tl) 
  | [a] -> [(a,a)]
  | [] -> [] 
let lst = [1..9]
printfn "listPair lst = %A" (listPair lst)

// **************** Using List.groupBy, List.map and List.length Implement a function listHistogram ****~************
// e.g. listHistogram [ 8; 1; 2; 7; 2; 1 ] = [ (1,2) ; (2,2) ; (7,1) ; (8,1) ]
printfn "List.groupby lst = %A" (List.groupBy id [1;2;3;2]) //[(1, [1]); (2, [2; 2]); (3, [3])]
let listHistogram lst =
  lst
  |> List.groupBy id
  |> List.map (fun (a,b) -> (a,List.length b)) 
printfn "listHistogram lst = %A " (listHistogram lst)



// ***************************************** Exercise 3: Taylor Series Revisited *************************************
// ***************************************** Exercise 3: Taylor Series Revisited *************************************
// ***************************************** Exercise 3: Taylor Series Revisited *************************************
// ***************************************** Exercise 3: Taylor Series Revisited *************************************


// ***** implement using List.fold over the list [1..N], where the fold state is a tuple (sumOfPrevTerms,nextTerm) *****
let sinSeries x n= 
  let folder (sumOfPrevTerms, nextTerm) n =
    let a = 2.0*(float n)
    (sumOfPrevTerms+nextTerm, -nextTerm*x*x/(a*(a+1.0))) 
  List.fold folder (0.0,x) [1.0..(float n)]
  |> fun (s,t) -> s+t
printfn "sinSeries 1 100 = %A" (sinSeries 1.0 100)


//*************** using a tail recursive function with three parameters: i, sumOfPrevTerms and nextTerm ****************
let rec sinSeries x n = 
  let rec sinSeries' sumOfPrevTerms nextTerm i =
    let y = 2.0*(float i)
    match i=n+1 with
    | true -> sumOfPrevTerms + nextTerm
    | false -> sinSeries' (sumOfPrevTerms+nextTerm) (-nextTerm*x*x/(y*(y+1.0))) (i+1)
  sinSeries' 0.0 x 1
printfn "sinSeries 1 100 = %A" (sinSeries 1.0 100)


//**************************** Suppose you had to write a lot of Taylor series functions *******************************
/// fold next term of series using (sum of prev terms, last Term) as state
let foldSum termGen initTerm lst =
   let summer = fun (s, t) n -> (s+t, termGen t n) // sums the terms
   List.fold summer (0.0, initTerm) lst
   |> fun (s,t) -> s+t

let taylorSine n x =
    /// generate next Term from previous one and term order
    let termGen prevTerm n = 
        -prevTerm*x*x/(n*(n-1.0))  ///note the optimisation here
    foldSum termGen x [3.0..2.0..n] // sum first n terms. 
printfn "sinSeries 1 100 = %A" (taylorSine 100.0 1.0)


// *************************************** Rewrite lines below using |> instead of >> **********************************
//Array.indexed
//>> Array.collect (fun (i,b)-> if i >= 2 && b then [|i|] else [||])


// ************** simplify this line --- [|2..maxPrimeFactor|] |> Array.iter (fun p -> removeMultiples p) **************
// [|2..maxPrimeFactor|] |> Array.iter removeMultiples p


// ****************************************** example function using array *********************************************
open System // for Math.Ceiling 
let primes upTo =
  // array of booleans. Index 0 and 1 are not used.
  let primeSieve = Array.create (upTo+1) true
  // Largest prime factor we need to sieve by
  let maxPrimeFactor = upTo |> float |> sqrt |> Math.Ceiling |> int // Return array of primes from the current value of primeSieve 
  let sieveToPrimes =
    Array.indexed
    >> Array.collect (fun (i,b)-> if i >= 2 && b then [|i|] else [||])
  // set elements with indexes multiples of p to false in primeSieve array. 
  let removeMultiples p =
    if primeSieve.[p] 
    then [| p*p..p..upTo|]
      |> Array.iter (fun p -> primeSieve.[p] <- false)
  [|2..maxPrimeFactor|] |> Array.iter (fun p -> removeMultiples p)// do all the prime factor sieving 
  sieveToPrimes primeSieve
printfn "primes upTo 100 = %A" (primes 100)


// ****************************************** rewrite example function using array *************************************
/// array of all primes up to upto
let primes upTo =
   // Largest prime factor we need to sieve by
   let maxPrimeFactor = upTo |> float |> sqrt |> Math.Ceiling |> int
   /// Return array of primes from a sieve of bool true for prime indices
   let sieveToPrimes = 
      Array.indexed
      >> Array.collect (fun (i,b)-> if i >= 2 && b then [|i|] else [||]) 
   /// set elements in sieve with with indexes multiples of p to false.
   /// type constraint on primeSieve is needed since inference does not disambiguate
   let removeMultiples (p:int) (primeSieve:bool []) =
      if primeSieve.[p]
      then [| p*p..p..upTo|] 
           |> Array.iter (fun p -> primeSieve.[p] <- false)
   // set elements in sieve with non-prime indexes to false
   let removeNonPrimes sieve =
       [|2..maxPrimeFactor|] 
       |> Array.iter (fun p -> removeMultiples p sieve)// do all the prime factor sieving
       sieve   
   Array.create (upTo+1) true // array of booleans
   |> removeNonPrimes
   |> sieveToPrimes
printfn "primes upTo 100 = %A" (primes 100)


// ****************************************** rewrite function using list **********************************************
let rec filterNonPrimes lst = 
  match lst with
  | (p::xs) -> 
    let isDivisibleByP x = x % p > 0 /// note here we break up the expression to improve readability
    p :: filterNonPrimes (List.filter isDivisibleByP xs) 
  | [] -> []
let primes n = filterNonPrimes [2..n]
printfn "%A" (primes 50)


// ********************************* Transform the above func into tail recursive form *********************************
///functuin version - ***pattern matching expression**** List.map (fun x -> match x with | 1 -> "one" | _ -> "not one") [0;1;2;3;1]
///functuin version - ***pattern matching function**** List.map (function 1 -> "one" | _ -> "not one") [0;1;2;3;1] 
let filterNonPrimes n =
    let rec filterNonPrimes' primesSoFar = function // function == fun x->match x with
        | (p::xs) -> filterNonPrimes' (p :: primesSoFar) (List.filter (fun x -> x % p > 0) xs)
        | []      -> List.rev primesSoFar
    filterNonPrimes' [] [2..n]
printfn "filterNonPrimesTR [2..10] = %A" (filterNonPrimes (100))



// ************************************************ Maps and Memoise ***************************************************
let xm = Map ["first","the" ; "second","cat"]              /// type : Map<string,string>
let ym = Map ["cat",7 ; "the",3]                           /// type : Map<string,int>
let zm = Map [ [1;2;3],5 ]                                 /// type : Map<int list,int>
zm.[[1 ; 2 ; 3]] // look up key in zm map (returns 5)
let ym' = Map.add "fox" 2 ym // new map with ("fox",5) added to ym.
/// note that map is sorted
printfn "ym' = %A" ym'


// *********************************** obtain the list of all keys in a map m ******************************************
let ym = Map ["cat",7 ; "the",3]   
printfn "obtain the list of all keys in a map = %A" (ym |> Map.toList |> List.map fst) ///fst returns the first element of a tuple


// ******************************************  Find prime factors of integer n *****************************************
let rec primeFactors (n:int64) =
    let rec pf factLst c i =
        match i % c = 0L with
        | true -> c :: primeFactors (n/c)
        | false when c*c > n -> [n]
        | false when i = 1L -> []
        | _ -> pf factLst (c+1L) i
    pf ([]:int64 list) 2L (n:int64)
let a = int64 (268435399L)
printfn "primeFactors 10 = %A" (primeFactors a)


// ************************* Find prime factors of integer n - full tail recursive implementation **********************
let primeFactors (n:int64) =
    let rec pf factLst c i =
        match (i % c = 0L) && (i <> c) with
        | true -> pf factLst c (i/c)
        | false when c*c > n -> [n]
        | false when i = 1L -> factLst
        | false when i=c -> [c]
        | _ -> pf factLst (c+1L) i
    pf [] 2L (n:int64)
let a = int64 (2683239L)
printfn "primeFactors 10 = %A" (primeFactors a)


// ************************************************** memoise **************************************************
/// fast fetch
let memoise fn =
   let mutable cache = Map []
   fun x ->
      match Map.containsKey x cache with
      | true -> cache.[x] // return cached value
      | false -> let res = fn x // compute function
                 cache <- Map.add x res cache //store result in cache
                 res // return result

                