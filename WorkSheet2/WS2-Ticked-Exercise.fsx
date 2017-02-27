/// This will find, for each number in the input list, the corresponding largest prime factor.
/// The output list is of the largest prime factors in the corresponding same order. 
/// The code should work efficiently for long lists.

let findLargestPrimeFactor (inLst:int64 list) :int64 list =
    let rec find (p:int64) (x:int64) =
        match (x%p = 0L) && (x>p) with
        | true -> find p (x/p)
        | false when p*p>x -> x
        | false -> find (p+1L) x 

    inLst |> List.map (fun x -> find 2L x)
let a = [13L; 15L; 268435459L; 1099511627689L; 26843L; 1L; 2L; 3L; 21L]
printfn "findLargestPrimeFactor = %A" (findLargestPrimeFactor a)
