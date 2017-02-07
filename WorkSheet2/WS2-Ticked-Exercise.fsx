/// This will find, for each number in the input list, the corresponding largest prime factor.
/// The output list is of the largest prime factors in the corresponding same order. 
/// The code should work efficiently for long lists.

let findLargestPrimeFactor (lst:int64 list)= //int64 list -> int64 list

    let rec findLargestPrimeFactorTR' p (input:int64) =
        match (input % p = 0L) && (input > p) with
        | true -> findLargestPrimeFactorTR' p (input/p)     
        | false when (p*p > input) -> input      ///optimisation 1
        | false when (input <= p) -> p           ///optimisation 2
        | _ -> findLargestPrimeFactorTR' (p+1L) input
    lst |> List.map (fun x -> findLargestPrimeFactorTR' 2L x)

/// Note that list does not support larger literals, so for testing I prefer using array for larger literals then call Array.ToList 
let a = [|13L; 15L; 268435459L; 1099511627689L; 26843L; 1L; 2L; 3L; 21L|]
printfn "findLargestPrimeFactor = %A" (findLargestPrimeFactor (Array.toList a))