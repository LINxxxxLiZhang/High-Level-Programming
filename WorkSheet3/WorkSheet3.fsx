type Student = StudentId of string
type Ticks = T1 | T2 | T3 | T4
type Tests = Test1 | Test2
type Assessment = Tick of tck: Ticks * maxMark: int | Test of tst: Tests * maxMark: int
type MarkKind = Resit | Exception | Normal
type Mark = int * MarkKind
type Marking = Student * Mark * Assessment // one mark
type MCData = Set<Student*Assessment> // specifies students with MC
type Markings = Marking list
type TotalMark = 
    |Total of float
    |Error of (Marking * string) list

/// This function calculates the total marks of a student, given a list of markings of different students
/// If the assessment that the student took is credited for MC, the student won't be marked down even if the assessment is a resit
/// The function returns error type if:
/// ---- (1) the assessment is a test but the mark kind of the mark is resit.
/// ---- (2) the mark is greater than the maximum mark limit 
let totalMarks (student:Student) (mc:MCData) (markings:Markings) :TotalMark =

    ///collect all the marks of the student
    let studentMarks (std:Student) (mkings:Markings) :Markings =
        mkings |> List.filter (fun (s,_,_) -> s=std)

    /// find invalid marks
    let badMark (mking:Marking) :(Marking*string) list=
        let _,(n,mk),ass = mking
        match mk,ass with
        // the assessment is a test but the mark kind of the mark is resit
        | Resit _, Test _ -> [mking,"The mark kind of a test assessment cannot be 'resit'"]
        // the mark is greater than the maximum mark limit
        | _, Tick(_,mm) | _, Test(_,mm) when n>mm -> [mking,"Mark cannot exceedo the maximum mark limit"]
        | _ -> []
    
    /// calculate the actual mark of each marking
    let calculateMark (mking:Marking) =
        let std,(n,mk),ass = mking
        match mk, Set.contains (std,ass) mc with // the second element checks if the student is credited for MC for this module
        | Resit, true -> (float n)/2.0
        | _ -> float n

    match studentMarks student markings |> List.collect badMark with
    | [] -> //no error, start calculating the total mark
        studentMarks student markings
        |> List.map (fun mking -> calculateMark mking)
        |> List.sum
        |> Total
    | x -> Error(x)

