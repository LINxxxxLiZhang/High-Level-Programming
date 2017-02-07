
type Student = StudentId of string
type Ticks = T1 | T2 | T3 | T4
type Tests = Test1 | Test2
type Assessment = Tick of tck: Ticks * maxMark: int | Test of tst: Tests * maxMark: int
type MarkKind = Resit | Exception | Normal
type Mark = int * MarkKind
type Marking = Student * Mark * Assessment // one mark
type MCData = Set<Student*Assessment> // specifies students with MC
type Markings = Marking list
type TotalMark = ValidTotalMark of int | InvalidTotalMark of string



let totalMarks (student:Student) (mc: MCData) (markings:Markings) = 
        
    // check whether a student is MC
    // return true if is a student is MC, else false
    let isMCStudent (std:Student) (assessment:Assessment) (mc: MCData) = Set.contains (std,assessment) mc  


    // calculate the total marks
    let rec calculateTotalMarks (student:Student) (mc:MCData) (markings:Markings) = 
        match markings with 
        | h::tl -> 
            match h with
            | std,(mark,markKind),assessment when std = student -> // find student's marking
                // check if the student is MC
                match isMCStudent std assessment mc with
                //MC student
                | true ->  mark + calculateTotalMarks std mc tl
                //normal student
                | false -> 
                    match assessment with
                    | Test (x,maxMark) -> mark + calculateTotalMarks std mc tl
                    | Tick (x,maxMark) -> 
                        //check if the assessment is a resit
                        match markKind with
                        | Resit -> mark/2 + calculateTotalMarks std mc tl
                        | _ -> mark + calculateTotalMarks std mc tl
            | std,(mark,markKind),assessment when std <> student -> calculateTotalMarks student mc tl // keep searching for this student's marking
            | _,_,_ -> 0
        | [] -> 0

    /// output should be valid for all types of inputs (whether valid or invalid)
    // check input types. Return false if mark>maxMark. Return true if all input marks are valid
    let rec checkMarkValidity student mc markings = 
        match markings with
        | h::tl ->
            match h with
            | std,(mark,markKind),assessment when std = student -> // find student's marking
                match assessment with
                | Test (x,maxMark) when mark>maxMark -> false 
                | Tick (x,maxMark) when mark>maxMark -> false
                | _ -> true
            | std,(mark,markKind),assessment when std <> student -> checkMarkValidity student mc tl // keep searching for this student's marking
            | _,_,_ -> true
        | [] -> true

    // print result
    let printTotalMark student mc markings= 
        if  (checkMarkValidity student mc markings) then ValidTotalMark (calculateTotalMarks student mc markings)
        else InvalidTotalMark ("invalid input: mark cannot exceed the maximum mark")
            

    printTotalMark student mc markings

let student1 = StudentId ("James")
let maxMark1 = 50
let assessment1 = Tick (T5,maxMark1)
let mark1 =  (60,Resit)
let marking1 = (student1,mark1,assessment1)

let student2 = StudentId ("Tom")
let maxMark2 = 100
let assessment2 = Tick (T1,maxMark2)
let mark2 =  (70,Resit)
let marking2 = (student2,mark2,assessment2)

let mc = Set [(student1,assessment1);(student2,assessment2)]

let markings = [marking1;marking2]
printfn "Total mark of student %A is %A " student1 (totalMarks student1 mc markings)

