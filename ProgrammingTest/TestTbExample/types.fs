namespace TestCode

module Types =

    type TestAnswer = { 
            testQ1: unit -> int -> int 
            testQ2: unit -> float -> float -> float
            testQ3: unit -> (int -> int) -> int -> int
            }

    let noAnswer = fun () -> failwith "Answer not implemented"

    /// default answer with no question answers implemented
    let defaultAnswer = { 
        testQ1 = noAnswer
        testQ2 = noAnswer
        testQ3 = noAnswer
        }


