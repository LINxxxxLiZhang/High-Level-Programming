namespace TestCode

module Answer =
    open Types

    let q1Answer n = n // attempt at answering Q1

    let q3Answer = fun f x -> f (f x)

    /// Record containing answers. Fields not defined here will default to 'noAnswer'
    let theAnswer = 
        {
            Types.defaultAnswer with     
                testQ1 = fun () -> q1Answer
                // testQ2 = ...
                testQ3 = fun () -> q3Answer
        }
               
   
   

