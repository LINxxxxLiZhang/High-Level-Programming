namespace TestCode

module Answer =
    open Model.Types
    //-----------------Types and values from Model.Types-------------------------
    (*
    type Month = 
        | January 
        | February 
        | March 
        | April 
        | May 
        | June 
        | July 
        | August 
        | September 
        | October 
        | November 
        | December

    type Station = | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10

    type Time = {
        day: int // calendar day of month, starting from 1
        month: Month
        year: int // year (e.g. 1998)
        }

    type TimeOrder = | After | Before | Equal

    type WeatherData = { temperature: float option;  time: Time ; station: Station}

    let monthOrder = [January;February;March;April;May;June;July;August;September;October;November; December]

    type TestResults = {
        Q1amInt: Unit-> Month -> int
        Q1bdaysOfMonth : Unit -> int -> Month -> int
        Q2timeToYearEnd: Unit -> Time->int
        Q3daysInYear : Unit ->  int -> int
        Q4atimeOrder: Unit -> int -> int -> (Unit->TimeOrder) -> TimeOrder
        Q4btimeCompare : Unit -> Time -> Time -> TimeOrder
        Q5timeDiff : Unit -> Time -> Time -> int
        Q6astationReadings: Unit -> WeatherData list -> Map<Station,WeatherData list>
        Q6byearReadings: Unit -> WeatherData list -> Map<int,WeatherData list>
        Q6cmonthReadings: Unit -> WeatherData list -> Map<Month,WeatherData list>
        Q7maxDiffByMonthYearStation: Unit -> WeatherData list -> int option   
    }
    *)



    /// number of Month (January = 1, December = 12)
    /// m: Month (month)
    let mInt (m:Month) : int =
        match m with
        | January -> 1
        | February -> 2
        | March -> 3
        | April -> 4
        | May -> 5
        | June -> 6
        | July -> 7
        | August ->8
        | September -> 9
        | October -> 10
        | November -> 11
        | December -> 12
    
    /// number of days in month
    /// month: Month (month)
    /// year: int (year)
    /// Sept Apr Jun Nov = 30, Feb = 29 (year divisible by 4) or 28. All others 31
    let daysOfMonth (year:int) (month:Month) : int =
        match (year%4,month) with
        | _,January | _, March | _,May | _,July | _,August | _,October | _,December  -> 31
        | _,April | _,June| _,November |_,September-> 30
        | 0,February -> 29
        | _, February -> 28

    /// number of days to end of year from Time value
    /// Dec 31 = 1, Jan 1 = 355 or 356
    /// tim: Time time value
    let timeToYearEnd (tim:Time) : int = 
        let rec sumDays time =
            match mInt time.month with
            | 12 -> daysOfMonth time.year December
            | 11 -> daysOfMonth time.year November + sumDays {time with month = December}
            | 10 -> daysOfMonth time.year October + sumDays {time with month = November}
            | 9 -> daysOfMonth time.year September + sumDays {time with month = October}
            | 8 -> daysOfMonth time.year August + sumDays {time with month = September}
            | 7 -> daysOfMonth time.year July + sumDays {time with month = August}
            | 6 -> daysOfMonth time.year June + sumDays {time with month = July}
            | 5 -> daysOfMonth time.year May + sumDays {time with month = June}
            | 4 -> daysOfMonth time.year April + sumDays {time with month = May}
            | 3 -> daysOfMonth time.year March + sumDays {time with month = April}
            | 2 -> daysOfMonth time.year February + sumDays {time with month = March}
            | 1 -> daysOfMonth time.year January + sumDays {time with month = February}
        (sumDays tim) - tim.day + 1

    /// Number of days in given year (356 if year is divisible by 4 else 355)
    /// y: int year
    let daysInYear (y:int) : int = 
        match y%4 with
        | 0 -> 366
        | _ -> 365

    /// If t1 > t2 return After, if t1 < t2 return Before, if t1 = t2 return f()
    /// t1: int
    /// t2: int
    /// f: Unit -> TimeOrder. Result if t1 = t2
    let timeOrder (t1:int) (t2:int) (f: Unit -> TimeOrder) : TimeOrder = 
        match t1> t2 with
        |true -> After
        |false when t1 = t2 -> f()
        |false -> Before

    /// Compare t1 with t2, returning TimeOrder value
    /// t1: Time
    /// t2: Time
    let timeCompare (t1:Time) (t2:Time) : TimeOrder= 
        let f() = Equal 
        match t1.year > t2.year with
        | true -> After
        | false when t1 = t2 -> timeOrder (- timeToYearEnd t1) (- timeToYearEnd t2) f
        | false -> Before

    /// Return the time difference (in days) between t1 and t2
    /// If t1 is before t2 return a negative value
    /// t1: Time
    /// t2: Time
    /// NB the implementation CANNOT use System.DateTime! Do it yourself!
    let rec timeDiff (t1:Time) (t2 : Time) : int =
        let yearDiff ta tb = [ta.year .. (-1).. tb.year+1] |> List.sumBy (fun t -> daysInYear t)
        let sameYearDiff ta tb = (timeToYearEnd tb) - (timeToYearEnd ta)
        match t1.year - t2.year with
        | x when x >0 ->  yearDiff t1 t2 + sameYearDiff t1 t2//- timeToYearEnd t1 + timeToYearEnd t2
        | x when x = 0 -> sameYearDiff t1 t2
        | x when x < 0 -> -yearDiff t2 t1 + sameYearDiff t1 t2

        
    /// return all weather data in wsl in a map keyed by station
    /// wsl: WeatherData list
    let stationReadings (wsl:WeatherData list) : Map<Station,WeatherData list> =
        let mutable data = Map.empty
        let updateValue s= 
            (Map.find s data) @ [temp,tim,sta]

        let findStation = List.map (fun temp,tim,sta-> match sta with
                                                       | S1 -> data <- data |> Map.add S1 (updateValue S1)
                                                       | S2 -> data <- data |> Map.add S2 (updateValue S2)
                                                       | S3 -> data <- data |> Map.add S3 (updateValue S3)
                                                       | S4 -> data <- data |> Map.add S4 (updateValue S4)
                                                       | S5 -> data <- data |> Map.add S5 (updateValue S5)
                                                       | S6 -> data <- data |> Map.add S6 (updateValue S6)
                                                       | S7 -> data <- data |> Map.add S7 (updateValue S7)
                                                       | S8 -> data <- data |> Map.add S8 (updateValue S8)
                                                       | S9 -> data <- data |> Map.add S9 (updateValue S9)
                                                       | S10 -> data <- data |> Map.add S10 (updateValue S10)
                                                       )
        wsl |> findStation


    /// return all weather data in wsl in a map keyed by year
    /// wsl: WeatherData list
    let yearReadings (wsl: WeatherData list): Map<int,WeatherData list> = failwithf "Not implemented yet"

    /// return all weather data in wsl in a map keyed by month
    /// wsl: WeatherData list
    let monthReadings (wsl:WeatherData list) : Map<Month,WeatherData list> = failwithf "Not implemented yet"

    /// return the maximum (positive) time difference in days between any two not necessarily distinct readings in wsl
    /// for any given month, year and station (MYS)
    /// maximise over all month,year,station combinations
    /// ignore MYS with no readings
    /// return none if there are no readings
    /// wsl: Weatherdata list
    let maxDiffByMonthYearStation (wsl:WeatherData list) = failwithf "Not implemented yet"

    let theAnswer = {
        Q1amInt = fun () -> mInt
        Q1bdaysOfMonth = fun () -> daysOfMonth
        Q2timeToYearEnd = fun () -> timeToYearEnd
        Q3daysInYear = fun () -> daysInYear
        Q4atimeOrder = fun () -> timeOrder
        Q4btimeCompare = fun () -> timeCompare
        Q5timeDiff = fun () -> timeDiff
        Q6astationReadings = fun () -> stationReadings
        Q6byearReadings = fun () -> yearReadings
        Q6cmonthReadings = fun () -> monthReadings
        Q7maxDiffByMonthYearStation = fun () -> maxDiffByMonthYearStation     
    }


