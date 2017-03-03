namespace Model

module Types =

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

    type Time = {day: int; month: Month; year: int }

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

