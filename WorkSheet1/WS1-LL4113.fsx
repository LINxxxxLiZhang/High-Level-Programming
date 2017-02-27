let sineSeries (x:float,n:int) :float =
    let factorial i = [1.0..(float (2*i+1))] |> List.reduce (*)
    let power x i = x ** (float i)
    let term i = (power -1.0 i) * (power x (2*i+1)) / factorial i
    [0..n] |> List.map term |> List.reduce (+)

printfn "result: %A" (sineSeries (2.0,6))