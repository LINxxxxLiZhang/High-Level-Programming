let sineSeries x n = //x:float, n:int
  let pow a b = a ** (float b)
  let factorial a = [1.0..(float a)] |> List.reduce (*)
  let term i = (pow x (2*i+1)) * (pow -1.0 i) / factorial (2*i+1)
  [0..n] |> List.map term |> List.reduce (+)


printfn "result: %A" (sineSeries 2.0 6)