let input = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\\" + "01-input.txt")
    |> Array.map int
    |> Array.map (fun moduleMass -> (moduleMass / 3) - 2)
    |> Array.sum

input