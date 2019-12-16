
let splitOnComma (input:string) = input.Split([|','|])

let setState (intcode: int array) = 
    intcode.[1] <- 12
    intcode.[2] <- 2
    intcode

let calculate (intcode: int array) = 
    
    let doOperation (intcode: int array) (position: int) (operation: int->int->int) =
        let left = intcode.[ intcode.[position + 1] ]
        let right = intcode.[ intcode.[position + 2] ]
        let posResult = intcode.[position + 3]
        intcode.[posResult] <- operation left right
        intcode

    let rec calculate' (intcode: int array) (position: int) = 
        let opcode1 = intcode.[position] 
        match opcode1 with 
        | 1 -> calculate' (doOperation intcode position (+) ) (position + 4)
        | 2 -> calculate' (doOperation intcode position (*) ) (position + 4)
        | 99 -> intcode
        | _ -> failwith "error"

    calculate' intcode 0

let input = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\\" + "02-input.txt")
    |> Array.head
    |> splitOnComma
    |> Array.map int
    |> setState
    |> calculate
    |> Array.head