
let splitOnComma (input:string) = input.Split([|','|])

let initialize (noun:int) (verb:int) (intcode: int array) = 
    intcode.[1] <- noun
    intcode.[2] <- verb
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

let part1 = 
    input
    |> Array.map int
    |> initialize 12 2
    |> calculate
    |> Array.head

let part2 = 
    seq {
        for noun in 0 .. 99 do
            for verb in 0 .. 99 do
                let result = input |> Array.map int |> initialize noun verb |> calculate |> Array.head
                if result = 19690720 then 
                    yield (100 * noun + verb)
    } |> Seq.head