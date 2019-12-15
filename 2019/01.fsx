let calculateFuel moduleMass = 

    let rec calculateFuel' fuel mass = 
        let fuelForMass = mass / 3 - 2
        match fuelForMass > 0 with
        | false -> fuel
        | true -> calculateFuel' (fuel + fuelForMass) fuelForMass

    calculateFuel' 0 moduleMass

let fuelRequirements = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\\" + "01-input.txt")
    |> Array.map int
    |> Array.map calculateFuel
    |> Array.sum

fuelRequirements
