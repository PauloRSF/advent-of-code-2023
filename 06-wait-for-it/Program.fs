open System.IO

type Race = { time: int64; distance: int64 }

// ---------------------
// Input parsing
// ---------------------

let parseInputLine (inputLine: string) =
    inputLine.Split " "
    |> Array.skip 1
    |> Array.filter (fun item -> item <> "")
    |> Array.map int64

let readInput =
    File.ReadLines
    >> Seq.map parseInputLine
    >> Seq.transpose
    >> Seq.map Seq.toArray
    >> Seq.map (fun x ->
        { Race.time = x[0]
          Race.distance = x[1] })
    >> Seq.toArray

// ---------------------
// First puzzle
// ---------------------

let computeTraveledDistanceForButtonHoldTime raceTime buttonHoldTime =
    buttonHoldTime * (raceTime - buttonHoldTime)

let isTraveledDistanceGreaterThenRaceRecord raceDistance traveledDistance = traveledDistance > raceDistance

// The traveled distance graph is a parabola, so it is symmetrical in the y axis,
// hence `(2 * x)`.
// However, if the number of ways to beat the record is even (i.e. the graph
// has a single integer maximum value), it doesn't have a meaningful symmetric
// value, hence `(1 - race.time % 2)`
let addSymmetricalWaysToBeatRaceRecord raceTime count =
    2L * (int64) count - (1L - raceTime % 2L)

let computeNumberOfWaysToBeatRaceRecord race =
    seq { 1L .. race.time / 2L }
    |> Seq.map (computeTraveledDistanceForButtonHoldTime race.time)
    |> Seq.filter (isTraveledDistanceGreaterThenRaceRecord race.distance)
    |> Seq.length
    |> addSymmetricalWaysToBeatRaceRecord race.time

// ---------------------
// Second puzzle
// ---------------------

let getIntegerDigitCount (n: int64) =
    let rec loop (acc: int) (n: int64) =
        match n / 10L with
        | 0L -> acc
        | _ -> loop (acc + 1) (n / 10L)

    loop 1 n

let concatDigits (x: int64) (y: int64) =
    (x * (int64) (pown 10 (getIntegerDigitCount y))) + y

let concatRacesIntoSingleRace =
    Seq.reduce (fun acc race ->
        { Race.time = concatDigits acc.time race.time
          Race.distance = concatDigits acc.distance race.distance })

// ---------------------
// Main
// ---------------------

let races = readInput "input.txt"

let productOfNumberOfWaysToBeatRacesRecords =
    races |> Array.map computeNumberOfWaysToBeatRaceRecord |> Array.reduce (*)

let numberOfWaysToBeatSingleRaceRecord =
    races |> concatRacesIntoSingleRace |> computeNumberOfWaysToBeatRaceRecord

printfn "Product of number of ways to beat the races records: %u" productOfNumberOfWaysToBeatRacesRecords
printfn "Number of ways to beat the single race record: %u" numberOfWaysToBeatSingleRaceRecord
