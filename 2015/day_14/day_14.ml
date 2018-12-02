open Core
let testInput = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

let puzzleInput = "Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
Cupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds.
Prancer can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
Donner can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 11 km/s for 12 seconds, but then must rest for 125 seconds.
Comet can fly 21 km/s for 6 seconds, but then must rest for 121 seconds.
Blitzen can fly 18 km/s for 3 seconds, but then must rest for 50 seconds.
Vixen can fly 20 km/s for 4 seconds, but then must rest for 75 seconds.
Dancer can fly 7 km/s for 20 seconds, but then must rest for 119 seconds."

type deer = {
  name: string;
  speed: int;
  duration: int;
  rest: int;
} [@@deriving sexp]

let parse deerString = match Toolbox.String.words deerString with
  | name :: _can :: _fly :: speedStr :: _kms :: _for :: durationStr :: _seconds :: _but :: _then :: _must :: _rest :: __for :: rest :: __seconds :: [] ->  { 
      name; 
      speed = Int.of_string speedStr; 
      duration = Int.of_string durationStr; 
      rest = Int.of_string rest;
    }
  | _ -> failwith ("Malformed deer: " ^ deerString)

let deers = puzzleInput |> String.split_lines |> List.map ~f:parse

let testDeers = testInput |> String.split_lines |> List.map ~f:parse

type activity = Moving of int | Resting of int [@@deriving sexp]
type state = { 
  activity: activity;
  spec : deer;
  travelled : int;
} [@@deriving sexp]

let initialState deerSpec = { 
  activity = Moving deerSpec.duration;
  spec = deerSpec;
  travelled = 0;
} [@@deriving sexp]

let increment {activity; spec; travelled;} = 
  match activity with 
  | Moving n -> {
      activity = if n > 1 then Moving (n - 1) else Resting spec.rest;
      spec;
      travelled = travelled + spec.speed ;
    }
  | Resting n -> {
      activity = if n > 1 then Resting (n - 1) else Moving spec.duration;
      spec;
      travelled;
    }

let partOne deers = 
  let initialDeers = List.map deers ~f:initialState in
  List.fold (List.range 0 2503) ~init:initialDeers ~f:(fun deerStates (_s : int) -> 
      List.map deerStates ~f:increment
    )
  |> List.max_elt ~compare:(fun deerStateA deerStateB -> 
      Int.compare deerStateA.travelled deerStateB.travelled)

type stateTwo = { 
  state : state;  
  points : int;
} [@@deriving sexp]

let initialStateTwo deerSpec = { 
  state = initialState deerSpec;
  points = 0;
} [@@deriving sexp]

let distributePoints deerStates = 
  let leader = 
    List.max_elt deerStates ~compare:(fun deerA deerB -> 
        Int.compare deerA.state.travelled deerB.state.travelled) 
    |> Option.value_exn
  in
  List.map deerStates ~f:(fun deer -> 
      { deer 
        with points = 
               if deer.state.travelled = leader.state.travelled then 
                 deer.points + 1 
               else 
                 deer.points })

let partTwo deers = 
  let initialDeers = List.map deers ~f:initialStateTwo in
  List.fold (List.range 0 2503) ~init:initialDeers ~f:(fun deerStates (_s : int) -> 
      let updatedPositions = List.map deerStates ~f:(fun s2 -> { s2 with state = increment s2.state }) in
      distributePoints updatedPositions
    )
  |> List.max_elt ~compare:(fun deerStateA deerStateB -> 
      Int.compare deerStateA.points deerStateB.points)


let%expect_test _ =
  partOne deers |> Option.value_exn |> [%sexp_of : state] |> print_s;
  partTwo deers |> Option.value_exn |> [%sexp_of : stateTwo] |> print_s;
  [%expect {|
    ((activity (Moving 3))
     (spec ((name Cupid) (speed 8) (duration 17) (rest 114))) (travelled 2696))
    ((state
      ((activity (Resting 92))
       (spec ((name Rudolph) (speed 22) (duration 8) (rest 165)))
       (travelled 2640)))
     (points 1084)) |}] 
