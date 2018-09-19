open Core

type element = 
  | Plutonium
  | Promethium 
  | Ruthenium
  | Stronium
  | Thulium 
[@@ deriving compare, sexp]

type equipment =   
  | Generator of element
  | Microchip of element
[@@deriving compare, sexp]

module EquipmentList = struct
  module T = struct 
    type t = equipment list [@@deriving compare, sexp]  
  end
  include T
  include Comparator.Make(T)
end

let rec equipment_sets (equipment : EquipmentList.t) = match equipment with
  | [] -> Set.empty (module EquipmentList)
  | x :: [] -> Set.singleton (module EquipmentList) [x]  
  | x :: y :: zs -> 
    Set.union 
      (Set.singleton (module EquipmentList) [x;y;]) 
      (Set.union (equipment_sets (x :: zs)) (equipment_sets (y :: zs))  )    

let%expect_test _ =
  equipment_sets [Generator Promethium; Generator Ruthenium; Generator Stronium; Generator Thulium;] 
  |> Set.to_list |> [%sexp_of : equipment list list] |> Sexp.to_string_hum |> print_endline;
  [%expect {|
    (((Generator Promethium)) ((Generator Promethium) (Generator Ruthenium))
     ((Generator Promethium) (Generator Stronium))
     ((Generator Promethium) (Generator Thulium)) ((Generator Ruthenium))
     ((Generator Ruthenium) (Generator Stronium))
     ((Generator Ruthenium) (Generator Thulium)) ((Generator Stronium))
     ((Generator Stronium) (Generator Thulium)) ((Generator Thulium)))
  |}]

type frtg_floor = {
  number: int;
  equipment: equipment list;
} [@@deriving compare, sexp]

module State = struct 
  module T = struct
    type t = {
      elevator: int;
      floors: frtg_floor list;  
    } [@@deriving compare, sexp]

    let directions_for = function 
      | 1 -> [(1, 2)]
      | 4 -> [(4, 3)]
      | x -> [(x, x + 1); (x, x - 1)]

    let remove_devices devices equipment =  
      List.filter equipment ~f:(fun equipii -> List.for_all devices ~f:(( <> ) equipii))

    let add_devices devices equipment =
      List.append devices equipment

    let move_devices src dst devices floors =
      List.map floors ~f:(fun floor -> match (floor.number = src, floor.number = dst) with 
          | (true, _) -> { floor with equipment = remove_devices devices floor.equipment }
          | (_, true) -> { floor with equipment = add_devices devices floor.equipment }
          | _ -> floor)

    let possible_states ({elevator; floors;}) = 
      let directions = directions_for elevator in
      let {equipment; _;} = List.nth_exn floors (elevator - 1) in 
      let equipment_subsets = equipment_sets equipment |> Set.to_list in
      List.bind directions ~f:(fun (source, destination) -> List.map equipment_subsets ~f:(fun devices -> {
            elevator = destination;
            floors = move_devices source destination devices floors
          }))

    let%expect_test _ =
      possible_states {elevator=2;floors=[{number=1;equipment=[]}; {number=2;equipment=[Generator Promethium; Generator Ruthenium;]}; {number=3;equipment=[]}]} |> [%sexp_of : t list] 
      |> Sexp.to_string_hum |> print_endline;
      [%expect {|
    (((elevator 3)
      (floors
       (((number 1) (equipment ()))
        ((number 2) (equipment ((Generator Ruthenium))))
        ((number 3) (equipment ((Generator Promethium)))))))
     ((elevator 3)
      (floors
       (((number 1) (equipment ())) ((number 2) (equipment ()))
        ((number 3) (equipment ((Generator Promethium) (Generator Ruthenium)))))))
     ((elevator 3)
      (floors
       (((number 1) (equipment ()))
        ((number 2) (equipment ((Generator Promethium))))
        ((number 3) (equipment ((Generator Ruthenium)))))))
     ((elevator 1)
      (floors
       (((number 1) (equipment ((Generator Promethium))))
        ((number 2) (equipment ((Generator Ruthenium))))
        ((number 3) (equipment ())))))
     ((elevator 1)
      (floors
       (((number 1) (equipment ((Generator Promethium) (Generator Ruthenium))))
        ((number 2) (equipment ())) ((number 3) (equipment ())))))
     ((elevator 1)
      (floors
       (((number 1) (equipment ((Generator Ruthenium))))
        ((number 2) (equipment ((Generator Promethium))))
        ((number 3) (equipment ())))))) |}]
  end
  include T
  include Comparator.Make(T)
end

(* Hopefully a good enough metric to guide the search *)
let distance_to_solution ({elevator; floors;} : State.t) = 
  List.map floors ~f:(fun { number; equipment;} -> (List.length equipment) * (4 - (number + 1))) 
  |> List.fold ~init:0 ~f: ( + )
  |> ( + ) (4 - elevator)

let contains_generator_for el equipment =
  let equality_test = [%compare.equal : equipment] (Generator el) in
  match List.find ~f:equality_test equipment with 
  | None -> false
  | Some _ -> true

let compare_states state_a state_b = Int.compare (distance_to_solution state_a) (distance_to_solution state_b)

let generators = List.filter ~f:(fun eq -> match eq with Generator _ -> true | _ -> false)

let microchip_elements = List.filter_map ~f:(fun eq -> match eq with Microchip el -> Some el | _ -> None)

(* A microchip is safe if the floor contains its generator or the floor contains no generators *)
let is_safe_floor {equipment; _} = List.for_all (microchip_elements equipment) ~f:(fun chip -> 
    contains_generator_for chip equipment || List.length (generators equipment) = 0)

let is_safe ({floors; _} : State.t) = List.for_all floors ~f:is_safe_floor

let instructions = {|
  The fourth floor contains nothing relevant.
  The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
  The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
  The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
|}

let initial_state : State.t = {
  elevator = 1;
  floors = [
    { number = 4; equipment = [];};
    { number = 3; equipment = [Generator Promethium; Generator Ruthenium; Microchip Promethium; Microchip Ruthenium]; };
    { number = 2; equipment = [Microchip Plutonium; Microchip Stronium]; };
    { number = 1; equipment = [Generator Thulium; Generator Plutonium; Generator Stronium; Microchip Thulium;]; };
  ];
}

let goal_state : State.t = {
  elevator = 4;
  floors = [
    { 
      number = 4; 
      equipment = [
        Generator Plutonium; Generator Promethium; Generator Ruthenium;
        Generator Stronium; Generator Thulium; Microchip Plutonium;
        Microchip Promethium; Microchip Ruthenium; Microchip Stronium; Microchip Thulium; 
      ];
    };
    { number = 3; equipment = [] };
    { number = 2; equipment = [] };
    { number = 1; equipment = [] };
  ];
}

let search () =
  let queued_states = 
    Heap.create 
      ~min_size:1000 
      ~cmp:compare_states
      () 
  in
  Heap.add queued_states initial_state;
  let rec loop (acc, seen_states) = 
    print_endline ("Path length: " ^ Int.to_string (List.length acc));
    print_endline ("Seen states: " ^ Int.to_string (Set.length seen_states));
    print_endline ("Heap size: " ^ Int.to_string (Heap.length queued_states));
    let current_state_r = Heap.pop queued_states in          
    match current_state_r with 
    | None -> acc
    | Some current_state -> 
      if distance_to_solution current_state = 0 then acc 
      else (
        State.possible_states current_state 
        (* |> List.filter ~f:is_safe  *)
        |> List.filter ~f:(fun state -> not (Set.mem seen_states state))
        |> List.iter ~f:(Heap.add queued_states);
        loop (current_state :: acc, Set.add seen_states current_state)
      )
  in
  loop ([], Set.empty (module State))

let%expect_test _ =
  let path = search () in
  path |> [%sexp_of : State.t list] |> Sexp.to_string_hum |> print_endline;
  [%expect {|
    Path length: 0
    Seen states: 0
    Heap size: 1
    Path length: 1
    Seen states: 1
    Heap size: 0
    (((elevator 1)
      (floors
       (((number 4) (equipment ()))
        ((number 3)
         (equipment
          ((Generator Promethium) (Generator Ruthenium) (Microchip Promethium)
           (Microchip Ruthenium))))
        ((number 2) (equipment ((Microchip Plutonium) (Microchip Stronium))))
        ((number 1)
         (equipment
          ((Generator Thulium) (Generator Plutonium) (Generator Stronium)
           (Microchip Thulium)))))))) |}]