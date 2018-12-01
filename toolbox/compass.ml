type t = N | E | S | W [@@deriving sexp]

type direction = Clockwise | Anticlockwise

let rotate direction orientation = (match orientation with
    | N -> (match direction with | Clockwise -> E | Anticlockwise -> W)
    | E -> (match direction with | Clockwise -> S | Anticlockwise -> N)
    | S -> (match direction with | Clockwise -> W | Anticlockwise -> E)
    | W -> (match direction with | Clockwise -> N | Anticlockwise -> S)
  )
