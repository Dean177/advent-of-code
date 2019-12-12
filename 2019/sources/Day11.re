open Standard;
open AdventOfCode;

let input = read("./Day11.txt");

module Computer = Day09.Computer;

type direction =
  | N
  | E
  | S
  | W;

type color =
  | Black
  | White;

type rotation =
  | Right
  | Left;

type outputState =
  | PaintPanel
  | Rotate;

let rotate = (direction, rotation) =>
  switch (direction, rotation) {
  | (E, Left)
  | (W, Right) => N
  | (S, Left)
  | (N, Right) => E
  | (W, Left)
  | (E, Right) => S
  | (N, Left)
  | (S, Right) => W
  };

let step = ((x, y), direction) =>
  switch (direction) {
  | N => (x, y - 1)
  | E => (x + 1, y)
  | S => (x, y + 1)
  | W => (x - 1, y)
  };

let outputState = ref(PaintPanel);
let position = ref((0, 0));
let facing = ref(N);
let panelColors = ref(Map.Poly.empty());
let computer =
  Computer.make(
    Computer.Program.parse(input),
    ~input=
      () =>
        Int64.of_int(
          switch (
            Map.get(panelColors^, position^)->Option.get(~default=Black)
          ) {
          | Black => 0
          | White => 1
          },
        ),
    ~output=
      value => {
        switch (outputState^) {
        | PaintPanel =>
          let color =
            switch (Int64.to_int(value)) {
            | 0 => Black
            | 1 => White
            | _ =>
              raise(
                Invalid_argument(
                  "Unexpected color: " ++ Int64.to_string(value),
                ),
              )
            };
          panelColors := Map.add(panelColors^, ~key=position^, ~value=color);
          outputState := Rotate;
        | Rotate =>
          let rotation =
            switch (Int64.to_int(value)) {
            | 0 => Left
            | 1 => Right
            | _ =>
              raise(
                Invalid_argument(
                  "Unexpected rotation: " ++ Int64.to_string(value),
                ),
              )
            };
          let direction = rotate(facing^, rotation);
          facing := direction;
          position := step(position^, direction);
          outputState := PaintPanel;
        }
      },
  );

let rec loop = () => {
  switch (Computer.run(computer)) {
  | `HaltOnOutput => loop()
  | `Halt => ()
  };
};

let part1 = () => {
  loop();
  print(("Painted panels:", Map.keys(panelColors^)->List.length));
};

let extent = (t, ~compare) =>
  Array.fold(t, ~initial=None, ~f=(range, element) => {
    switch (range) {
    | None => Some((element, element))
    | Some((min, max)) =>
      Some((
        compare(element, min) < 0 ? element : min,
        compare(element, max) > 0 ? element : max,
      ))
    }
  });

let part_2 = {
  panelColors := Map.Poly.singleton(~key=position^, ~value=White);
  loop();
  print(("Painted panels:", Map.keys(panelColors^)->List.length));
  let paints = Map.toArray(panelColors^);
  let (minx, maxx) =
    extent(Array.map(paints, ~f=(((x, _), _)) => x), ~compare)
    ->Option.getExn;
  let (miny, maxy) =
    extent(Array.map(paints, ~f=(((_, y), _)) => y), ~compare)
    ->Option.getExn;

  let message =
    Array.initialize(~length=1 + maxy - miny, ~f=_ => {
      Array.initialize(~length=1 + maxx - minx, ~f=_ => {js|🔲|js})
    });

  Array.forEach(paints, ~f=(((x, y), color)) => {
    message[y - miny][x - minx] = (
      switch (color) {
      | Black => {js|⬛️|js}
      | White => {js|⬜️|js}
      }
    )
  });

  message
  ->Array.map(~f=row => Array.toList(row)->String.join(~sep=""))
  ->Array.toList
  ->String.join(~sep="\n")
  ->print;
};

let _ = ();