open Standard;
open AdventOfCode;

module Computer = Day09.Computer;

let input = read("./Day15.txt");

type direction =
  | N
  | E
  | S
  | W;

let directions = [|N, E, S, W|];

let ofInt =
  fun
  | 1 => N
  | 2 => S
  | 3 => W
  | _ => E;

let toInt = direction =>
  Int64.of_int(
    switch (direction) {
    | N => 1
    | S => 2
    | W => 3
    | E => 4
    },
  );

let step = ((x, y)) =>
  fun
  | N => (x, y - 1)
  | E => (x + 1, y)
  | S => (x, y + 1)
  | W => (x - 1, y);

type status =
  | HitWall
  | Moved
  | FoundOxygen;

type tile =
  | Empty
  | Wall
  | Oxygen
  | Start;

let fromString =
  fun
  | "." => Empty
  | "#" => Wall
  | "O" => Oxygen
  | "S" => Start
  | tile => raise(Invalid_argument(tile));

let show = (droidPosition, maze: Hashtbl.t((int, int), tile)): string => {
  let tiles =
    Array.initialize(
      ~length=Hashtbl.length(maze),
      ~f=Fun.constant(((0, 0), Empty)),
    );
  let i = ref(0);

  maze
  |> Hashtbl.iter((key, value) => {
       tiles[i^] = (key, value);
       i := i^ + 1;
     });

  let (minx, maxx) =
    Array.extent(Array.map(tiles, ~f=(((x, _), _)) => x), ~compare)
    ->Option.getExn;
  let (miny, maxy) =
    Array.extent(Array.map(tiles, ~f=(((_, y), _)) => y), ~compare)
    ->Option.getExn;

  let display =
    Array.initialize(~length=1 + maxy - miny, ~f=_ => {
      Array.initialize(~length=1 + maxx - minx, ~f=_ => " ")
    });

  Array.forEach(tiles, ~f=(((x, y), tile)) => {
    display[y - miny][x - minx] = (
      switch (tile) {
      | Start => "S"
      | Empty => "."
      | Wall => "#"
      | Oxygen => "O"
      }
    )
  });

  display[- miny][- minx] = "S";
  let (x, y) = droidPosition;
  display[y - miny][x - minx] = "D";

  Array.map(display, ~f=row => Js.Array.joinWith("", row))
  |> Js.Array.joinWith("\n");
};

// Just let this run until the maze is fully discovered
let genMaze = () => {
  let position = ref((0, 0));
  let direction = ref(N);
  let steps = ref(0);
  let maze = Hashtbl.create(50 * 50);
  Hashtbl.add(maze, (0, 0), Empty);

  let computer =
    Computer.make(
      Computer.Program.parse(input),
      ~input=
        () => {
          let choice =
            Float.floor(1. +. Js.Math.random() *. 4.)
            ->Float.toInt
            ->Option.getExn;
          steps := steps^ + 1;
          direction := ofInt(choice);
          Int64.of_int(choice);
        },
      ~output=
        value => {
          let status =
            switch (Int64.to_int(value)) {
            | 0 => HitWall
            | 1 => Moved
            | 2 => FoundOxygen
            | other =>
              raise(
                Invalid_argument(
                  "Unexpected status: " ++ Int.toString(other),
                ),
              )
            };

          switch (status) {
          | FoundOxygen =>
            position := step(position^, direction^);
            Hashtbl.add(maze, position^, Oxygen);
          | HitWall => Hashtbl.add(maze, step(position^, direction^), Wall)
          | Moved =>
            position := step(position^, direction^);
            Hashtbl.add(maze, position^, Empty);
          };

          if (steps^ mod 10_000 == 0) {
            LogUpdate.update(show(position^, maze));
          };
        },
    );

  Computer.runToHalt(computer);
};

let maze =
  read("./Day15-maze.txt")
  ->String.split(~on="\n")
  ->Array.map(~f=row => String.split(row, ~on="")->Array.map(~f=fromString));

let isEmpty = (maze, (x, y)) => {
  maze[y][x] == Empty || maze[y][x] == Oxygen;
};

let rec pathBetween = (a, ~visited=Set.Poly.singleton(a), b) =>
  if (a == b) {
    Some(Set.length(visited));
  } else {
    let available =
      Array.map(directions, ~f=step(a))
      ->Array.filter(~f=position => isEmpty(maze, position))
      ->Array.filter(~f=position => !Set.includes(visited, position));

    if (Array.isEmpty(available)) {
      None;
    } else {
      Array.fold(available, ~initial=None, ~f=(path, step) => {
        switch (path) {
        | Some(_) => path
        | None => pathBetween(~visited=Set.add(visited, a), step, b)
        }
      });
    };
  };

let rec findIndex = (~x=0, ~y=0, arrays, ~f) =>
  if (y >= Array.length(arrays)) {
    None;
  } else if (x >= Array.length(arrays[y])) {
    findIndex(~x=0, ~y=y + 1, arrays, ~f);
  } else if (f(arrays[y][x])) {
    Some((x, y));
  } else {
    findIndex(~x=x + 1, ~y, arrays, ~f);
  };

let oxygen = findIndex(maze, ~f=tile => tile == Oxygen)->Option.getExn;
let start = findIndex(maze, ~f=tile => tile == Start)->Option.getExn;

let part_1 = {
  print("Part 1");
  print(pathBetween(start, oxygen));
};

let part_2 = {
  let maximum = ref(0);
  for (y in 0 to Array.length(maze) - 1) {
    for (x in 0 to Array.length(maze[y]) - 1) {
      if (isEmpty(maze, (x, y))) {
        let pathLength =
          pathBetween(oxygen, (x, y))->Option.get(~default=0);
        if (pathLength > maximum^) {
          maximum := pathLength;
        };
      };
    };
  };
  print(maximum^);
};