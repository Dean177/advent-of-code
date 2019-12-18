open Standard;
open AdventOfCode;
open Test;

type tile =
  | Empty
  | Wall
  | Entrance
  | Key(string)
  | Gate(string);

type maze = Matrix.t(tile);

let fromString =
  fun
  | "." => Empty
  | "#" => Wall
  | "@" => Entrance
  | tile =>
    if (Char.fromString(tile)
        ->Option.getOrFailWith(~exn=Invalid_argument(tile))
        ->Char.isLowercase) {
      Key(tile);
    } else {
      Gate(String.toLower(tile));
    };

let isEmpty = (maze: maze, position) => {
  switch (Matrix.get(maze, position)) {
  | Empty
  | Entrance
  | Key(_) => true
  | _ => false
  };
};

let parse = string =>
  String.split(string, ~on="\n")
  ->Array.map(~f=row => String.split(row, ~on="")->Array.map(~f=fromString));

let findKeys = maze =>
  Matrix.foldI(maze, ~initial=[], ~f=(keys, tile, position) => {
    switch (tile) {
    | Key(key) => [(key, position), ...keys]
    | _ => keys
    }
  });

let reachableKeys = (maze, ~inventory, ~from) => {
  let unexplored = MutableDeque.fromList([from]);
  let positionToSteps = ref(Map.Poly.singleton(~key=from, ~value=0));
  let keyToSteps = ref(Map.String.empty);
  while (!MutableDeque.isEmpty(unexplored)) {
    let position = MutableDeque.popLeft(unexplored)->Option.getExn;
    Array.map(Compass.directions, ~f=Compass.step(position))
    ->Array.map(~f=position => {(position, Matrix.get(maze, position))})
    ->Array.filter(~f=((position, tile)) => {
        switch (tile) {
        | Wall => false
        | Gate(requiredKey) => Set.includes(inventory, requiredKey)
        | _ => !Map.includes(positionToSteps^, position)
        }
      })
    ->Array.forEach(~f=((newPosition, tile)) => {
        let steps = Map.get(positionToSteps^, position)->Option.getExn + 1;
        positionToSteps :=
          Map.add(positionToSteps^, ~key=newPosition, ~value=steps);
        switch (tile) {
        | Key(key) when !Set.includes(inventory, key) =>
            keyToSteps :=
              Map.add(keyToSteps^, ~key, ~value=(steps, newPosition));
        | _ => 
          MutableDeque.pushRight(unexplored, newPosition);
        };
      });
  };
  keyToSteps^;
};

let seen = ref(Map.Poly.empty());

let toKey = (position, inventory) => (position, Set.toList(inventory));

let rec minimumSteps = (~inventory=Set.String.empty, maze, start) => {
  switch (Map.get(seen^, toKey(start, inventory))) {
  | Some(steps) => steps
  | None =>
    let keys = reachableKeys(maze, ~inventory, ~from=start);
    let steps =
      if (Map.length(keys) == 0) {
        0;
      } else {
        Map.toArray(keys)
        ->Array.map(~f=((key, (distance, position))) => {
            distance
            + minimumSteps(maze, position, ~inventory=Set.add(inventory, key))
          })
        ->Array.minimum(~compare)
        ->Option.getExn;
      };
    seen := Map.add(seen^, ~key=toKey(start, inventory), ~value=steps);
    steps;
  };
};

test(expect => {
  let maze =
    parse(
      {|########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################|},
    );
  let (entrance, _) =
    Matrix.findIndex(maze, ~f=(_, tile) => tile == Entrance)->Option.getExn;
  expect(minimumSteps(maze, entrance))->toEqual(132);
});

test(expect => {
  let maze =
    parse(
      {|#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################|},
    );
  let (entrance, _) =
    Matrix.findIndex(maze, ~f=(_, tile) => tile == Entrance)->Option.getExn;
  expect(minimumSteps(maze, entrance))->toEqual(136);
});

let part_1 = {
  let input = read("./Day18.txt");
  let maze = parse(input);
  let (entrance, _) =
    Matrix.findIndex(maze, ~f=(_, tile) => tile == Entrance)->Option.getExn;

  let start = Js.Date.now()
  print(("Part 1", minimumSteps(maze, entrance), Js.Date.now() -. start ));
}

let toKey = (positions, inventory) => (positions, Set.toList(inventory));

let seen = ref(Map.Poly.empty());

let rec minimumSteps = (~inventory=Set.String.empty, maze, positions) => {
  switch (Map.get(seen^, toKey(positions, inventory))) {
  | Some(steps) => steps
  | None =>
    let robotKeys = Array.map(positions, ~f=(position) => reachableKeys(maze, ~from=position, ~inventory));
    let discovered = Array.map(robotKeys, ~f=Map.length) |> Array.sum((module Int));
    let steps =
      if ((discovered) == 0) {
        0;
      } else {
        Array.mapWithIndex(robotKeys, ~f=(i, keys) => {
          Map.toArray(keys)
          ->Array.map(~f=((key, (distance, keyPosition))) => {
            let positions = Array.initialize(~length=Array.length(robotKeys), ~f=j => {
              (i == j) ? keyPosition : positions[j]
            });
            distance
              + minimumSteps(maze, positions, ~inventory=Set.add(inventory, key))
          })
        })
        ->Array.concatenate
        ->Array.minimum(~compare)
        ->Option.getExn;
        
      };
    seen := Map.add(seen^, ~key=toKey(positions, inventory), ~value=steps);
    steps;
  };
};

let entrances = (maze) => {
  let entrances = ref([]);
  Matrix.forEachI(maze, ~f=(tile, position) => {
    if (tile == Entrance) {
      entrances := [position, ...entrances^]
    }
  })
  Array.fromList(entrances^)
}

test(expect => {
  let maze =
    parse(
      {|#############
#DcBa.#.GhKl#
#.###@#@#I###
#e#d#####j#k#
###C#@#@###J#
#fEbA.#.FgHi#
#############|},
    );
  
  expect(minimumSteps(maze, entrances(maze)))->toEqual(32);
});

let part_2 = {
  let input = read("./Day18Part2.txt");
  let maze = parse(input);  
  let start = Js.Date.now()
  print(("Part 2", minimumSteps(maze, entrances(maze))
  ,Js.Date.now() -. start ));
}
