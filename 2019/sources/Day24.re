open Standard;
open AdventOfCode;
open Test;

let input = ".##.#
###..
#...#
##.#.
.###.";

let parse = input =>
  String.split(input, ~on="\n")->Array.map(~f=String.split(~on=""));

let show = grid =>
  Array.map(grid, ~f=row => Array.toList(row)->String.join(~sep=""))
  ->Array.toList
  ->String.join(~sep="\n");

let adjacentBugs = (grid, position) =>
  Array.filterMap(Compass.directions, ~f=direction => {
    Matrix.getAt(grid, Compass.step(position, direction))
  })
  ->Array.count(~f=tile => tile == "#");

let iter = grid =>
  Matrix.mapI(
    grid,
    ~f=(tile, position) => {
      let bugs = adjacentBugs(grid, position);
      switch (tile) {
      | "#" when bugs != 1 => "."
      | "." when bugs == 1 || bugs == 2 => "#"
      | "#" => "#"
      | "." => "."
      | other => other
      };
    },
  );

test(expect => {
  let two = "#####\n....#\n....#\n...#.\n#.###";
  let three = "#....\n####.\n...##\n#.##.\n.##.#";
  expect(iter(parse(two))->show)->toEqual(three);
});

let bioRating = grid =>
  Matrix.foldI(grid, ~initial=0, ~f=(total, tile, (x, y)) =>
    if (tile == "#") {
      total + Int.power(~base=2, ~exponent=x + 5 * y);
    } else {
      total;
    }
  );

test(expect => {
  expect(bioRating(parse(".....\n.....\n.....\n#....\n.#...")))
  ->toEqual(2129920)
});

let part_1 = () => {
  let seen = ref(Set.Poly.empty());
  let current = ref(parse(input));
  let iteration = ref(0);
  while (!Set.includes(seen^, current^)) {
    print(iteration^);
    print(show(current^));
    seen := Set.add(seen^, current^);
    current := iter(current^);

    Ref.increment(iteration);
  };
  print(bioRating(current^));
};

let center = (2, 2);

let empty = parse(".....\n.....\n..?..\n.....\n.....");

let layer = (layers, layer) =>
  Map.get(layers, layer)->Option.get(~default=empty);

let adjacentBugs = (layers, z, position) => {
  Array.bind(
    Compass.directions,
    ~f=direction => {
      let (x, y) = Compass.step(position, direction);
      switch (x, y) {
      | ((-1), _)
      | (5, _)
      | (_, (-1))
      | (_, 5) => [|
          Matrix.get(layer(layers, z + 1), Compass.step(center, direction)),
        |]
      | (2, 2) =>
        switch (direction) {
        | N => layer(layers, z - 1)->Matrix.row(4)
        | S => layer(layers, z - 1)->Matrix.row(0)
        | E => layer(layers, z - 1)->Matrix.column(0)
        | W => layer(layers, z - 1)->Matrix.column(4)
        }
      | _ => [|Matrix.get(layer(layers, z), (x, y))|]
      };
    },
  )
  ->Array.count(~f=tile => tile == "#");
};

let part_2 = {
  let grid = parse(".##.#\n###..\n#.?.#\n##.#.\n.###.");
  let layers = ref(Map.Int.singleton(~key=0, ~value=grid));
  let minute = ref(0);
  while (minute^ < 200) {
    let (minLayerZ, maxLayerZ) = Map.extent(layers^)->Option.getExn;
    let newLayers: Map.Int.t(Matrix.t(string)) =
      (layers^)
      ->Map.add(~key=minLayerZ - 1, ~value=empty)
      ->Map.add(~key=maxLayerZ + 1, ~value=empty)
      ->Map.map(~f=((z, grid)) =>
          Matrix.mapI(
            grid,
            ~f=(tile, position) => {
              let bugs = adjacentBugs(layers^, z, position);
              switch (tile) {
              | "#" when bugs != 1 => "."
              | "." when bugs == 1 || bugs == 2 => "#"
              | "#" => "#"
              | "." => "."
              | other => other
              };
            },
          )
        )
      ->Map.Int.fromArray;

    layers := newLayers;
    Ref.increment(minute);
  };

  print("Part 2");
  Map.toArray(layers^)
  ->Array.map(~f=((_z, grid)) => Matrix.count(grid, ~f=tile => tile == "#"))
  |> Array.sum((module Int))
  |> print;
};

let _ = ();