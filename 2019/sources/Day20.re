open Standard;
open AdventOfCode;

let input = read("Day20.txt");

let portalName = (a, b, direction: Compass.direction) =>
  switch (direction) {
  | N
  | W => b ++ a
  | S
  | E => a ++ b
  };

let nameAndEntrance = (maze, namePartA, position) => {
  Array.filterMap(Compass.directions, ~f=direction => {
    switch (Matrix.getAt(maze, Compass.step(position, direction))) {
    | None
    | Some(" ")
    | Some(".")
    | Some("#") => None
    | Some(namePartB) =>
      let name = portalName(namePartA, namePartB, direction);
      let entrance = Compass.step(position, Compass.opposite(direction));
      switch (Matrix.getAt(maze, entrance)) {
      | Some(".") => Some((name, position))
      | _ => None
      };
    }
  })
  ->Array.first;
};

let parse = input => {
  let maze =
    String.split(input, ~on="\n")->Array.map(~f=String.split(~on=""));
  let portals =
    Matrix.foldI(
      maze, ~initial=Map.String.empty, ~f=(portals, tile, position) =>
      switch (tile) {
      | " "
      | "."
      | "#" => portals
      | portal =>
        Option.fold(
          nameAndEntrance(maze, portal, position),
          ~initial=portals,
          ~f=(portals, (name, entrance)) =>
          Map.update(
            portals,
            ~key=name,
            ~f=
              fun
              | None => Some([entrance])
              | Some([exit]) => Some([entrance, exit])
              | Some(other) => {
                  print(("fail", Array.fromList([entrance, ...other])));
                  raise(
                    Invalid_argument(
                      "Attempted to add additional entrance to portal: "
                      ++ name,
                    ),
                  );
                },
          )
        )
      }
    );
  (maze, portals);
};

let availableSteps = (maze, portals, position) => {
  Array.filterMap(
    Compass.directions,
    ~f=direction => {
      let newPosition = Compass.step(position, direction);
      switch (Matrix.getAt(maze, newPosition)) {
      | None
      | Some(" ")
      | Some("#") => None
      | Some(".") => Some(newPosition)
      | Some(portalNameA) =>
        let portalNameB =
          Matrix.get(maze, Compass.step(newPosition, direction));
        let name = portalName(portalNameA, portalNameB, direction);
        switch (Map.get(portals, name)) {
        | Some([orange, blue]) =>
          let exit = newPosition == orange ? blue : orange;
          let exitVector =
            Array.find(Compass.directions, ~f=direction => {
              Matrix.getAt(maze, Compass.step(exit, direction)) == Some(".")
            });
          Option.map(exitVector, ~f=Compass.step(exit));
        | _ => None
        };
      };
    },
  );
};

let minSteps = (maze, portals, start, finish) => {
  let unexplored = MutableDeque.fromList([start]);
  let positionToSteps = ref(Map.Poly.singleton(~key=start, ~value=0));
  while (Map.get(positionToSteps^, finish)->Option.isNone
         && !MutableDeque.isEmpty(unexplored)) {
    let position = MutableDeque.popLeft(unexplored)->Option.getExn;
    availableSteps(maze, portals, position)
    ->Array.filter(~f=newPosition =>
        !Map.includes(positionToSteps^, newPosition)
      )
    ->Array.forEach(~f=newPosition => {
        MutableDeque.pushRight(unexplored, newPosition);
        let steps = Map.get(positionToSteps^, position)->Option.getExn;
        positionToSteps :=
          Map.add(positionToSteps^, ~key=newPosition, ~value=steps + 1);
      });
  };
  Map.get(positionToSteps^, finish);
};

let testMaze = {|         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
DE..#######...###.#
  #.#########.###.#
FG..#########.....#
  ###########.#####
             Z
             Z       |};

let part_1 = () => {
  print("Part 1");
  let (maze, portals) = parse(input);
  let start =
    Map.get(portals, "AA")
    ->Option.bind(~f=List.head)
    ->Option.getExn
    ->Compass.step(S);

  let finish =
    Map.get(portals, "ZZ")
    ->Option.bind(~f=List.head)
    ->Option.getExn
    ->Compass.step(S);

  minSteps(maze, portals, start, finish)->print;
};

let availableSteps =
    (maze, portals, (position, level)): array(((int, int), int)) => {
  Array.filterMap(Compass.directions, ~f=(direction) =>
    (
      {
        let entrance = Compass.step(position, direction);
        switch (Matrix.getAt(maze, entrance)) {
        | None
        | Some(" ")
        | Some("#") => None
        | Some(".") => Some((entrance, level))
        | Some(portalNameA) =>
          let portalNameB =
            Matrix.get(maze, Compass.step(entrance, direction));
          let name = portalName(portalNameA, portalNameB, direction);
          switch (Map.get(portals, name)) {
          | Some([orange, blue]) =>
            let exit = entrance == orange ? blue : orange;
            let exitVector =
              Array.find(Compass.directions, ~f=direction => {
                Matrix.getAt(maze, Compass.step(exit, direction))
                == Some(".")
              });
            Option.bind(
              exitVector,
              ~f=direction => {
                let (x, y) = entrance;
                let newLevel =
                  if ((y >= 3 && y < Array.length(maze) - 3)
                      && x >= 3
                      && x < Array.length(maze[y])
                      - 3) {
                    level + 1;
                  } else {
                    level - 1;
                  };
                if (level < 0) {
                  None;
                } else {
                  Some((Compass.step(exit, direction), newLevel));
                };
              },
            );
          | _ => None
          };
        };
      }:
        option(((int, int), int))
    )
  );
};

let minSteps = (maze, portals, start, finish) => {
  let unexplored = MutableDeque.fromList([start]);
  let positionToSteps = ref(Map.Poly.singleton(~key=start, ~value=0));
  while (Map.get(positionToSteps^, finish)->Option.isNone
         && !MutableDeque.isEmpty(unexplored)) {
    let position = MutableDeque.popLeft(unexplored)->Option.getExn;
    availableSteps(maze, portals, position)
    ->Array.filter(~f=newPosition =>
        !Map.includes(positionToSteps^, newPosition)
      )
    ->Array.forEach(~f=newPosition => {
        MutableDeque.pushRight(unexplored, newPosition);
        let steps = Map.get(positionToSteps^, position)->Option.getExn;
        positionToSteps :=
          Map.add(positionToSteps^, ~key=newPosition, ~value=steps + 1);
      });
  };
  Map.get(positionToSteps^, finish);
};

let part_2 = {
  print("Part 2");
  let (maze, portals) = parse(input);
  let start =
    Map.get(portals, "AA")
    ->Option.bind(~f=List.head)
    ->Option.getExn
    ->Compass.step(S);

  let finish =
    Map.get(portals, "ZZ")
    ->Option.bind(~f=List.head)
    ->Option.getExn
    ->Compass.step(S);

  minSteps(maze, portals, (start, 0), (finish, 0))->print;
};