open Standard;
open AdventOfCode;

let input = read("./Day10.txt");

let field =
  String.split(input, ~on="\n")
  ->List.map(~f=line => String.split(line, ~on="")->Array.fromList)
  ->Array.fromList;

let columns = Array.length(field) - 1;
let rows = Array.length(field[0]) - 1;

let get = (asteroids, (x, y)) => asteroids[y][x];

let isAsteroid = (asteroids, position) => get(asteroids, position) == "#";

let vector = ((x1, y1), (x2, y2)) => {
  let dx = x2 - x1;
  let dy = y2 - y1;
  let denominator = gcd(Int.absolute(dx), Int.absolute(dy));
  (dx / denominator, dy / denominator);
};

let part1 = () => {
  let maximumAsteroidCount = ref((0, (0, 0)));
  for (x1 in 0 to rows) {
    for (y1 in 0 to columns) {
      let basePosition = (x1, y1);
      if (field->isAsteroid(basePosition)) {
        let vectors = ref(Set.Poly.empty());
        for (x2 in 0 to rows) {
          for (y2 in 0 to columns) {
            let asteroidPosition = (x2, y2);
            if (asteroidPosition != basePosition
                && field->isAsteroid(asteroidPosition)) {
              vectors :=
                Set.add(vectors^, vector(basePosition, asteroidPosition));
            };
          };
        };
        let visibleAsteroids = Set.length(vectors^);
        if (visibleAsteroids > Tuple.first(maximumAsteroidCount^)) {
          maximumAsteroidCount := (visibleAsteroids, basePosition);
        };
      };
    };
  };

  print(maximumAsteroidCount^);
};

let toRadians = ((x, y)) =>
  Float.atan2(~x=Float.fromInt(x), ~y=Float.fromInt(y));

let rec toPositiveRotation = angle =>
  if (angle >= 0.) {
    angle;
  } else {
    toPositiveRotation(2. *. Float.pi +. angle);
  };

let toClockwiseUp = radians => toPositiveRotation(radians +. Float.pi /. 2.);

let part2 = {
  let basePosition = (17, 23);
  let vectorToCount = ref(Map.Poly.empty());
  for (x2 in 0 to rows) {
    for (y2 in 0 to columns) {
      let asteroidPosition = (x2, y2);
      if (asteroidPosition != basePosition
          && field->isAsteroid(asteroidPosition)) {
        vectorToCount :=
          Map.update(
            vectorToCount^,
            ~key=vector(basePosition, asteroidPosition),
            ~f=value =>
            switch (value) {
            | None => Some([asteroidPosition])
            | Some(asteroids) => Some([asteroidPosition, ...asteroids])
            }
          );
      };
    };
  };

  let radiansToAsteroids =
    Map.toArray(vectorToCount^)
    ->Array.map(~f=((vector, asteroids)) =>
        (
          toRadians(vector)->toClockwiseUp,
          List.sortBy(asteroids, ~f=((x, y)) =>
            (17 - x) * (17 - x) + (23 - y) * (23 - y)
          ),
        )
      );

  Array.sort(radiansToAsteroids, ~compare=((angle1, _), (angle2, _)) =>
    compare(angle1, angle2)
  );

  field[23][17] = "X";

  let index = ref(0);
  let destroyed = ref(0);
  while (destroyed^ < 199) {
    let (radians, asteroids) = radiansToAsteroids[index^];
    switch (asteroids) {
    | [] => ()
    | [asteroid, ...asteroids] =>
      let (x, y) = asteroid;
      field[y][x] = Int.toString(destroyed^ + 1);
      destroyed := destroyed^ + 1;
      radiansToAsteroids[index^] = (radians, asteroids);
    };
    index := index^ < Array.length(radiansToAsteroids) ? index^ + 1 : 0;
  };
  //      print(
  //        field
  //        ->Array.map(~f=row => Array.toList(row)->String.join(~sep=""))
  //        ->Array.toList
  //        ->String.join(~sep="\n"),
  //      );

  print(Tuple.second(radiansToAsteroids[index^])->List.head);
};