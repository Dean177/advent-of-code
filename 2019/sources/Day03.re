open Standard;
open AdventOfCode;
open Test;

type direction =
  | U
  | D
  | L
  | R;

type instruction = (direction, int);

let parseWire = input =>
  String.split(input, ~on=",")
  ->List.map(~f=string => {
      let direction =
        switch (String.slice(string, ~from=0, ~to_=1)) {
        | "U" => U
        | "D" => D
        | "L" => L
        | "R" => R
        | other => raise(Invalid_argument(other))
        };
      let magnitude =
        String.slice(string, ~from=1, ~to_=String.length(string))
        ->Int.fromString
        ->Option.getExn;

      (direction, magnitude);
    });

let parse = input =>
  String.split(input, ~on="\n")
  ->List.map(~f=parseWire)
  ->Tuple.fromList
  ->Option.getExn;

let closestIntersection = input => {
  let (first, second) =
    parse(input)
    ->Tuple.mapAll(~f=instructions => {
        let points = ref(Set.Poly.empty());
        let position = ref((0, 0));
        List.forEach(instructions, ~f=((direction, steps)) =>
          Array.range(steps)
          ->Array.forEach(~f=_ => {
              let (x, y) = position^;
              position :=
                (
                  switch (direction) {
                  | U => (x, y + 1)
                  | D => (x, y - 1)
                  | L => (x - 1, y)
                  | R => (x + 1, y)
                  }
                );
              points := Set.add(points^, position^);
            })
        );
        points^;
      });

  Set.intersection(first, second)
  ->Set.toList
  ->List.map(~f=((x, y)) => Int.absolute(x) + Int.absolute(y))
  ->List.minimum
  ->Option.getExn;
};

test(expect => {
  let input = "R8,U5,L5,D3\nU7,R6,D4,L4";
  closestIntersection(input)->expect->toEqual(6);
});

test(expect => {
  let input = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83";
  closestIntersection(input)->expect->toEqual(159);
});

test(expect => {
  let input = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7";
  closestIntersection(input)->expect->toEqual(135);
});

//read("./Day03.txt")->closestIntersection->Console.log;

let fewestCombinedSteps = input => {
  let (first, second) =
    parse(input)
    ->Tuple.mapAll(~f=instructions => {
        let points = ref(Map.Poly.empty());
        let position = ref((0, 0));
        let steps = ref(0);
        List.forEach(instructions, ~f=((direction, magnitude)) =>
          Array.range(magnitude)
          ->Array.forEach(~f=_ => {
              let (x, y) = position^;
              position :=
                (
                  switch (direction) {
                  | U => (x, y + 1)
                  | D => (x, y - 1)
                  | L => (x - 1, y)
                  | R => (x + 1, y)
                  }
                );
              steps := steps^ + 1;
              points := Map.add(points^, ~key=position^, ~value=steps^);
            })
        );
        points^;
      });

  Map.merge(first, second, ~f=(_, pointA, pointB) =>
    switch (pointA, pointB) {
    | (Some(stepsToA), Some(stepsToB)) => Some(stepsToA + stepsToB)
    | _ => None
    }
  )
  ->Map.values
  ->List.minimum
  ->Option.getExn;
};

test(expect => {
  let input = "R8,U5,L5,D3\nU7,R6,D4,L4";
  fewestCombinedSteps(input)->expect->toEqual(30);
});

test(expect => {
  let input = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83";
  fewestCombinedSteps(input)->expect->toEqual(610);
});

test(expect => {
  let input = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7";
  fewestCombinedSteps(input)->expect->toEqual(410);
});

read("./Day03.txt")->fewestCombinedSteps->print;