open Standard;
open AdventOfCode;

let input = read("./Day17.txt");

module Computer = Day15.Computer;

let stdout = value => {
  Int64.to_int(value)
  ->Char.fromCode
  ->Option.map(~f=Char.toString)
  ->Option.get(~default=Int64.to_string(value))
  ->print_string;
};

let show = output =>
  List.reverse(output)
  ->List.map(~f=int64 =>
      Int64.to_int(int64)->Char.fromCode->Option.getExn->Char.toString
    )
  ->String.join(~sep="");

let part_1 = () => {
  let outputs = ref([]);
  let computer =
    Computer.make(
      Computer.Program.parse(input),
      ~input=Computer.Input.list([]),
      ~output=Computer.Output.list(outputs),
    );

  Computer.runToHalt(computer);

  print(show(outputs^));

  let map =
    String.split(show(outputs^), ~on="\n")
    ->Array.map(~f=String.split(~on=""));

  let isScaffold = ((x, y)) =>
    Array.getAt(map, ~index=y)
    ->Option.bind(~f=Array.getAt(~index=x))
    ->Option.map(~f=tile => tile == "#")
    ->Option.get(~default=false);

  let isIntersection = ((x, y)) =>
    Array.all(
      [|(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)|],
      ~f=isScaffold,
    );

  let intersectionPositions = ref([]);
  Array.forEachI(map, ~f=(row, y) => {
    Array.forEachI(row, ~f=(_tile, x) =>
      if (isIntersection((x, y))) {
        intersectionPositions := [(x, y), ...intersectionPositions^];
      }
    )
  });

  List.map(intersectionPositions^, ~f=Tuple.uncurry(Int.multiply))
  |> List.sum((module Int))
  |> print;
};

let part_2 = {
  let program = Computer.Program.parse(input);
  program[0] = Int64.of_int(2);
  let main = "A,A,B,C,B,C,B,C,B,A";
  let a = "R,10,L,12,R,6";
  let b = "R,6,R,10,R,12,R,6";
  let c = "R,10,L,12,L,12";

  let inputs =
    (String.join([main, a, b, c], ~sep="\n") ++ "\nn\n")
    ->String.split(~on="")
    ->Array.map(~f=string =>
        Char.fromString(string)
        ->Option.getOrFailWith(~exn=Invalid_argument(string))
        ->Char.toCode
      )
    ->Array.toList;

  Computer.make(program, ~input=Computer.Input.list(inputs), ~output=stdout)
  |> Computer.runToHalt;
};