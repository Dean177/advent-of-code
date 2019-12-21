open Standard;
open AdventOfCode;

let input = read("Day21.txt");

module Computer = Day09.Computer;

module SpringScript = {
  let toAscii = instructions =>
    String.split(instructions, ~on="")
    ->Array.map(~f=character =>
        Char.fromString(character)->Option.getExn->Char.toCode
      )
    ->Array.toList;
};

// @#### -> N
// @###. -> N
// @##.# -> Y
// @#.## -> Y
// @#.#. -> N
// @##.. -> N
// @#..# -> Y
// @##.. -> N
// @#... -> N
// @...# -> Y
// Jump when something is missing, and there is something to land on

let instructions = "NOT A T
OR T J
NOT B T
OR T J
NOT C T
OR T J
NOT D T
NOT T T
AND T J
WALK
";

let part_1 = () => {
  Computer.make(
    Computer.Program.parse(input),
    ~input=Computer.Input.list(SpringScript.toAscii(instructions)),
    ~output=Computer.Output.stdout,
  )
  |> Computer.runToHalt;
};

let instructions = "NOT I T
NOT T J
OR F J
AND E J
OR H J
AND D J
NOT A T
NOT T T
AND B T
AND C T
NOT T T
AND T J
RUN
";

let part_2 = {
  Computer.make(
    Computer.Program.parse(input),
    ~input=Computer.Input.list(SpringScript.toAscii(instructions)),
    ~output=Computer.Output.stdout,
  )
  |> Computer.runToHalt;
};