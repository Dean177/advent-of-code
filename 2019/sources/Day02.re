open Standard;
open AdventOfCode;
open Test;

let input = read("./Day02.txt");

let parse = input =>
  String.split(input, ~on=",")
  ->List.map(~f=string => Int.fromString(string)->Option.getExn)
  ->Array.fromList;

let format = memory =>
  String.join(Array.map(memory, ~f=Int.toString)->Array.toList, ~sep=",");

let rec run = (memory, position) => {
  switch (memory[position]) {
  | 1 =>
    memory[memory[position + 3]] =
      memory[memory[position + 1]] + memory[memory[position + 2]];
    run(memory, position + 4);
  | 2 =>
    memory[memory[position + 3]] =
      memory[memory[position + 1]] * memory[memory[position + 2]];
    run(memory, position + 4);
  | 99 => memory
  | _ =>
    raise(
      Invalid_argument(
        "Index" ++ Int.toString(position) ++ ", " ++ format(memory),
      ),
    )
  };
};

test(expect =>
  parse("1,0,0,0,99")->run(0)->format->expect->toEqual("2,0,0,0,99")
);
test(expect =>
  parse("2,3,0,3,99")->run(0)->format->expect->toEqual("2,3,0,6,99")
);
test(expect =>
  parse("2,4,4,5,99,0")->run(0)->format->expect->toEqual("2,4,4,5,99,9801")
);
test(expect =>
  parse("1,1,1,4,99,5,6,0,99")
  ->run(0)
  ->format
  ->expect
  ->toEqual("30,1,1,4,2,5,6,0,99")
);

let part1 = () => {
  let memory = parse(input);
  memory[1] = 12;
  memory[2] = 2;
  let _ = run(memory, 0);

  print(format(memory));
};

let part2 = () => {
  for (noun in 0 to 100) {
    for (verb in 0 to 100) {
      let memory = parse(input);
      memory[1] = noun;
      memory[2] = verb;
      let _ = run(memory, 0);
      let output = memory[0];
      if (output == 19690720) {
        print(100 * noun + verb);
      };
    };
  };
};