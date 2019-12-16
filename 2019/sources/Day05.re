open Standard;
open AdventOfCode;
open Test;

type mode =
  | Positional
  | Immediate;

type opcode = int;

type instruction = (mode, mode, mode, opcode);

let parse = input =>
  String.split(input, ~on=",")
  ->Array.map(~f=string => Int.fromString(string)->Option.getExn);

let divRem = (n, ~by) => (Int.divide(n, ~by), Int.remainder(n, ~by));

type exn +=
  | UnrecognizedMode(int)
  | UnrecognizedOpcode(int, int, array(int))
  | ReadFromEmptyInput;

let parseMode = int =>
  switch (int) {
  | 0 => Positional
  | 1 => Immediate
  | other => raise(UnrecognizedMode(other))
  };

module Computer = {
  type t = {
    position: int,
    memory: array(int),
    input: unit => int,
    output: int => unit,
  };

  let make = (program, ~input, ~output) => {
    position: 0,
    memory: program,
    input,
    output,
  };

  let instruction = ({memory, position, _}) => {
    let instruction = memory[position];
    let opcode = Int.remainder(instruction, ~by=100);
    let (a, instruction) = divRem(instruction, ~by=10_000);
    let (b, instruction) = divRem(instruction, ~by=1_000);
    let c = Int.divide(instruction, ~by=100);
    (a, b, c, opcode);
  };

  let read = ({memory, _}, mode, position) => {
    //    print(("read", mode, memory[position]));
    let value = memory[position];
    switch (parseMode(mode)) {
    | Immediate => value
    | Positional => memory[value]
    };
  };

  let goto = (computer, position) => {...computer, position};

  let format = ({memory, _}) =>
    String.join(Array.map(memory, ~f=Int.toString)->Array.toList, ~sep=",");

  let rec run = computer => {
    let (_a, b, c, opcode) = computer->instruction;
    let position = computer.position;
    //    print(("position", position));
    //    print((
    //      "memory",
    //      Array.slice(computer.memory, ~from=position, ~to_=position + 4),
    //    ));
    switch (opcode) {
    | 1 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      let target = computer.memory[position + 3];
      computer.memory[target] = first + second;
      //      print(("add", first, second, target));
      run(computer->goto(position + 4));
    | 2 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      let target = computer.memory[position + 3];
      computer.memory[target] = first * second;
      //      print(("mul", first, second, target));
      run(computer->goto(position + 4));
    | 3 =>
      let value = computer.input();
      let target = computer.memory[position + 1];
      computer.memory[target] = value;
      print(("inp", value, target));
      run(computer->goto(position + 2));
    | 4 =>
      let value = computer->read(c, position + 1);
      computer.output(value);
      print(("out", value));
      run(computer->goto(position + 2));
    | 5 =>
      let value = computer->read(c, position + 1);
      let destination =
        if (value != 0) {
          computer->read(b, position + 2);
        } else {
          position + 3;
        };
      //      print(("jmp1", value, destination));
      run(computer->goto(destination));
    | 6 =>
      let value = computer->read(c, position + 1);
      let destination =
        if (value == 0) {
          computer->read(b, position + 2);
        } else {
          position + 3;
        };
      //      print(("jmp0", value, destination));
      run(computer->goto(destination));
    | 7 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      let target = computer.memory[position + 3];
      let result = first < second ? 1 : 0;
      computer.memory[target] = result;
      //      print(("lt", first, second));
      run(computer->goto(position + 4));
    | 8 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      let target = computer.memory[position + 3];
      let result = first == second ? 1 : 0;
      computer.memory[target] = result;
      //      print(("eq", first, second));
      run(computer->goto(position + 4));
    | 99 => computer
    | code => raise(UnrecognizedOpcode(code, position, computer.memory))
    };
  };
};

module Input = {
  let list = list => {
    let inputs = ref(list);
    () => {
      let (input, rest) =
        switch (inputs^) {
        | [input, ...rest] => (input, rest)
        | _ => raise(ReadFromEmptyInput)
        };
      inputs := rest;
      input;
    };
  };
};

module Output = {
  let list = () => {
    let outputs = ref([]);
    (outputs, value => outputs := [value, ...outputs^]);
  };

  let console = value => print(("Output", value));
};
//
test(expect => {
  let (_, output) = Output.list();
  let computer =
    Computer.make(parse("1002,4,3,4,33"), ~input=Input.list([1]), ~output);
  let _ = Computer.run(computer);

  expect(Computer.format(computer))->toEqual("1002,4,3,4,99");
});

//test(expect => {
//  let (_, output) = Output.list();
//  let computer =
//    Computer.make(
//      parse("1101,100,-1,4,0"),
//      ~input=Input.list([1]),
//      ~output,
//    );
//  let _ = Computer.run(computer);
//  computer->Computer.format->expect->toEqual("1101,100,-1,4,99");
//});

let part1 = () => {
  print("part1");
  let (outputs, output) = Output.list();
  let computer =
    Computer.make(
      parse(read("./Day05.txt")),
      ~input=Input.list([1]),
      ~output,
    );

  let _ = Computer.run(computer);
  print(("Outputs", (outputs^)->List.reverse->Array.fromList));
};

let part2 = () => {
  print("part2");
  let (outputs, output) = Output.list();
  let computer =
    Computer.make(
      parse(read("./Day05.txt")),
      ~input=Input.list([5]),
      ~output,
    );

  let _ = Computer.run(computer);
  print(("Outputs", (outputs^)->List.reverse->Array.fromList));
};

//part2();

let _ = ();