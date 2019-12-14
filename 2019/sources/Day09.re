open Standard;
open AdventOfCode;
open Test;

let input = read("./Day09.txt");

module Computer = {
  type exn +=
    | UnrecognizedMode(int)
    | UnrecognizedOpcode(int)
    | ReadFromEmptyInput
    | WroteMemoryInImmediateMode;

  type mode =
    | Positional
    | Immediate
    | Relative;

  type opcode = int;

  type instruction = (mode, mode, mode, opcode);

  let parseMode = int =>
    switch (int) {
    | 0 => Positional
    | 1 => Immediate
    | 2 => Relative
    | other => raise(UnrecognizedMode(other))
    };

  module Program = {
    type t = array(Int64.t);
    let parse = input =>
      String.split(input, ~on=",")
      ->List.map(~f=Int64.of_string)
      // TODO Reason parses Int32 by default !?
      // string =>
      //          Int.fromString(string)
      //          ->Option.getOrFailWith(~exn=Invalid_argument(string))
      ->Array.fromList;
  };

  module Memory = {
    type t = Hashtbl.t(int, Int64.t);
    let make = program => {
      let memory = Hashtbl.create(Array.length(program));
      for (i in 0 to Array.length(program) - 1) {
        Hashtbl.add(memory, i, program[i]);
      };
      memory;
    };

    let read = (t, address) => {
      Hashtbl.find_opt(t, address)->Option.get(~default=Int64.of_int(0));
    };

    let write = (t, address, value) => {
      Hashtbl.add(t, address, value);
    };
  };

  module Input = {
    type t = unit => Int64.t;

    let list = (list): t => {
      let inputs = ref(list);
      () => {
        let (input, rest) =
          switch (inputs^) {
          | [input, ...rest] => (input, rest)
          | _ => raise(ReadFromEmptyInput)
          };
        inputs := rest;
        Int64.of_int(input);
      };
    };
  };

  module Output = {
    type t = Int64.t => unit;

    let list = (outputs, value) => {
      outputs := [value, ...outputs^];
    };

    let console: t = value => print(("Output", value));
  };

  type t = {
    mutable position: int,
    mutable relative: int,
    memory: Memory.t,
    input: Input.t,
    output: Output.t,
  };

  let make = (program, ~input, ~output) => {
    position: 0,
    relative: 0,
    memory: Memory.make(program),
    input,
    output,
  };

  let divRem = (n, ~by) => (Int64.div(n, by), Int64.rem(n, by));

  let instruction = ({memory, position, _}) => {
    let instruction = Memory.read(memory, position);
    let (a, instruction) = divRem(instruction, ~by=Int64.of_int(10_000));
    let (b, instruction) = divRem(instruction, ~by=Int64.of_int(1_000));
    let (c, opcode) = divRem(instruction, ~by=Int64.of_int(100));
    (
      Int64.to_int(a),
      Int64.to_int(b),
      Int64.to_int(c),
      Int64.to_int(opcode),
    );
  };

  let read = ({memory, relative, _}, mode, position) => {
    let location = Memory.read(memory, position);
    //    print(("read", mode, position));
    switch (parseMode(mode)) {
    | Immediate => location
    | Positional => Memory.read(memory, location->Int64.to_int)
    | Relative => Memory.read(memory, location->Int64.to_int + relative)
    };
  };

  let write = ({memory, relative, _}, mode, position, value) => {
    //    print(("write", mode, position));
    let location = Memory.read(memory, position)->Int64.to_int;
    switch (parseMode(mode)) {
    | Immediate => raise(WroteMemoryInImmediateMode)
    | Positional => Memory.write(memory, location, value)
    | Relative => Memory.write(memory, location + relative, value)
    };
  };

  let goto = (computer, position) => computer.position = position;

  let rec run = computer => {
    let (a, b, c, opcode) = computer->instruction;
    let position = computer.position;
    switch (opcode) {
    | 1 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      computer->write(a, position + 3, Int64.add(first, second));
      computer->goto(position + 4);
      //      print(("add", first, second));
      run(computer);
    | 2 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      computer->write(a, position + 3, Int64.mul(first, second));
      computer->goto(position + 4);
      //      print(("mul", first, second));
      run(computer);
    | 3 =>
      let value = computer.input();
      computer->write(c, position + 1, value);
      computer->goto(position + 2);
      //      print(("inp", value, target));
      run(computer);
    | 4 =>
      let value = computer->read(c, position + 1);
      computer.output(value);
      //      print(("out", value->Int64.to_string));
      computer->goto(position + 2);
      `HaltOnOutput;
    | 5 =>
      let value = computer->read(c, position + 1);
      let destination =
        if (value != Int64.zero) {
          Int64.to_int(computer->read(b, position + 2));
        } else {
          position + 3;
        };
      computer->goto(destination);
      run(computer);
    | 6 =>
      let value = computer->read(c, position + 1);
      let destination =
        if (value == Int64.zero) {
          Int64.to_int(computer->read(b, position + 2));
        } else {
          position + 3;
        };
      computer->goto(destination);
      run(computer);
    | 7 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      computer->write(a, position + 3, Int64.of_int(first < second ? 1 : 0));
      computer->goto(position + 4);
      run(computer);
    | 8 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      computer->write(
        a,
        position + 3,
        Int64.of_int(first == second ? 1 : 0),
      );
      computer->goto(position + 4);
      run(computer);
    | 9 =>
      let first = computer->read(c, position + 1);
      computer.relative = computer.relative + Int64.to_int(first);
      computer->goto(position + 2);
      run(computer);
    | 99 => `Halt
    | code => raise(UnrecognizedOpcode(code))
    };
  };

  let rec runToHalt = computer => {
    switch (run(computer)) {
    | `HaltOnOutput => runToHalt(computer)
    | `Halt => ()
    };
  };
};

let runToHalt = (~inputs=[], program) => {
  let outputs = ref([]);
  let computer =
    Computer.make(
      program,
      ~input=Computer.Input.list(inputs),
      ~output=Computer.Output.list(outputs),
    );
  let rec loop = () => {
    switch (computer->Computer.run) {
    | `HaltOnOutput => loop()
    | `Halt => List.reverse(outputs^)
    };
  };
  loop();
};

test(expect => {
  let program =
    Computer.Program.parse(
      "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99",
    );
  expect(runToHalt(program)->Array.fromList)->toEqual(program);
});

test(expect => {
  let program = Computer.Program.parse("1102,34915192,34915192,7,4,7,99,0");
  expect(
    runToHalt(program)
    ->List.head
    ->Option.getExn
    ->Int64.to_string
    ->String.length,
  )
  ->toEqual(16);
});

test(expect => {
  let program = Computer.Program.parse("104,1125899906842624,99");
  expect(runToHalt(program)->List.head->Option.getExn->Int64.to_string)
  ->toEqual("1125899906842624");
});

let part_1 = () => {
  print("Part 1");
  print(
    runToHalt(~inputs=[1], Computer.Program.parse(input))
    ->List.map(~f=Int64.to_string),
  );
};

let part_2 = () => {
  print("Part 2");
  print(
    runToHalt(~inputs=[2], Computer.Program.parse(input))
    ->List.map(~f=Int64.to_string),
  );
};

let _ = ();