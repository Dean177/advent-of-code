open Standard;
open AdventOfCode;

module Computer = Day05.Computer;
module Input = Day05.Input;
module Output = Day05.Output;

let input = read("Day07.txt");

let part1 = () => {
  let maxSignal = ref(Int.minimumValue);
  Stream.from(permutations([0, 1, 2, 3, 4]))
  |> Stream.iter(phases => {
       let phases = Array.fromList(phases);
       let value =
         Belt.Array.reduce(
           phases,
           0,
           (inputSignal, phase) => {
             let (outputs, output) = Output.list();
             let computer =
               Computer.make(
                 Day05.parse(input),
                 ~input=Input.list([phase, inputSignal]),
                 ~output,
               );
             let _ = Computer.run(computer);
             (outputs^)->List.head->Option.getExn;
           },
         );
       if (value > maxSignal^) {
         maxSignal := value;
       };
     });
  print(("Result:", maxSignal^));
};

module AsyncComputer = {
  type exn +=
    | UnrecognizedMode(int)
    | UnrecognizedOpcode(int, int, array(int))
    | ReadFromEmptyInput;

  type mode =
    | Positional
    | Immediate;

  type opcode = int;

  type instruction = (mode, mode, mode, opcode);

  let parseMode = int =>
    switch (int) {
    | 0 => Positional
    | 1 => Immediate
    | other => raise(UnrecognizedMode(other))
    };

  type t = {
    mutable position: int,
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

  let divRem = (n, ~by) => (Int.divide(n, ~by), Int.remainder(n, ~by));

  let instruction = ({memory, position, _}) => {
    let instruction = memory[position];
    let opcode = Int.remainder(instruction, ~by=100);
    let (a, instruction) = divRem(instruction, ~by=10_000);
    let (b, instruction) = divRem(instruction, ~by=1_000);
    let c = Int.divide(instruction, ~by=100);
    (a, b, c, opcode);
  };

  let read = ({memory, _}, mode, position) => {
    let value = memory[position];
    switch (parseMode(mode)) {
    | Immediate => value
    | Positional => memory[value]
    };
  };

  let goto = (computer, position) => computer.position = position;

  let format = ({memory, _}) =>
    String.join(Array.map(memory, ~f=Int.toString)->Array.toList, ~sep=",");

  let rec run = computer => {
    let (_a, b, c, opcode) = computer->instruction;
    let position = computer.position;
    switch (opcode) {
    | 1 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      let target = computer.memory[position + 3];
      computer.memory[target] = first + second;
      //      print(("add", first, second, target));
      computer->goto(position + 4);
      run(computer);
    | 2 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      let target = computer.memory[position + 3];
      computer.memory[target] = first * second;
      //      print(("mul", first, second, target));
      computer->goto(position + 4);
      run(computer);
    | 3 =>
      let value = computer.input();
      let target = computer.memory[position + 1];
      computer.memory[target] = value;
      //      print(("inp", value, target));
      computer->goto(position + 2);
      run(computer);
    | 4 =>
      let value = computer->read(c, position + 1);
      computer.output(value);
      //      print(("out", value));
      computer->goto(position + 2);
      `HaltOnOutput;
    | 5 =>
      let value = computer->read(c, position + 1);
      let destination =
        if (value != 0) {
          computer->read(b, position + 2);
        } else {
          position + 3;
        };
      computer->goto(destination);
      run(computer);
    | 6 =>
      let value = computer->read(c, position + 1);
      let destination =
        if (value == 0) {
          computer->read(b, position + 2);
        } else {
          position + 3;
        };
      computer->goto(destination);
      run(computer);
    | 7 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      let target = computer.memory[position + 3];
      let result = first < second ? 1 : 0;
      computer.memory[target] = result;
      computer->goto(position + 4);
      run(computer);
    | 8 =>
      let first = computer->read(c, position + 1);
      let second = computer->read(b, position + 2);
      let target = computer.memory[position + 3];
      let result = first == second ? 1 : 0;
      computer.memory[target] = result;
      computer->goto(position + 4);
      run(computer);
    | 99 => `Halt
    | code => raise(UnrecognizedOpcode(code, position, computer.memory))
    };
  };
};

let constantThenQueueInput =
    (phase, queue: Belt.MutableQueue.t(int)): (unit => int) => {
  let read = ref(false);

  () =>
    if (read^) {
      Belt.MutableQueue.popExn(queue);
    } else {
      read := true;
      phase;
    };
};

let part2 = {
  let maxSignal = ref(Int.minimumValue);
  Stream.from(permutations([5, 6, 7, 8, 9]))
  |> Stream.iter(phaseSettings => {
       let phaseSettings = Array.fromList(phaseSettings);
       let inputs = Belt.MutableQueue.fromArray([|0|]);
       let machines =
         Array.initialize(~length=5, ~f=i =>
           AsyncComputer.make(
             Day05.parse(input),
             ~input=constantThenQueueInput(phaseSettings[i], inputs),
             ~output=output =>
             Belt.MutableQueue.add(inputs, output)
           )
         );

       let rec loop = () => {
         AsyncComputer.run(machines[0])->ignore;
         AsyncComputer.run(machines[1])->ignore;
         AsyncComputer.run(machines[2])->ignore;
         AsyncComputer.run(machines[3])->ignore;
         switch (AsyncComputer.run(machines[4])) {
         | `Halt =>
           let last =
             Belt.MutableQueue.toArray(inputs)->Array.last->Option.getExn;
           if (last > maxSignal^) {
             maxSignal := last;
           };
         | `HaltOnOutput => loop()
         };
       };
       loop();
     });
  print(("Result:", maxSignal));
};