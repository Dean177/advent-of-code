open Standard;
open AdventOfCode;

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
    type t = (Int64.t => unit) => unit;

    let list = (list): t => {
      let inputs = ref(list);
      continue => {
        let (input, rest) =
          switch (inputs^) {
          | [input, ...rest] => (input, rest)
          | _ => raise(ReadFromEmptyInput)
          };
        inputs := rest;
        continue(Int64.of_int(input));
      };
    };
  };

  module Output = {
    type t = (Int64.t, unit => unit) => unit;

    let list = (outputs, value, conntinue) => {
      outputs := [value, ...outputs^];
      conntinue();
    };

    let console: t =
      (value, continue) => {
        print(("Output", value));
        continue();
      };
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

  let run = (~debug=false, computer) => {
    let log = value =>
      if (debug) {
        print(value);
      };

    let step = computer => {
      let (a, b, c, opcode) = computer->instruction;
      let position = computer.position;
      switch (opcode) {
      | 1 =>
        let first = computer->read(c, position + 1);
        let second = computer->read(b, position + 2);
        computer->write(a, position + 3, Int64.add(first, second));
        computer->goto(position + 4);
        log(("add", first, second));
        Async.return(`Run);
      | 2 =>
        let first = computer->read(c, position + 1);
        let second = computer->read(b, position + 2);
        computer->write(a, position + 3, Int64.mul(first, second));
        computer->goto(position + 4);
        log(("mul", first, second));
        Async.return(`Run);
      | 3 =>
        Async.make(continue => {
          computer.input(value => {
            computer->write(c, position + 1, value);
            computer->goto(position + 2);
            log(("inp", value->Int64.to_string));
            continue(`Run);
          })
        })
      | 4 =>
        let value = computer->read(c, position + 1);
        log(("out", value->Int64.to_string));
        Async.make(continue => {
          computer.output(
            value,
            () => {
              computer->goto(position + 2);
              continue(`Run);
            },
          )
        });
      | 5 =>
        let value = computer->read(c, position + 1);
        let destination =
          if (value != Int64.zero) {
            Int64.to_int(computer->read(b, position + 2));
          } else {
            position + 3;
          };
        log(("jump-not-zero", value, destination));
        computer->goto(destination);
        Async.return(`Run);
      | 6 =>
        let value = computer->read(c, position + 1);
        let destination =
          if (value == Int64.zero) {
            Int64.to_int(computer->read(b, position + 2));
          } else {
            position + 3;
          };
        log(("jump-if-zero", value, destination));
        computer->goto(destination);
        Async.return(`Run);
      | 7 =>
        let first = computer->read(c, position + 1);
        let second = computer->read(b, position + 2);
        computer->write(
          a,
          position + 3,
          Int64.of_int(first < second ? 1 : 0),
        );
        computer->goto(position + 4);
        Async.return(`Run);
      | 8 =>
        let first = computer->read(c, position + 1);
        let second = computer->read(b, position + 2);
        computer->write(
          a,
          position + 3,
          Int64.of_int(first == second ? 1 : 0),
        );
        computer->goto(position + 4);
        Async.return(`Run);
      | 9 =>
        let first = computer->read(c, position + 1);
        computer.relative = computer.relative + Int64.to_int(first);
        computer->goto(position + 2);
        Async.return(`Run);
      | 99 => Async.return(`Halt)
      | code => raise(UnrecognizedOpcode(code))
      };
    };

    let rec loop = () => {
      Async.bind(
        step(computer),
        ~f=
          fun
          | `Run => loop()
          | `Halt => Async.return(),
      );
    };

    loop();
  };
};

let input = read("./Day13.txt");

type outputState =
  | ReadX
  | ReadY(Int64.t)
  | Draw(Int64.t, Int64.t);

type tile =
  | Empty
  | Wall
  | Block
  | Paddle
  | Ball;

let printTile =
  fun
  | Empty => {js|⬜️|js}
  | Wall => {js|🔲|js}
  | Block => {js|⬛️|js}
  | Paddle => {js|🏓|js}
  | Ball => {js|🏀|js};

let part_1 = () => {
  let outputState = ref(ReadX);

  let screen = ref(Map.Poly.empty());

  let computer =
    Computer.make(
      Computer.Program.parse(input),
      ~input=Computer.Input.list([]),
      ~output=(value, continue) => {
      switch (outputState^) {
      | ReadX => outputState := ReadY(value)
      | ReadY(x) => outputState := Draw(x, value)
      | Draw(x, y) =>
        let tile =
          switch (Int64.to_int(value)) {
          | 0 => Empty
          | 1 => Wall
          | 2 => Block
          | 3 => Paddle
          | 4 => Ball
          | _ =>
            raise(
              Invalid_argument(
                "Unexpected color: " ++ Int64.to_string(value),
              ),
            )
          };
        screen := Map.add(screen^, ~key=(x, y), ~value=tile);
        outputState := ReadX;
        continue();
      }
    });

  Computer.run(computer)
  ->Async.bind(~f=() => {
      Map.values(screen^)->List.count(~f=tile => tile == Block)->print;
      Async.return();
    });
};

let show = (screen: Map.Poly.t((Int64.t, Int64.t), tile)): string => {
  let paints =
    Map.toArray(screen)
    ->Array.map(~f=Tuple.mapFirst(~f=Tuple.mapAll(~f=Int64.to_int)));
  let (minx, maxx) =
    extent(Array.map(paints, ~f=(((x, _), _)) => x), ~compare)
    ->Option.getExn;
  let (miny, maxy) =
    extent(Array.map(paints, ~f=(((_, y), _)) => y), ~compare)
    ->Option.getExn;

  let message =
    Array.initialize(~length=1 + maxy - miny, ~f=_ => {
      Array.initialize(~length=1 + maxx - minx, ~f=_ => "🔲")
    });

  Array.forEach(paints, ~f=(((x, y), tile)) => {
    message[y - miny][x - minx] = printTile(tile)
  });
  message
  ->Array.map(~f=row => Array.toList(row)->String.join(~sep=""))
  ->Array.toList
  ->String.join(~sep="\n");
};

let part_2 = {
  let program = Computer.Program.parse(input);
  program[0] = Int64.of_int(2);
  let outputState = ref(ReadX);
  let screen = ref(Map.Poly.empty());
  let score = ref(Int64.zero);
  let computer =
    Computer.make(
      program,
      ~input=
        continue => {
          print("read input");
          let tiles = Map.toArray(screen^);
          let ((ballX, _), _) =
            Array.find(tiles, ~f=((_, tile)) => tile == Ball)->Option.getExn;
          let ((paddleX, _), _) =
            Array.find(tiles, ~f=((_, tile)) => tile == Paddle)
            ->Option.getExn;

          LogUpdate.update(show(screen^));
          Js.Global.setTimeout(
            () => {
              continue(
                if (ballX < paddleX) {
                  Int64.of_int(-1);
                } else if (ballX > paddleX) {
                  Int64.of_int(1);
                } else {
                  Int64.zero;
                },
              )
            },
            60,
          )
          ->ignore;
        },
      ~output=
        (value, continue) => {
          switch (outputState^) {
          | ReadX => outputState := ReadY(value)
          | ReadY(x) => outputState := Draw(x, value)
          | Draw(x, y) =>
            switch (Int64.to_int(x), Int64.to_int(y)) {
            | ((-1), 0) => score := value
            | _ =>
              let tile =
                switch (Int64.to_int(value)) {
                | 0 => Empty
                | 1 => Wall
                | 2 => Block
                | 3 => Paddle
                | 4 => Ball
                | _ =>
                  raise(
                    Invalid_argument(
                      "Unexpected color: " ++ Int64.to_string(value),
                    ),
                  )
                };
              screen := Map.add(screen^, ~key=(x, y), ~value=tile);
            };

            outputState := ReadX;
          };
          continue();
        },
    );

  print(("Startng", Int64.to_string(score^)));
  Computer.run(computer)
  ->Async.bind(~f=() => {
      print(("Game over", Int64.to_string(score^)));
      Async.return();
    });
};