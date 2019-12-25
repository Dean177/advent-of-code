open Standard;
open AdventOfCode;

module Computer = Day13.Computer;

let computer =
  Computer.make(
    Computer.Program.parse(read("./Day25.txt")),
    ~output=Computer.Output.stdout,
    ~input={
      let inputBuffer = MutableQueue.make();

      Readline.readline(userInput => {
        String.split(userInput, ~on="")
        ->Array.forEach(~f=char =>
            Char.fromString(char)->Option.getExn->Char.toCode->Int64.of_int
            |> MutableQueue.add(inputBuffer)
          );
        MutableQueue.add(inputBuffer, 10L);
      });

      continue => {
        let rec loop = () =>
          if (MutableQueue.isEmpty(inputBuffer)) {
            Js.Global.setTimeout(loop, 20)->ignore;
          } else {
            continue(MutableQueue.popExn(inputBuffer));
          };

        loop();
      };
    },
  );

Computer.run(computer)
->Async.bind(~f=() => {
    Readline.close();
    Async.return();
  });