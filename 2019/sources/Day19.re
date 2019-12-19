open Standard;
open AdventOfCode;

module Computer = Day09.Computer;

let input = read("Day19.txt");

print("Part 1");
Array.(
  bind(range(50), ~f=x =>
    bind(
      range(50),
      ~f=y => {
        let outputs = ref([]);
        Computer.make(
          Computer.Program.parse(input),
          ~input=Computer.Input.list([x, y]),
          ~output=Computer.Output.list(outputs),
        )
        ->Computer.runToHalt;
        fromList(outputs^);
      },
    )
  )
  |> sum((module Int64))
  |> Int64.to_string
  |> print
);

let inBeam = (x, y) => {
  let outputs = ref([]);
  Computer.make(
    Computer.Program.parse(input),
    ~input=Computer.Input.list([x, y]),
    ~output=Computer.Output.list(outputs),
  )
  ->Computer.runToHalt;
  List.head(outputs^)->Option.getExn->Int64.to_int == 1;
};

let solution = ref(None);
let x = ref(0);
let y = ref(99);
while (Option.isNone(solution^)) {
  let foundBeam = ref(false);
  while (! foundBeam^) {
    foundBeam := inBeam(x^, y^);
    if (foundBeam^ && inBeam(x^ + 99, y^ - 99)) {
      solution := Some((x^, y^ - 99));
    } else {
      x := x^ + 1;
    };
  };
  y := y^ + 1;
};
let (x, y) = Option.getExn(solution^);
print("Part 2");
print(x * 10_000 + y);