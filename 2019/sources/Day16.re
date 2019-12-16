open Standard;
open AdventOfCode;
open Test;

let input = read("./Day16.txt");

let parse = input =>
  String.split(input, ~on="")
  ->List.map(~f=digit =>
      Int.fromString(digit)
      ->Option.getOrFailWith(~exn=Invalid_argument(digit))
    );

let basePattern = [|0, 1, 0, (-1)|];

let pattern = position =>
  Stream.from(i => Some(basePattern[(i + 1) / position mod 4]));

test(expect =>
  expect(pattern(2) |> Stream.npeek(10) |> Array.fromList)
  ->toEqual([|0, 1, 1, 0, 0, (-1), (-1), 0, 0, 1|])
);

let phase = signal => {
  let length = List.length(signal);
  List.mapI(
    signal,
    ~f=(index, _) => {
      let pattern = pattern(index + 1) |> Stream.npeek(length);
      List.zipWith(signal, pattern, ~f=Int.multiply)
      |> List.sum((module Int))
      |> Int.modulo(~by=10)
      |> Int.absolute;
    },
  );
};

test(expect =>
  expect(phase(parse("12345678"))->Array.fromList)
  ->toEqual(parse("48226158")->Array.fromList)
);

let rec fft = (signal, phases) =>
  if (phases == 0) {
    signal;
  } else {
    fft(phase(signal), phases - 1);
  };

test(expect =>
  expect(fft(parse("12345678"), 4)->Array.fromList)
  ->toEqual(parse("01029498")->Array.fromList)
);

test(expect =>
  expect(
    fft(parse("80871224585914546619083218645595"), 100)
    ->List.take(~count=8)
    ->Array.fromList,
  )
  ->toEqual(parse("24176176")->Array.fromList)
);

test(expect =>
  expect(
    fft(parse("19617804207202209144916044189917"), 100)
    ->List.take(~count=8)
    ->Array.fromList,
  )
  ->toEqual(parse("73745418")->Array.fromList)
);

let digits = parse(input);

let part_1 = () => {
  print(
    fft(digits, 100)
    ->List.take(~count=8)
    ->List.map(~f=Int.toString)
    ->String.join(~sep=""),
  );
};

let part_2 = {
  let offset =
    List.take(digits, ~count=7)
    ->List.map(~f=Int.toString)
    ->String.join(~sep="")
    ->Int.fromString
    ->Option.getExn;

  let length = String.length(input) * 10_000;

  let input =
    Array.repeat(digits->Array.fromList, ~length=10_000)->Array.concatenate;

  for (_ in 1 to 100) {
    let partialSum =
      ref(Array.sum((module Int), Array.slice(input, ~from=offset)));

    for (i in offset to length - 1) {
      let sum = partialSum^;
      partialSum := partialSum^ - input[i];
      input[i] = Int.absolute(sum) mod 10;
    };
  };

  print(Array.slice(input, ~from=offset, ~to_=offset + 8));
};