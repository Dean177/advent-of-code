open Standard;
open AdventOfCode;
open Test;

let input = read("./Day08.txt");

type pixel = Int.t;
type layer = array(pixel);

let getOrError = (t, ~message) =>
  switch (t) {
  | Some(value) => value
  | None => raise(Invalid_argument(message))
  };

let parse = (input, ~width, ~height): Array.t(layer) => {
  let pixels =
    String.split(input, ~on="")
    ->Array.fromList
    ->Array.map(~f=pixel =>
        Int.fromString(pixel)
        ->getOrError(~message="Parsed non digit pixel: " ++ pixel)
      );

  Array.sliding(pixels, ~step=width * height, ~size=width * height);
};

test(expect =>
  expect(parse("123456789012", ~width=3, ~height=2))
  ->toEqual([|[|1, 2, 3, 4, 5, 6|], [|7, 8, 9, 0, 1, 2|]|])
);

let countNumber = (layer: layer, pixelValue: int) =>
  Array.fold(layer, ~initial=0, ~f=(total, pixel) =>
    pixel == pixelValue ? total + 1 : total
  );

test(expect =>
  expect(parse("123456789012", ~width=3, ~height=2)[0]->countNumber(1))
  ->toEqual(1)
);

let width = 25;
let height = 6;

let part1 = () => {
  let layers = parse(input, ~width, ~height);

  let (layerWithLeastZeros, _) =
    Array.fold(
      layers,
      ~initial=None,
      ~f=(lowestSeen: option((layer, int)), layer) => {
        let zeroCount = countNumber(layer, 0);
        switch (lowestSeen) {
        | None => Some((layer, zeroCount))
        | Some((_, minZeroes)) =>
          zeroCount < minZeroes ? Some((layer, zeroCount)) : lowestSeen
        };
      },
    )
    ->Option.getExn;

  let ones = countNumber(layerWithLeastZeros, 1);
  let twos = countNumber(layerWithLeastZeros, 2);
  print(ones * twos);
};

let toString = (layer, ~width) => {
  let layer =
    Array.map(layer, ~f=pixel =>
      switch (pixel) {
      | 0 => {js|⬛️|js}
      | 1 => {js|⬜️|js}
      | _ =>
        raise(
          Invalid_argument(
            "Tried to show pixel value " ++ Int.toString(pixel),
          ),
        )
      }
    );
  Array.sliding(layer, ~step=width, ~size=width)
  ->Array.map(~f=row => Array.toList(row)->String.join(~sep=""))
  ->Array.toList
  ->String.join(~sep="\n");
};

let isTransparent = pixel => pixel == 2;

let flatten = (layers, ~width, ~height) =>
  Array.foldRight(
    layers,
    ~initial=Array.repeat(~length=width * height, 1),
    ~f=(result: layer, layer) => {
      for (i in 0 to Array.length(layer) - 1) {
        if (!isTransparent(layer[i])) {
          result[i] = layer[i];
        };
      };
      result;
    },
  );

test(expect => {
  let width = 2;
  let height = 2;

  expect(
    parse("0222112222120000", ~width, ~height)
    ->flatten(~width, ~height)
    ->toString(~width),
  )
  ->toEqual({js|⬛️⬜️\n⬜️⬛️|js});
});

let part2 = {
  let layers = parse(input, ~width, ~height);
  let result = flatten(layers, ~width, ~height);
  print(result->toString(~width));
};
let _ = ();