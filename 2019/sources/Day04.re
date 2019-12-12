open Standard;
open AdventOfCode;
open Test;

let isIncreasing = digits =>
  Array.sliding(digits, ~size=2)
  ->Array.all(~f=pair => {
      let (a, b) = Tuple.fromArray(pair)->Option.getExn;
      a <= b;
    });

let hasRepeatedDigit = digits =>
  Array.sliding(digits, ~size=2)
  ->Array.any(~f=pair => {
      let (a, b) = Tuple.fromArray(pair)->Option.getExn;
      a == b;
    });

let isPossiblePassword = number => {
  let digits = digits(number);
  Array.length(digits) == 6
  && isIncreasing(digits)
  && hasRepeatedDigit(digits);
};

test(expect =>
  isPossiblePassword(111111)->expect->toEqual(true)
);

test(expect =>
  isPossiblePassword(223450)->expect->toEqual(false)
);

test(expect =>
  isPossiblePassword(123789)->expect->toEqual(false)
);

let input = Array.range(~from=172930, 683082 + 1);

//input->Array.filter(~f=isPossiblePassword)->Array.length->Console.log;

let hasExactlyTwiceRepeatedDigit = digits =>
  Array.fold(digits, ~initial=[], ~f=(chains, digit) =>
    switch (chains) {
    | [] => [[digit]]
    | [chain, ...chains] =>
      switch (chain) {
      | [previousDigit, ..._] when previousDigit == digit => [
          [digit, ...chain],
          ...chains,
        ]
      | _ => [[digit], chain, ...chains]
      }
    }
  )
  ->List.any(~f=digitChain => List.length(digitChain) == 2);

let isPossiblePassword = number => {
  let digits = digits(number);
  Array.length(digits) == 6
  && isIncreasing(digits)
  && hasExactlyTwiceRepeatedDigit(digits);
};

test(expect =>
  isPossiblePassword(112233)->expect->toEqual(true)
);

test(expect =>
  isPossiblePassword(123444)->expect->toEqual(false)
);

test(expect =>
  isPossiblePassword(111122)->expect->toEqual(true)
);

input->Array.filter(~f=isPossiblePassword)->Array.length->print;