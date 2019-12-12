open Standard;
open AdventOfCode;
open Test;

let fuel = mass => Float.(round(mass / 3., ~direction=`Down) - 2.);

test(expect =>
  fuel(12.)->expect->toEqual(2.)
);
test(expect =>
  fuel(14.)->expect->toEqual(2.)
);
test(expect =>
  fuel(1969.)->expect->toEqual(654.)
);
test(expect =>
  fuel(100756.)->expect->toEqual(33583.)
);

let modules =
  read("./Day01.txt")
  ->String.split(~on="\n")
  ->Array.fromList
  ->Array.map(~f=Js.Float.fromString);

print(Array.sum((module Float), Array.map(modules, ~f=fuel)));

let rec totalFuel = mass => {
  let requiredFuel = fuel(mass);
  if (requiredFuel <= 0.) {
    0.;
  } else {
    requiredFuel +. totalFuel(requiredFuel);
  };
};

test(expect =>
  totalFuel(12.)->expect->toEqual(2.)
);
test(expect =>
  totalFuel(1969.)->expect->toEqual(966.)
);
test(expect =>
  totalFuel(100756.)->expect->toEqual(50346.)
);

print(Array.sum((module Float), Array.map(modules, ~f=totalFuel)));