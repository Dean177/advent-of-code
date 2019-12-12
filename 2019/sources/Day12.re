open Standard;
open AdventOfCode;

let add = ((x, y, z), (x1, y1, z1)) => (x + x1, y + y1, z + z1);

let zip = ((x, y, z), (x1, y1, z1), ~f) => (
  f(x, x1),
  f(y, y1),
  f(z, z1),
);

type position = (int, int, int);
type velocity = (int, int, int);
type moon = (position, velocity);

let potentialEnergy = xyz =>
  Tuple3.toList(xyz)->List.map(~f=Int.absolute) |> List.sum((module Int));

let kineticEnergy = potentialEnergy;

let totalEnergy = ((position, velocity)) =>
  potentialEnergy(position) * kineticEnergy(velocity);

let zeroVelocity = (0, 0, 0);

let io = (((-2), 9, (-5)), zeroVelocity);
let europa = ((16, 19, 9), zeroVelocity);
let ganymede = ((0, 3, 6), zeroVelocity);
let callisto = ((11, 0, 11), zeroVelocity);

let moons = [|io, europa, ganymede, callisto|];

let applyGravity = (moons, (position, velocity)): moon => {
  let velocityChange =
    Array.fold(
      moons, ~initial=(0, 0, 0), ~f=(velocityChange, (position2, _)) =>
      zip(position, position2, ~f=(d1, d2) =>
        Int.clamp(d2 - d1, ~lower=-1, ~upper=1)
      )
      ->add(velocityChange)
    );

  (position, add(velocity, velocityChange));
};

let applyVelocity = ((position, velocity)) => (
  add(position, velocity),
  velocity,
);

let step = moons =>
  Array.map(moons, ~f=applyGravity(moons))->Array.map(~f=applyVelocity);

let part_1 = () => {
  let moons = ref(moons);
  for (_ in 1 to 1000) {
    moons := step(moons^);
  };
  print(
    Array.fold(moons^, ~initial=0, ~f=(energy, moon) =>
      energy + totalEnergy(moon)
    ),
  );
};

let state = (moons, axis) => Array.map(moons, ~f=Tuple.mapAll(~f=axis));

let cycleLength = (moons, ~axis) => {
  let initialState = state(moons, axis);
  let moons = ref(step(moons));
  let iterations = ref(1);
  while (compare(state(moons^, axis), initialState) != 0) {
    iterations := iterations^ + 1;
    moons := step(moons^);
  };
  iterations^;
};

let rec gcd = (x, y) => y == Int64.zero ? x : gcd(y, Int64.rem(x, y));

let lcm = (x: Int64.t, y: Int64.t) =>
  Int64.(x == zero || y == zero ? zero : mul(x, y)->div(gcd(x, y)));

let part_2 = {
  let xCycleLength = cycleLength(moons, ~axis=Tuple3.first);
  let yCycleLength = cycleLength(moons, ~axis=Tuple3.second);
  let zCycleLength = cycleLength(moons, ~axis=Tuple3.third);

  print(
    lcm(
      Int64.of_int(xCycleLength),
      lcm(Int64.of_int(yCycleLength), Int64.of_int(zCycleLength)),
    )
    ->Int64.to_string,
  );
};