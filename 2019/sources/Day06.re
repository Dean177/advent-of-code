open Standard;
open AdventOfCode;

let orbits =
  read("./Day06.txt")
  ->String.split(~on="\n")
  ->Array.map(~f=line =>
      String.split(line, ~on=")")->Tuple.fromArray->Option.getExn
    );

let (planets, planetToParent) =
  Array.fold(
    orbits,
    ~initial=(Set.String.empty, Map.String.empty),
    ~f=((planets, planetToParent), (parent, planet)) =>
    (
      Set.add(planets, planet)->Set.add(parent),
      Map.add(planetToParent, ~key=planet, ~value=parent),
    )
  );

let planetToOrbitPathLength =
  ref(Map.String.singleton(~key="COM", ~value=0));

let rec orbitPathLength = planet => {
  switch (Map.get(planetToOrbitPathLength^, planet)) {
  | Some(pathLength) => pathLength
  | None =>
    let parent = Map.get(planetToParent, planet)->Option.getExn;
    let pathLength = 1 + orbitPathLength(parent);
    planetToOrbitPathLength :=
      Map.add(planetToOrbitPathLength^, ~key=planet, ~value=pathLength);
    pathLength;
  };
};

// Part 1
Array.map(Set.toArray(planets), ~f=orbitPathLength)
|> Array.sum((module Int))
|> print;

let rec pathFrom = (~path=[], source, destination) =>
  if (source == destination) {
    path;
  } else {
    let parent = Map.get(planetToParent, source)->Option.getExn;
    pathFrom(~path=[parent, ...path], parent, destination);
  };

let comParents = Set.String.fromList(pathFrom("SAN", "COM"));
let commonAncestor =
  pathFrom("YOU", "COM")
  ->List.reverse
  ->List.find(~f=Set.includes(comParents))
  ->Option.getExn;

let youParent = Map.get(planetToParent, "YOU")->Option.getExn;
let sanParent = Map.get(planetToParent, "SAN")->Option.getExn;
let youParentToAncestor = pathFrom(youParent, commonAncestor);
let sanParentToAncestor = pathFrom(sanParent, commonAncestor);

// Part 2
print(youParentToAncestor->List.length + sanParentToAncestor->List.length);
let _ = ();