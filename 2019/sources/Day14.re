open Standard;
open AdventOfCode;
open Test;
open Int64;

let input = read("./Day14.txt");

type ingredient = {
  amount: int,
  name: string,
};

type reaction = {
  quantity: int,
  inputs: list(ingredient),
};

let parseChemical = string => {
  let (quantity, name) =
    String.split(string, ~on=" ")->Tuple.fromList->Option.getExn;
  {amount: Int.fromString(quantity)->Option.getExn, name};
};

let parse = input =>
  String.split(input, ~on="\n")
  ->List.map(~f=line => {
      let (inputs, output) =
        String.split(line, ~on=" => ")
        ->Tuple.fromList
        ->Option.getOrFailWith(~exn=Invalid_argument("Bad line: " ++ line));

      let {amount, name} = parseChemical(output);
      (
        name,
        {
          quantity: amount,
          inputs: String.split(inputs, ~on=", ")->List.map(~f=parseChemical),
        },
      );
    })
  ->Map.String.fromList;

let oreCostForFuel =
    (~inventory=ref(Map.String.empty), chemicalToInputs, quantity) => {
  open Int64;
  let rec requiredOre = (chemical, quantity) => {
    let reaction =
      Map.get(chemicalToInputs, chemical)
      ->Option.getOrFailWith(~exn=Invalid_argument(chemical));

    let multiplier = {
      let divisor = div(quantity, of_int(reaction.quantity));
      rem(quantity, of_int(reaction.quantity)) > zero
        ? add(divisor, one) : divisor;
    };

    let ore =
      List.fold(
        reaction.inputs,
        ~initial=zero,
        ~f=(ore, input) => {
          let required = mul(multiplier, of_int(input.amount));
          if (input.name == "ORE") {
            add(ore, required);
          } else {
            let leftover =
              Map.get(inventory^, input.name)->Option.get(~default=zero);

            inventory :=
              Map.add(
                inventory^,
                ~key=input.name,
                ~value=sub(leftover, required),
              );

            if (leftover < required) {
              add(ore, requiredOre(input.name, sub(required, leftover)));
            } else {
              ore;
            };
          };
        },
      );

    inventory :=
      Map.update(inventory^, ~key=chemical, ~f=leftover =>
        Option.get(leftover, ~default=zero)
        ->add(mul(multiplier, of_int(reaction.quantity)))
        ->Option.some
      );

    ore;
  };

  requiredOre("FUEL", quantity);
};

test(expect => {
  let reactions = "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL";
  expect(oreCostForFuel(parse(reactions), one)->to_int)->toEqual(31);
});

test(expect => {
  let reactions = "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL";
  expect(oreCostForFuel(parse(reactions), one)->to_int)->toEqual(165);
});

let reactions = parse(input);

let oreCostForOneFuelWithNoStors = oreCostForFuel(reactions, one);
print(("Part 1", oreCostForOneFuelWithNoStors));

let ore = of_string("1_000_000_000_000");
let remainingOre = ref(ore);
let fuelProduced = ref(zero);
let targetBatchSize = ref(div(ore, oreCostForOneFuelWithNoStors));
let inventory = ref(Map.String.empty);

while (targetBatchSize^ > zero) {
  print((to_string(remainingOre^), to_string(targetBatchSize^)));
  let newInventory = ref(inventory^);
  let oreConsumbed =
    oreCostForFuel(~inventory=newInventory, reactions, targetBatchSize^);

  if (oreConsumbed > remainingOre^) {
    targetBatchSize := div(targetBatchSize^, of_int(2));
  } else {
    fuelProduced := add(fuelProduced^, targetBatchSize^);
    remainingOre := sub(remainingOre^, oreConsumbed);
    inventory := newInventory^;
  };
};
print(("Part 2", to_string(fuelProduced^)));

let _ = ();