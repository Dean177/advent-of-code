open Standard;

type buffer;
[@bs.send] external toString: buffer => string = "toString";
[@bs.module "fs"] external readFileSync: string => buffer = "readFileSync";

let read = path => readFileSync(path)->toString;

module Async = {
  type t(+'a) = Js.Promise.t('a);

  [@bs.val] [@bs.scope "Promise"] external fail: 'a => t('a) = "reject";

  [@bs.val] [@bs.scope "Promise"] external return: 'a => t('a) = "resolve";

  let make = (f): t('a) =>
    Js.Promise.make((~resolve, ~reject as _) => {
      let resolve = value => resolve(. value);
      f(resolve);
    });

  /**
 * Why is there no [map]?
 * [map] is {b unsafe}.
 * Using it may violate the soundness of the type system.
 * This is due to promises 'auto-folding', if you return a promise from map,
 * the type system thinks you have a Promise.t(Promise.t('a)), but due to auto-folding you will actually have a Promise.t('a)
 * You should always use bind instead.
 * */
  [@bs.send]
  external bind: (t('a), ~f: 'a => t('b)) => t('b) = "then";

  [@bs.send] external catch: (t('a), ~f: 'a => t('b)) => t('b) = "catch";

  let all = Js.Promise.all;

  let both = Js.Promise.all2;

  let delay = (milliseconds: int): t(unit) =>
    Js.Promise.make((~resolve, ~reject as _) => {
      let unit = ();
      Js.Global.setTimeout(() => resolve(. unit), milliseconds)->ignore;
    });
};

module Test = {
  let isTest = false;
  type expectation('a) = 'a;
  let expect = (actual: 'a): expectation('a) => actual;

  let test = testFn =>
    if (isTest) {
      switch (testFn(expect)) {
      | () => ()
      | exception error => Js.Console.error(("Fail", error))
      };
    };

  let toEqual = (actual: expectation('a), expected: 'a) =>
    if (expected != actual) {
      Js.Console.error((
        "Fail",
        "Expected:",
        expected,
        "but received:",
        actual,
      ));
    } else {
      Js.Console.info("Success");
    };
};

let print = value => Js.Console.log(value);

let digits = number => {
  Standard.(
    Int.toString(number)
    ->String.toList
    ->Array.fromList
    ->Obj.magic
    ->Array.map(~f=characterString =>
        Int.fromString(characterString)->Option.getExn
      )
  );
};

let rec gcd = (x, y) => y == 0 ? x : gcd(y, x mod y);

let factorial = t => {
  let rec loop = (total, current) => {
    current <= 1 ? total : loop(total * current, current - 1);
  };
  loop(1, t);
};

let permutations = lst => {
  let lstar = Array.fromList(lst);
  let len = Array.length(lstar);
  let ks = Array.range(~from=1, len + 1);
  let indices = Set.Int.fromArray(Array.range(len));
  let choose = ((v, indices, res): (int, Set.Int.t, List.t(int)), k: int) => {
    let ix = Set.toArray(indices)[v mod k];
    (v / k, Set.remove(indices, ix), [lstar[ix], ...res]);
  };

  i => {
    let initial: (int, Set.Int.t, List.t(int)) = (i, indices, []);
    let (v, _, res) = Belt.Array.reduceReverse(ks, initial, choose);

    if (v > 0) {
      None;
    } else {
      Some(res);
    };
  };
};

let distinct = array => Set.Int.fromArray(array)->Set.toArray;