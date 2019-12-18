open Standard;

type buffer;
[@bs.send] external toString: buffer => string = "toString";
[@bs.module "fs"] external readFileSync: string => buffer = "readFileSync";

let read = path => readFileSync(path)->toString;

module Array = {
  include Array;
  let clone = t => Array.map(t, ~f=Fun.identity);
};

module Option = {
  include Option;
  let or_ = (a, b) => Option.isSome(a) ? a : b;
}

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
  let runTests = false;
  type expectation('a) = 'a;
  let expect = (actual: 'a): expectation('a) => actual;

  let test = testFn =>
    if (runTests) {
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
  Int.toString(number)
  ->String.split(~on="")
  ->Array.map(~f=characterString =>
      Int.fromString(characterString)->Option.getExn
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

module Matrix = {
  type t('a) = array(array('a));

  let get = (t, (x, y)) => t[y][x];

  let set = (t, x, y, value) => t[y][x] = value;

  let clone = (t: t('a)): t('a) => Array.map(t, ~f=Array.clone);

  let foldI = (t, ~initial, ~f) => {
    let res = ref(initial);
    for (y in 0 to Array.length(t) - 1) {
      for (x in 0 to Array.length(t[y]) - 1) {
        res := f(res^, get(t, (x, y)), (x, y));
      };
    };
    res^;
  };

  let rec findIndex =
          (~x=0, ~y=0, t: t('a), ~f: ((int, int), 'a) => bool)
          : option(((int, int), 'a)) =>
    if (y >= Array.length(t)) {
      None;
    } else if (x >= Array.length(t[y])) {
      findIndex(~x=0, ~y=y + 1, t, ~f);
    } else if (f((x, y), t[y][x])) {
      Some(((x, y), t[y][x]));
    } else {
      findIndex(~x=x + 1, ~y, t, ~f);
    };

  let forEachI = (t, ~f) =>
    foldI(t, ~initial=(), ~f=((), position, element) =>
      f(position, element)
    );
};

module Compass = {
  type direction =
    | N
    | E
    | S
    | W;

  let directions = [|N, E, S, W|];

  let step = ((x, y)) =>
    fun
    | N => (x, y - 1)
    | E => (x + 1, y)
    | S => (x, y + 1)
    | W => (x - 1, y);
};

module Ref = {
  let increment = i => i := i^ + 1;
};

module MutableDeque = {
  type t('a) = ref((list('a), list('a)));

  let empty = () => ref(([], []));

  let fromList = list => ref((list, []));

  let isEmpty = t =>
    switch (t^) {
    | ([], []) => true
    | _ => false
    };

  let rec popLeft = t => {
    switch (t^) {
    | ([], []) => None
    | ([element, ...front], back) =>
      t := (front, back);
      Some(element);
    | ([], back) =>
      t := (List.reverse(back), []);
      popLeft(t);
    };
  };

  let pushRight = (t, element) => {
    let (front, back) = t^;
    t := (front, [element, ...back]);
  };

  let pushLeft = (t, element) => {
    let (front, back) = t^;
    t := ([element, ...front], back);
  };
};