open Standard;
type buffer;
[@bs.send] external toString: buffer => string = "toString";
[@bs.module "fs"] external readFileSync: string => buffer = "readFileSync";

let read = path => readFileSync(path)->toString;

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

module Range = {
  type t = {
    start: int,
    finish: int,
  };

  let forEach = (t, ~f) => {
    for (i in t.start to t.finish) {
      f(i);
    };
  };
};

module Seq = {
  open Standard;

  module Step = {
    /* 'a is an item in the sequence, 's is the state that will produce the remainder of
       the sequence */
    type t('a, 's) =
      | Done
      | Skip('s)
      | Yield('a, 's);
  };
  open Step;

  type t(+_) =
    | Sequence('s, 's => Step.t('a, 's)): t('a);

  let unfold_step = (~init, ~f) => Sequence(init, f);

  let unfold = (~init, ~f) =>
    unfold_step(~init, ~f=s =>
      switch (f(s)) {
      | None => Step.Done
      | Some((a, s)) => Step.Yield(a, s)
      }
    );

  let unfold_with = (s, ~init, ~f) =>
    switch (s) {
    | Sequence(s, next) =>
      Sequence(
        (init, s),
        ((seed, s)) =>
          switch (next(s)) {
          | Done => Done
          | Skip(s) => Skip((seed, s))
          | Yield(a, s) =>
            switch (f(seed, a)) {
            | Done => Done
            | Skip(seed) => Skip((seed, s))
            | Yield(a, seed) => Yield(a, (seed, s))
            }
          },
      )
    };

  let unfold_with_and_finish =
      (s, ~init, ~running_step, ~inner_finished, ~finishing_step) =>
    switch (s) {
    | Sequence(s, next) =>
      Sequence(
        `Inner_running((init, s)),
        state =>
          switch (state) {
          | `Inner_running(state, inner_state) =>
            switch (next(inner_state)) {
            | Done => Skip(`Inner_finished(inner_finished(state)))
            | Skip(inner_state) =>
              Skip(`Inner_running((state, inner_state)))
            | Yield(x, inner_state) =>
              switch (running_step(state, x)) {
              | Done => Done
              | Skip(state) => Skip(`Inner_running((state, inner_state)))
              | Yield(y, state) =>
                Yield(y, `Inner_running((state, inner_state)))
              }
            }
          | `Inner_finished(state) =>
            switch (finishing_step(state)) {
            | Done => Done
            | Skip(state) => Skip(`Inner_finished(state))
            | Yield(y, state) => Yield(y, `Inner_finished(state))
            }
          },
      )
    };

  let of_list = l =>
    unfold_step(
      ~init=l,
      ~f=
        fun
        | [] => Done
        | [x, ...l] => Yield(x, l),
    );

  let of_array = array =>
    unfold_step(~init=(array, 0), ~f=((array, position)) =>
      if (position >= Array.length(array)) {
        Done;
      } else {
        Yield(array[position], (array, position + 1));
      }
    );

  let fold = (t, ~init, ~f) => {
    let rec loop = (seed, v, next, f) =>
      switch (next(seed)) {
      | Done => v
      | Skip(s) => loop(s, v, next, f)
      | Yield(a, s) => loop(s, f(v, a), next, f)
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, init, next, f)
    };
  };

  let to_list_rev = t => fold(t, ~init=[], ~f=(l, x) => [x, ...l]);

  let to_list = (Sequence(s, next)) => {
    let safe_to_list = t => List.reverse(to_list_rev(t));
    let rec to_list = (s, next, i) =>
      if (i == 0) {
        safe_to_list(Sequence(s, next));
      } else {
        switch (next(s)) {
        | Done => []
        | Skip(s) => to_list(s, next, i)
        | Yield(a, s) => [a, ...to_list(s, next, i - 1)]
        };
      };

    to_list(s, next, 500);
  };

  let range =
      (~stride=1, ~start=`inclusive, ~stop=`exclusive, start_v, stop_v) => {
    let step =
      switch (stop) {
      | `inclusive when stride >= 0 => (
          i =>
            if (i > stop_v) {
              Done;
            } else {
              Yield(i, i + stride);
            }
        )
      | `inclusive => (
          i =>
            if (i < stop_v) {
              Done;
            } else {
              Yield(i, i + stride);
            }
        )
      | `exclusive when stride >= 0 => (
          i =>
            if (i >= stop_v) {
              Done;
            } else {
              Yield(i, i + stride);
            }
        )
      | `exclusive => (
          i =>
            if (i <= stop_v) {
              Done;
            } else {
              Yield(i, i + stride);
            }
        )
      };

    let init =
      switch (start) {
      | `inclusive => start_v
      | `exclusive => start_v + stride
      };

    unfold_step(~init, ~f=step);
  };

  let of_lazy = t_lazy =>
    unfold_step(
      ~init=t_lazy,
      ~f=t_lazy => {
        let Sequence(s, next) = Lazy.force(t_lazy);
        switch (next(s)) {
        | Done => Done
        | Skip(s) =>
          Skip(
            {
              let v = Sequence(s, next);
              lazy(v);
            },
          )
        | Yield(x, s) =>
          Yield(
            x,
            {
              let v = Sequence(s, next);
              lazy(v);
            },
          )
        };
      },
    );

  let map = (t, ~f) =>
    switch (t) {
    | Sequence(seed, next) =>
      Sequence(
        seed,
        seed =>
          switch (next(seed)) {
          | Done => Done
          | Skip(s) => Skip(s)
          | Yield(a, s) => Yield(f(a), s)
          },
      )
    };

  let mapi = (t, ~f) =>
    switch (t) {
    | Sequence(s, next) =>
      Sequence(
        (0, s),
        ((i, s)) =>
          switch (next(s)) {
          | Done => Done
          | Skip(s) => Skip((i, s))
          | Yield(a, s) => Yield(f(i, a), (i + 1, s))
          },
      )
    };

  let folding_map = (t, ~init, ~f) =>
    unfold_with(
      t,
      ~init,
      ~f=(acc, x) => {
        let (acc, x) = f(acc, x);
        Yield(x, acc);
      },
    );

  let folding_mapi = (t, ~init, ~f) =>
    unfold_with(
      t,
      ~init=(0, init),
      ~f=((i, acc), x) => {
        let (acc, x) = f(i, acc, x);
        Yield(x, (i + 1, acc));
      },
    );

  let filter = (t, ~f) =>
    switch (t) {
    | Sequence(seed, next) =>
      Sequence(
        seed,
        seed =>
          switch (next(seed)) {
          | Done => Done
          | Skip(s) => Skip(s)
          | Yield(a, s) when f(a) => Yield(a, s)
          | Yield(_, s) => Skip(s)
          },
      )
    };

  let filteri = (t, ~f) =>
    map(
      ~f=snd,
      filter(mapi(t, ~f=(i, s) => (i, s)), ~f=((i, s)) => f(i, s)),
    );

  let length = t => {
    let rec loop = (i, s, next) =>
      switch (next(s)) {
      | Done => i
      | Skip(s) => loop(i, s, next)
      | Yield(_, s) => loop(i + 1, s, next)
      };

    switch (t) {
    | Sequence(seed, next) => loop(0, seed, next)
    };
  };

  let to_list_rev_with_length = t =>
    fold(t, ~init=([], 0), ~f=((l, i), x) => ([x, ...l], i + 1));

  let to_array = t => {
    let (l, len) = to_list_rev_with_length(t);
    switch (l) {
    | [] => [||]
    | [x, ...l] =>
      let a = Array.repeat(~length=len, x);
      let rec loop = (i, l) =>
        switch (l) {
        | [] => assert(i == (-1))
        | [x, ...l] =>
          a[i] = x;
          loop(i - 1, l);
        };

      loop(len - 2, l);
      a;
    };
  };

  let find = (t, ~f) => {
    let rec loop = (s, next, f) =>
      switch (next(s)) {
      | Done => None
      | Yield(a, _) when f(a) => Some(a)
      | Yield(_, s)
      | Skip(s) => loop(s, next, f)
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, next, f)
    };
  };

  let find_map = (t, ~f) => {
    let rec loop = (s, next, f) =>
      switch (next(s)) {
      | Done => None
      | Yield(a, s) =>
        switch (f(a)) {
        | None => loop(s, next, f)
        | some_b => some_b
        }
      | Skip(s) => loop(s, next, f)
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, next, f)
    };
  };

  let find_mapi = (t, ~f) => {
    let rec loop = (s, next, f, i) =>
      switch (next(s)) {
      | Done => None
      | Yield(a, s) =>
        switch (f(i, a)) {
        | None => loop(s, next, f, i + 1)
        | some_b => some_b
        }
      | Skip(s) => loop(s, next, f, i)
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, next, f, 0)
    };
  };

  let for_all = (t, ~f) => {
    let rec loop = (s, next, f) =>
      switch (next(s)) {
      | Done => true
      | Yield(a, _) when !f(a) => false
      | Yield(_, s)
      | Skip(s) => loop(s, next, f)
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, next, f)
    };
  };

  let for_alli = (t, ~f) => {
    let rec loop = (s, next, f, i) =>
      switch (next(s)) {
      | Done => true
      | Yield(a, _) when !f(i, a) => false
      | Yield(_, s) => loop(s, next, f, i + 1)
      | Skip(s) => loop(s, next, f, i)
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, next, f, 0)
    };
  };

  let exists = (t, ~f) => {
    let rec loop = (s, next, f) =>
      switch (next(s)) {
      | Done => false
      | Yield(a, _) when f(a) => true
      | Yield(_, s)
      | Skip(s) => loop(s, next, f)
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, next, f)
    };
  };

  let existsi = (t, ~f) => {
    let rec loop = (s, next, f, i) =>
      switch (next(s)) {
      | Done => false
      | Yield(a, _) when f(i, a) => true
      | Yield(_, s) => loop(s, next, f, i + 1)
      | Skip(s) => loop(s, next, f, i)
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, next, f, 0)
    };
  };

  let iter = (t, ~f) => {
    let rec loop = (seed, next, f) =>
      switch (next(seed)) {
      | Done => ()
      | Skip(s) => loop(s, next, f)
      | Yield(a, s) =>
        f(a);
        loop(s, next, f);
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, next, f)
    };
  };

  let is_empty = t => {
    let rec loop = (s, next) =>
      switch (next(s)) {
      | Done => true
      | Skip(s) => loop(s, next)
      | Yield(_) => false
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, next)
    };
  };

  let mem = (t, a, ~equal) => {
    let rec loop = (s, next, a) =>
      switch (next(s)) {
      | Done => false
      | Yield(b, _) when equal(a, b) => true
      | Yield(_, s)
      | Skip(s) => loop(s, next, a)
      };

    switch (t) {
    | Sequence(seed, next) => loop(seed, next, a)
    };
  };

  let empty = Sequence((), () => Done);

  let bind = (t, ~f) =>
    unfold_step(
      ~f=
        fun
        | (Sequence(seed, next), rest) =>
          switch (next(seed)) {
          | Done =>
            switch (rest) {
            | Sequence(seed, next) =>
              switch (next(seed)) {
              | Done => Done
              | Skip(s) => Skip((empty, Sequence(s, next)))
              | Yield(a, s) => Skip((f(a), Sequence(s, next)))
              }
            }
          | Skip(s) => Skip((Sequence(s, next), rest))
          | Yield(a, s) => Yield(a, (Sequence(s, next), rest))
          },
      ~init=(empty, t),
    );

  let return = x =>
    unfold_step(
      ~init=Some(x),
      ~f=
        fun
        | None => Done
        | Some(x) => Yield(x, None),
    );

  let nth = (s, n) =>
    if (n < 0) {
      None;
    } else {
      let rec loop = (i, s, next) =>
        switch (next(s)) {
        | Done => None
        | Skip(s) => loop(i, s, next)
        | Yield(a, s) =>
          if (i == 0) {
            Some(a);
          } else {
            loop(i - 1, s, next);
          }
        };

      switch (s) {
      | Sequence(s, next) => loop(n, s, next)
      };
    };

  let nth_exn = (s, n) =>
    if (n < 0) {
      raise(Invalid_argument("Sequence.nth"));
    } else {
      switch (nth(s, n)) {
      | None => failwith("Sequence.nth")
      | Some(x) => x
      };
    };

  let hd = s => {
    let rec loop = (s, next) =>
      switch (next(s)) {
      | Done => None
      | Skip(s) => loop(s, next)
      | Yield(a, _) => Some(a)
      };

    switch (s) {
    | Sequence(s, next) => loop(s, next)
    };
  };

  let hd_exn = s =>
    switch (hd(s)) {
    | None => failwith("hd_exn")
    | Some(a) => a
    };

  let tl = s => {
    let rec loop = (s, next) =>
      switch (next(s)) {
      | Done => None
      | Skip(s) => loop(s, next)
      | Yield(_, a) => Some(a)
      };

    switch (s) {
    | Sequence(s, next) =>
      switch (loop(s, next)) {
      | None => None
      | Some(s) => Some(Sequence(s, next))
      }
    };
  };

  let tl_eagerly_exn = s =>
    switch (tl(s)) {
    | None => failwith("Sequence.tl_exn")
    | Some(s) => s
    };

  let lift_identity = (next, s) =>
    switch (next(s)) {
    | Done => Done
    | Skip(s) => Skip(`Identity(s))
    | Yield(a, s) => Yield(a, `Identity(s))
    };

  let next = s => {
    let rec loop = (s, next) =>
      switch (next(s)) {
      | Done => None
      | Skip(s) => loop(s, next)
      | Yield(a, s) => Some((a, Sequence(s, next)))
      };

    switch (s) {
    | Sequence(s, next) => loop(s, next)
    };
  };

  let filter_opt = s =>
    switch (s) {
    | Sequence(s, next) =>
      Sequence(
        s,
        s =>
          switch (next(s)) {
          | Done => Done
          | Skip(s) => Skip(s)
          | Yield(None, s) => Skip(s)
          | Yield(Some(a), s) => Yield(a, s)
          },
      )
    };

  let filter_map = (s, ~f) => filter_opt(map(s, ~f));
  let filter_mapi = (s, ~f) =>
    filter_map(mapi(s, ~f=(i, s) => (i, s)), ~f=((i, s)) => f(i, s));

  let split_n = (s, n) => {
    let rec loop = (s, i, accum, next) =>
      if (i <= 0) {
        (List.reverse(accum), Sequence(s, next));
      } else {
        switch (next(s)) {
        | Done => (List.reverse(accum), empty)
        | Skip(s) => loop(s, i, accum, next)
        | Yield(a, s) => loop(s, i - 1, [a, ...accum], next)
        };
      };

    switch (s) {
    | Sequence(s, next) => loop(s, n, [], next)
    };
  };

  let chunks_exn = (t, n) =>
    if (n <= 0) {
      raise(Invalid_argument("Sequence.chunks_exn"));
    } else {
      unfold_step(~init=t, ~f=t =>
        switch (split_n(t, n)) {
        | ([], _empty) => Done
        | ([_, ..._] as xs, t) => Yield(xs, t)
        }
      );
    };

  let findi = (s, ~f) =>
    find(mapi(s, ~f=(i, s) => (i, s)), ~f=((i, s)) => f(i, s));

  let find_exn = (s, ~f) =>
    switch (find(s, ~f)) {
    | None => failwith("Sequence.find_exn")
    | Some(x) => x
    };

  let append = (s1, s2) =>
    switch (s1, s2) {
    | (Sequence(s1, next1), Sequence(s2, next2)) =>
      Sequence(
        `First_list(s1),
        (
          fun
          | `First_list(s1) =>
            switch (next1(s1)) {
            | Done => Skip(`Second_list(s2))
            | Skip(s1) => Skip(`First_list(s1))
            | Yield(a, s1) => Yield(a, `First_list(s1))
            }
          | `Second_list(s2) =>
            switch (next2(s2)) {
            | Done => Done
            | Skip(s2) => Skip(`Second_list(s2))
            | Yield(a, s2) => Yield(a, `Second_list(s2))
            }
        ),
      )
    };

  let concat_map = (s, ~f) => bind(s, ~f);
  let concat = s => concat_map(s, ~f=Fun.identity);
  let concat_mapi = (s, ~f) =>
    concat_map(mapi(s, ~f=(i, s) => (i, s)), ~f=((i, s)) => f(i, s));

  let zip = (Sequence(s1, next1), Sequence(s2, next2)) => {
    let next =
      fun
      | (Yield(a, s1), Yield(b, s2)) =>
        Yield((a, b), (Skip(s1), Skip(s2)))
      | (Done, _)
      | (_, Done) => Done
      | (Skip(s1), s2) => Skip((next1(s1), s2))
      | (s1, Skip(s2)) => Skip((s1, next2(s2)));

    Sequence((Skip(s1), Skip(s2)), next);
  };

  let zip_full = (Sequence(s1, next1), Sequence(s2, next2)) => {
    let next =
      fun
      | (Yield(a, s1), Yield(b, s2)) =>
        Yield(`Both((a, b)), (Skip(s1), Skip(s2)))
      | (Done, Done) => Done
      | (Skip(s1), s2) => Skip((next1(s1), s2))
      | (s1, Skip(s2)) => Skip((s1, next2(s2)))
      | (Done, Yield(b, s2)) => Yield(`Right(b), (Done, next2(s2)))
      | (Yield(a, s1), Done) => Yield(`Left(a), (next1(s1), Done));

    Sequence((Skip(s1), Skip(s2)), next);
  };

  let bounded_length = (Sequence(seed, next), ~at_most) => {
    let rec loop = (i, seed, next) =>
      if (i > at_most) {
        `Greater;
      } else {
        switch (next(seed)) {
        | Done => `Is(i)
        | Skip(seed) => loop(i, seed, next)
        | Yield(_, seed) => loop(i + 1, seed, next)
        };
      };

    loop(0, seed, next);
  };

  let length_is_bounded_by = (~min=(-1), ~max=?, t) => {
    let length_is_at_least = (Sequence(s, next)) => {
      let rec loop = (s, acc) =>
        if (acc >= min) {
          true;
        } else {
          switch (next(s)) {
          | Done => false
          | Skip(s) => loop(s, acc)
          | Yield(_, s) => loop(s, acc + 1)
          };
        };

      loop(s, 0);
    };

    switch (max) {
    | None => length_is_at_least(t)
    | Some(max) =>
      switch (bounded_length(t, ~at_most=max)) {
      | `Is(len) when len >= min => true
      | _ => false
      }
    };
  };

  let iteri = (s, ~f) =>
    iter(mapi(s, ~f=(i, s) => (i, s)), ~f=((i, s)) => f(i, s));

  let foldi = (s, ~init, ~f) =>
    fold(~init, mapi(s, ~f=(i, s) => (i, s)), ~f=(acc, (i, s)) =>
      f(i, acc, s)
    );

  let reduce = (s, ~f) =>
    switch (next(s)) {
    | None => None
    | Some((a, s)) => Some(fold(s, ~init=a, ~f))
    };

  let reduce_exn = (s, ~f) =>
    switch (reduce(s, ~f)) {
    | None => failwith("Sequence.reduce_exn")
    | Some(res) => res
    };

  let group = (Sequence(s, next), ~break) =>
    unfold_step(
      ~init=Some(([], s)),
      ~f=
        fun
        | None => Done
        | Some((acc, s)) =>
          switch (acc, next(s)) {
          | (_, Skip(s)) => Skip(Some((acc, s)))
          | ([], Done) => Done
          | (acc, Done) => Yield(List.reverse(acc), None)
          | ([], Yield(cur, s)) => Skip(Some(([cur], s)))
          | ([prev, ..._] as acc, Yield(cur, s)) =>
            if (break(prev, cur)) {
              Yield(List.reverse(acc), Some(([cur], s)));
            } else {
              Skip(Some(([cur, ...acc], s)));
            }
          },
    );

  let find_consecutive_duplicate = (Sequence(s, next), ~equal) => {
    let rec loop = (last_elt, s) =>
      switch (next(s)) {
      | Done => None
      | Skip(s) => loop(last_elt, s)
      | Yield(a, s) =>
        switch (last_elt) {
        | Some(b) when equal(a, b) => Some((b, a))
        | None
        | Some(_) => loop(Some(a), s)
        }
      };

    loop(None, s);
  };

  let remove_consecutive_duplicates = (s, ~equal) =>
    unfold_with(s, ~init=None, ~f=(prev, a) =>
      switch (prev) {
      | Some(b) when equal(a, b) => Skip(Some(a))
      | None
      | Some(_) => Yield(a, Some(a))
      }
    );

  let count = (s, ~f) => length(filter(s, ~f));
  let counti = (t, ~f) => length(filteri(t, ~f));
  //  let sum = (m, t, ~f) => Container.sum(~fold, m, t, ~f);
  //  let min_elt = (t, ~compare) => Container.min_elt(~fold, t, ~compare);
  //  let max_elt = (t, ~compare) => Container.max_elt(~fold, t, ~compare);

  let init = (n, ~f) =>
    unfold_step(~init=0, ~f=i =>
      if (i >= n) {
        Done;
      } else {
        Yield(f(i), i + 1);
      }
    );

  let sub = (s, ~pos, ~len) => {
    if (pos < 0 || len < 0) {
      failwith("Sequence.sub");
    };
    switch (s) {
    | Sequence(s, next) =>
      Sequence(
        (0, s),
        ((i, s)) =>
          if (i - pos >= len) {
            Done;
          } else {
            switch (next(s)) {
            | Done => Done
            | Skip(s) => Skip((i, s))
            | Yield(a, s) when i >= pos => Yield(a, (i + 1, s))
            | Yield(_, s) => Skip((i + 1, s))
            };
          },
      )
    };
  };

  let take = (s, len) => {
    if (len < 0) {
      failwith("Sequence.take");
    };
    switch (s) {
    | Sequence(s, next) =>
      Sequence(
        (0, s),
        ((i, s)) =>
          if (i >= len) {
            Done;
          } else {
            switch (next(s)) {
            | Done => Done
            | Skip(s) => Skip((i, s))
            | Yield(a, s) => Yield(a, (i + 1, s))
            };
          },
      )
    };
  };

  let drop = (s, len) => {
    if (len < 0) {
      failwith("Sequence.drop");
    };
    switch (s) {
    | Sequence(s, next) =>
      Sequence(
        (0, s),
        ((i, s)) =>
          switch (next(s)) {
          | Done => Done
          | Skip(s) => Skip((i, s))
          | Yield(a, s) when i >= len => Yield(a, (i + 1, s))
          | Yield(_, s) => Skip((i + 1, s))
          },
      )
    };
  };

  let take_while = (s, ~f) =>
    switch (s) {
    | Sequence(s, next) =>
      Sequence(
        s,
        s =>
          switch (next(s)) {
          | Done => Done
          | Skip(s) => Skip(s)
          | Yield(a, s) when f(a) => Yield(a, s)
          | Yield(_, _) => Done
          },
      )
    };

  let drop_while = (s, ~f) =>
    switch (s) {
    | Sequence(s, next) =>
      Sequence(
        `Dropping(s),
        (
          fun
          | `Dropping(s) =>
            switch (next(s)) {
            | Done => Done
            | Skip(s) => Skip(`Dropping(s))
            | Yield(a, s) when f(a) => Skip(`Dropping(s))
            | Yield(a, s) => Yield(a, `Identity(s))
            }
          | `Identity(s) => lift_identity(next, s)
        ),
      )
    };

  let shift_right = (s, x) =>
    switch (s) {
    | Sequence(seed, next) =>
      Sequence(
        `Consing((seed, x)),
        (
          fun
          | `Consing(seed, x) => Yield(x, `Identity(seed))
          | `Identity(s) => lift_identity(next, s)
        ),
      )
    };

  let shift_right_with_list = (s, l) => append(of_list(l), s);

  let shift_left = drop;

  let intersperse = (s, ~sep) =>
    switch (s) {
    | Sequence(s, next) =>
      Sequence(
        `Init(s),
        (
          fun
          | `Init(s) =>
            switch (next(s)) {
            | Done => Done
            | Skip(s) => Skip(`Init(s))
            | Yield(a, s) => Yield(a, `Running(s))
            }
          | `Running(s) =>
            switch (next(s)) {
            | Done => Done
            | Skip(s) => Skip(`Running(s))
            | Yield(a, s) => Yield(sep, `Putting((a, s)))
            }
          | `Putting(a, s) => Yield(a, `Running(s))
        ),
      )
    };

  let repeat = x => unfold_step(~init=x, ~f=x => Yield(x, x));

  let cycle_list_exn = xs => {
    if (List.is_empty(xs)) {
      raise(Invalid_argument("Sequence.cycle_list_exn"));
    };
    let s = of_list(xs);
    concat_map(~f=() => s, repeat());
  };

  let cartesian_product = (sa, sb) =>
    concat_map(sa, ~f=a => zip(repeat(a), sb));

  let singleton = x => return(x);

  let fold_result = (s, ~init, ~f) => {
    let rec loop = (s, next, f, acc) =>
      switch (next(s)) {
      | Done => Result.ok(acc)
      | Skip(s) => loop(s, next, f, acc)
      | Yield(a, s) =>
        switch ((f(acc, a): Result.t(_, _))) {
        | Error(_) as e => e
        | Ok(acc) => loop(s, next, f, acc)
        }
      };

    switch (s) {
    | Sequence(s, next) => loop(s, next, f, init)
    };
  };

  let force_eagerly = t => of_list(to_list(t));

  let memoize = (type a, Sequence(s, next)) => {
    module M = {
      type t =
        | T(Lazy.t(Step.t(a, t)));
    };

    let rec memoize = s => M.T(lazy(find_step(s)))
    and find_step = s =>
      switch (next(s)) {
      | Done => Done
      | Skip(s) => find_step(s)
      | Yield(a, s) => Yield(a, memoize(s))
      };

    Sequence(memoize(s), (M.T(l)) => Lazy.force(l));
  };

  let drop_eagerly = (s, len) => {
    let rec loop = (i, ~len, s, next) =>
      if (i >= len) {
        Sequence(s, next);
      } else {
        switch (next(s)) {
        | Done => empty
        | Skip(s) => loop(i, ~len, s, next)
        | Yield(_, s) => loop(i + 1, ~len, s, next)
        };
      };

    switch (s) {
    | Sequence(s, next) => loop(0, ~len, s, next)
    };
  };

  let drop_while_option = (Sequence(s, next), ~f) => {
    let rec loop = s =>
      switch (next(s)) {
      | Done => None
      | Skip(s) => loop(s)
      | Yield(x, s) =>
        if (f(x)) {
          loop(s);
        } else {
          Some((x, Sequence(s, next)));
        }
      };

    loop(s);
  };
};