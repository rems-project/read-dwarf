(** More functional combinator.
    This module extends the base OCaml API of
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Fun.html}[Fun]}.*)

include Stdlib.Fun

(** An other application operator.
    [g @@ f @@ 4] replaces [g (f 4)] but [h $ g 4 $ f 5] replaces [h (g 4) (f 5)]

    [@@] has higher precedence so [h $ g @@ f 4 $ t @@ p 5 = h (g (f 4)) (t (p 5))]
    but in practice, I would advise not to mix them.
*)
let ( $ ) = ( @@ )

(** Abstraction of piping above a parameter
    useful to refactor things like [List.map f |> List.map g] into [List.map (f %> g)].

    The trivial definition is [x |> (f %> g) = x |> f |> g]
*)
let ( %> ) f g x = x |> f |> g

(** When you want to run some imperative code on a value before continuing the pipeline *)
let tee f a =
  f a;
  a

(** [curry f a b = f (a, b)] *)
let curry f a b = f (a, b)

(** [uncurry f (a, b) = f a b] *)
let uncurry f (a, b) = f a b

(** Shorthand for [const true] *)
let ctrue _ = true

(** Shorthand for [const false] *)
let cfalse _ = false
