(** More functional combinator *)

include Stdlib.Fun

(** An other application operator.
    g @@ f @@ 4 replace g (f 4) but h $ g 4 $ f 5 replaces h (g 4) (f 5)

    @@ has higher precedence so h $ g @@ f 4 $ t @@ p 5 = h (g (f 4)) (t (p 5))
    but in practice, I would advise not to mix them.
*)
let ( $ ) = ( @@ )

(** Abstraction of piping above a parameter
    useful to refactor things List.map f |> List.map g into List.map (f %> g).

    The trivial definition is x |> (f %> g) = x |> f |> g
*)
let ( %> ) f g x = x |> f |> g
