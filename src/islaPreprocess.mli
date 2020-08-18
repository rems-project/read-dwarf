(** This module is about preprocessing isla traces. This includes:

    - Removing exceptional traces
    - Removing useless operation and simplifying
    - Removing all effect of initialization before cycle
    - Splitting branches that are non exceptional the right way

    This is important because isla print everything the sail code does,
    but, as the sail code is not written for performance, it will often
    do a lot of computation and then later decide it wasn't useful and
    discard the result. This quickly make isla traces bloated.

    Once preprocessed, there is only a simple list of traces.

    - Special instructions like smc have zero traces and must be provided with special semantics
    - Normal instructions have one traces (all exceptional cases are classified as UB)
    - Branching instruction have more than one traces.

    For example at time of writing, a basic load like [ldr x0, [x1]]
    has about 1300 variable before preprocessing and 27 after.

    TODO: Remove useless register reads (Need a model where reading register has no side effect).
*)

(** Simplify a simple trace by removing all useless variables *)
val simplify_trc : Isla.rtrc -> Isla.rtrc

(** Preprocess a group of traces, by removing useless registers (according to
    the config), removing initialisation code and simplifying with
    {!simplify_trc} *)
val preprocess : IslaServer.config -> IslaServer.trcs -> Isla.rtrc list
