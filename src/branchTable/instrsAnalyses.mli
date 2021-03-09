(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

type 'a branch = Simple of int * 'a | Reg of State.Reg.t list

(** An instruction looks like a branch if it is not a return and writes to the PC *)
val looks_like_a_branch : Trace.Instr.t -> int -> bool branch option

type instructions = (int, Run.Runner.slot) Hashtbl.t

(** Triples of address of the branch, whether it is conditional (i.e. does it
    fall-through to the next instruction) and the target address *)
type simple_branches = private (int * bool * int) list

(** Pairs of the address of the branch and the register whose computed values is branched on. *)
type reg_branches = private (int * State.Reg.t list) list

(** Find and split simple branches from register branches.
    If it is simple, we record whether a branch is conditional (and so can
    fall-through to the next instruction) or unconditional and its target
    address to help construct the (simple) comes-before relation later. *)
val find_branches : instructions -> simple_branches * reg_branches

(** Compute (the one b to many as) comes-before relation.
    Address a comes-before address b iff a falls-through and/or branches to b.
    NOTE: The graph of this relation can be be cyclic.
    NOTE: It ignores the fact that BLR instructions effectively fall-through. *)
val compute_comes_before : instructions -> simple_branches -> (int, int * int list) Hashtbl.t

(** Track register spilling and reloading. Returns a map [(int, int) Hashtbl.t]
    where the key is the address of instructions that load from a stack-offset
    that was spilled-to earlier and the value is the address of that earlier spill.
    NOTE: This is an over-approximation: it will track all register spills but may
    also include standard stack/local variable manipulation.
    NOTE: Does NOT do/use any sort of pointer-aliasing analysis, just syntactic
    pattern-matching. *)
val track_spills : instructions -> simple_branches -> (int, int) Hashtbl.t
