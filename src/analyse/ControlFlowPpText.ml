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

(*****************************************************************************)
(**       pp control-flow branches to text                                   *)

(*****************************************************************************)

open Utils

(*open SdtUtils*)
(*open Elf*)
open ControlFlowTypes

(*open ControlFlow*)
(*open CollectedType*)
(*open DwarfLineInfo*)
(*open ComeFrom*)
(*open CollectedType   *)

type arrow = {
  source : index;
  targets : index list;
  first : index;
  (* min of source and all targets *)
  last : index;
  (* max of source and all targets *)
  weight : weight;
}

let render_ascii_control_flow max_branch_distance max_width instructions :
    glyph array array * glyph array array (*inbetweens*) * int (* actual_width *) =
  (* pull the arrows out of instructions *)
  let arrow_from (k : index) i : arrow option =
    (*(addr, i, m, args, c, targets1)*)
    let render_target_kind = function
      | T_plain_successor -> false
      | T_branch -> true
      | T_branch_and_link_call -> false
      | T_branch_and_link_call_noreturn -> false
      | T_branch_and_link_successor -> false
      | T_branch_cond_branch -> true
      | T_branch_cond_successor -> false
      | T_branch_register -> true
      | T_smc_hvc_successor -> false
      | T_out_of_range _ -> false
    in

    (* filter out targets that we're not going to render *)
    let targets2 = List.filter (function (tk, _, _, _) -> render_target_kind tk) i.i_targets in

    match targets2 with
    | [] -> None
    | _ ->
        (* sort targets by target instruction index *)
        let targets3 =
          List.sort_uniq
            (function
              | (_, _, k', _) -> (
                  function (_, _, k'', _) -> compare k' k''
                ))
            targets2
        in
        (* project out just the index *)
        let targets4 = List.map (function (_, _, k', _) -> k') targets3 in

        let first = min k (List.hd targets4) in
        let last = max k (List.last targets4) in

        Some
          { source = k; targets = targets4; first; last; weight = (if first < k then B else L) }
  in

  let array_filter_mapi (f : int -> 'a -> 'b option) (a : 'a array) : 'b list =
    let rec g k acc =
      if k < 0 then acc else g (k - 1) (match f k a.(k) with None -> acc | Some b -> b :: acc)
    in
    g (Array.length a - 1) []
  in

  let arrows0 : arrow list = array_filter_mapi arrow_from instructions in

  (* sort by size, to render short arrows rightmost *)
  let compare_arrow a1 a2 = compare (a1.last - a1.first) (a2.last - a2.first) in
  let arrows = List.stable_sort compare_arrow arrows0 in

  (* paint the arrows into a buffer of glyphs *)
  let buf = Array.make_matrix (Array.length instructions) max_width Gnone in
  let leftmost_column_used = ref max_width in

  let paint_arrow a =
    let rec forall k1 k2 f = if k1 > k2 then true else f k1 && forall (k1 + 1) k2 f in

    let rec largest c1 c2 f =
      if c2 < c1 then None else if f c2 then Some c2 else largest c1 (c2 - 1) f
    in

    let try_at_column dry_run c =
      let try_for_row k =
        let is_target = List.mem k a.targets in
        let is_source = k = a.source in
        let is_self_target = is_target && is_source in

        let free k c' = buf.(k).(c') = Gnone in

        let paint k c' g =
          if buf.(k).(c') = Gnone then (
            if dry_run then () else buf.(k).(c') <- g;
            true
          )
          else false
        in

        let paint_allowing_crossing k c' g =
          match
            match (buf.(k).(c'), g) with
            | (Gnone, g) -> Some g
            | (Glr w1, Gud w2) -> Some (Glrud (w1, w2))
            | (Gud w2, Glr w1) -> Some (Glrud (w1, w2))
            | (_, _) -> None
          with
          | None -> false
          | Some g' ->
              if dry_run then () else buf.(k).(c') <- g';
              true
        in

        let paint_target_arrow k c' ghead w =
          match
            largest c' (max_width - 1) (fun c'' ->
                free k c''
                && forall c' (c'' - 1) (fun c''' ->
                       buf.(k).(c''') = Gnone || buf.(k).(c''') = Gud w))
          with
          | Some c_head ->
              if dry_run then () else buf.(k).(c_head) <- ghead;
              for c'' = c' to c_head - 1 do
                ignore (paint_allowing_crossing k c'' (Glr w))
              done;
              true
          | None -> false
        in

        let paint_source_line k c' w =
          match
            largest c' (max_width - 1) (fun c'' ->
                forall c' c'' (fun c''' ->
                    match buf.(k).(c''') with Gnone -> true | Gud _ -> true | _ -> false))
          with
          | Some c_head ->
              for c'' = c' to c_head do
                ignore (paint_allowing_crossing k c'' (Glr w))
              done;
              true
          | None -> false
        in

        let w = a.weight in

        if is_target || is_source then
          paint k c
            ( match (a.first = k, k = a.last) with
            | (true, false) -> Grd w
            | (false, false) -> Grud w
            | (false, true) -> Gru w
            | (true, true) -> Gnone
            )
          &&
          if is_target then paint_target_arrow k (c + 1) (if is_self_target then GX else Ggt) w
          else paint_source_line k (c + 1) w
        else paint_allowing_crossing k c (Gud w)
      in

      forall a.first a.last try_for_row
    in

    if match max_branch_distance with None -> true | Some d -> a.last - a.first < d then
      match largest 1 (max_width - 2) (try_at_column true) with
      | Some c ->
          ignore (try_at_column false c);
          leftmost_column_used := min c !leftmost_column_used
      | None -> buf.(a.source).(max_width - 1) <- Gquery
    else begin
      (*hackish paint_long_branch, ignoring whatever is underneath*)
      buf.(a.source).(max_width - 1) <- Glt;
      buf.(a.source).(max_width - 2) <- Glt;
      List.iter
        (function
          | k' ->
              let g = if k' = a.source then GX else Ggt in
              buf.(k').(max_width - 1) <- g;
              buf.(k').(max_width - 2) <- g)
        a.targets;
      leftmost_column_used := min (max_width - 2) !leftmost_column_used
    end
  in
  List.iter paint_arrow arrows;

  (* actual width used *)
  let width = max_width - !leftmost_column_used in

  (* construct inter-line array of glphys of vertical arrows for filler *)
  let inbetweens =
    Array.init (Array.length buf) (function k ->
        if k = 0 then Array.make width Gnone
        else
          Array.map2
            (fun g1 g2 ->
              if
                List.mem g1 [Gud L; Grd L; Grud L; Glrud (L, L); Glrud (B, L)]
                && List.mem g2 [Gud L; Gru L; Grud L; Glrud (L, L); Glrud (B, L)]
              then Gud L
              else if
                List.mem g1 [Gud B; Grd B; Grud B; Glrud (L, B); Glrud (B, B)]
                && List.mem g2 [Gud B; Gru B; Grud B; Glrud (L, B); Glrud (B, B)]
              then Gud B
              else Gnone)
            (Array.sub buf.(k - 1) !leftmost_column_used width)
            (Array.sub buf.(k) !leftmost_column_used width))
  in

  ( Array.map
      (function
        | row -> Array.sub row !leftmost_column_used width)
      buf,
    inbetweens,
    width )


(* return index of first position where they differ *)
let common_prefix_end (gas: glyph array list) : int =
  let rec f ga0 gas i =
    match gas with
    | [] -> i
    | ga::gas' ->
       let rec g i' =
         if i' >= i then
           i
         else if i' >=Array.length ga0 then
           i' 
         else if ga.(i')=ga0.(i') then
           g (i'+1)
         else i'
       in
       let i' = g 0 in
       f ga0 gas' i' 
  in
  match gas with
  | [] -> 0
  | ga0::gas' -> f ga0 gas' (Array.length ga0)
               
  
    
let pp_glyphs rendered_control_flow_common_prefix_end (ga: glyph array) = 
                                   (* convert glyph matrix into string array *)
  let pp_glyph = function
    | Glr L -> "\u{2500}" (*   *)
    | Gud L -> "\u{2502}" (*   *)
    | Gru L -> "\u{2514}" (*   *)
    | Grd L -> "\u{250c}" (*   *)
    | Grud L -> "\u{251c}" (*   *)
    | Glrud (L, L) -> "\u{253c}" (*   *)
    | Glr B -> "\u{2550}" (*   *)
    | Gud B -> "\u{2551}" (*   *)
    | Gru B -> "\u{255a}" (*   *)
    | Grd B -> "\u{2554}" (*   *)
    | Grud B -> "\u{2560}" (*   *)
    | Glrud (B, B) -> "\u{256c}" (*   *)
    | Glrud (L, B) -> "\u{256b}" (*   *)
    | Glrud (B, L) -> "\u{256a}" (*   *)
    | Ggt -> ">" (*   *)
    | Glt -> "<" (*   *)
    | GX -> "X" (*   *)
    | Gnone -> " " (*   *)
    | Gquery -> "?"
    (*   *)
  in

  let ga' =
    if rendered_control_flow_common_prefix_end < Array.length ga then
      Array.to_list (Array.sub ga rendered_control_flow_common_prefix_end (Array.length ga - rendered_control_flow_common_prefix_end))
    else
      [] in
  String.concat ""
    (List.map pp_glyph ga')
    
