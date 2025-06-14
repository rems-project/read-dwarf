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
(**       extract DWARF inlining data                                        *)

(*****************************************************************************)

open Logs.Logger (struct
  let str = __MODULE__
end)

open ElfTypes
open ControlFlowTypes

let mk_inlining test sdt instructions =
  (* compute the inlining data *)
  (*let iss = Dwarf.analyse_inlined_subroutines test.dwarf_static.ds_dwarf in*)
  let iss = Dwarf.analyse_inlined_subroutines_sdt_dwarf sdt in
  let issr = Dwarf.analyse_inlined_subroutines_by_range iss in

  (* walk over instructions annotating with inlining data *)
  let rec f issr_current issr_rest label_last max_labels k acc =
    if k = Array.length instructions then (List.rev_append acc [], max_labels)
    else
      let i = instructions.(k) in
      let addr = i.i_addr in
      let issr_still_current =
        List.filter
          (function (_label, ((_n1, n2), (_m, _n), _is)) -> Sym.Ordered.less addr n2)
          issr_current
      in

      let rec find_first discard p acc xs =
        match xs with
        | [] -> (List.rev_append acc [], xs)
        | x :: xs' ->
            if discard x then find_first discard p acc xs'
            else if p x then find_first discard p (x :: acc) xs'
            else (List.rev_append acc [], xs)
      in

      let (issr_starting_here0, issr_rest') =
        find_first
          (function ((_n1, n2), (_m, _n), _is) -> Sym.Ordered.less_equal n2 addr)
          (function ((n1, _n2), (_m, _n), _is) -> Sym.Ordered.equal n1 addr)
          [] issr_rest
      in

      let rec enlabel labels_in_use label_last acc issr_new =
        match issr_new with
        | [] -> (List.rev_append acc [], label_last)
        | issr :: issr_new' ->
            if List.length labels_in_use >= 26 then fatal "%s" "inlining depth > 26";
            let rec fresh_label l =
              let l = (l + 1) mod 26 in
              if not (List.mem Int.equal l labels_in_use) then l else fresh_label l
            in
            let l = fresh_label label_last in
            enlabel (l :: labels_in_use) l ((l, issr) :: acc) issr_new'
      in

      let (issr_starting_here, label_last') =
        enlabel
          (List.map (function (label, _) -> label) issr_current)
          label_last [] issr_starting_here0
      in

      let issr_current' = issr_still_current @ issr_starting_here in

      let max_labels' = max max_labels (List.length issr_current') in

      let pp_label label =
        String.make 1
          (Char.chr
             ((label
             + 11
               (*offset to make mpool_fini inlining start with a, not the confusingly-identical-to-a-variable-name p*)
              )
              mod 26
             + Char.code 'a'
             ))
      in

      let ppd_labels =
        String.concat "" (List.map (function (label, _) -> pp_label label) issr_current')
      in

      let ppd_new_inlining =
        String.concat ""
          (List.map
             (function
               | (label, x) ->
                   pp_label label ^ ": "
                   ^ Dwarf.pp_inlined_subroutines_by_range test.dwarf_static [x])
             issr_starting_here)
      in

      let acc' = (ppd_labels, ppd_new_inlining, issr_current') :: acc in

      f issr_current' issr_rest' label_last' max_labels' (k + 1) acc'
  in

  let (inlining_list, max_labels) = f [] issr 25 0 0 [] in
  let inlining = Array.of_list inlining_list in

  let pp_inlining_label_prefix s =
    " " ^ s ^ String.make (max_labels - String.length s) ' ' ^ " "
  in

  (inlining, pp_inlining_label_prefix)
