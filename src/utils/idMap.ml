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

type ('a, 'b) t = { ids : ('a, int) Hashtbl.t; keys : 'a Vec.t; values : 'b Vec.t }

let make () = { ids = Hashtbl.create 10; keys = Vec.empty (); values = Vec.empty () }

exception Exists

let length m = Vec.length m.keys

let add m key value =
  if Hashtbl.mem m.ids key then raise Exists
  else
    let next_id = length m in
    Vec.add_one m.keys key;
    Vec.add_one m.values value;
    Hashtbl.add m.ids key next_id;
    next_id

let adds m key value = add m key value |> ignore

let to_ident m key = Hashtbl.find m.ids key

let to_ident_opt m key = Hashtbl.find_opt m.ids key

let of_ident m id = Vec.get m.keys id

let mem m key = Hashtbl.mem m.ids key

let mem_id m id = id >= 0 && id < length m

let geti m i = Vec.get m.values i

let unsafe_geti m i = Vec.unsafe_get m.values i

let getk m key = to_ident m key |> Vec.unsafe_get m.values

let getk_opt m key = to_ident_opt m key |> Option.map (Vec.unsafe_get m.values)

let seti m i v = Vec.set m.values i v

let unsafe_seti m i v = Vec.unsafe_set m.values i v

let setk m key value =
  let id = to_ident m key in
  Vec.unsafe_set m.values id value

let fill_all m v = Vec.fill_all m.values v

let iter f m =
  let len = Vec.length m.values in
  for i = 0 to len - 1 do
    f (Vec.unsafe_get m.keys i) i (Vec.unsafe_get m.values i)
  done

let map_to_list f m =
  let len = Vec.length m.values in
  let res = ref [] in
  for i = 0 to len - 1 do
    res := f (Vec.unsafe_get m.keys i) i (Vec.unsafe_get m.values i) :: !res
  done;
  List.rev !res

let pp ?(name = "") ~keys ~vals m =
  Pp.(
    mapping name
    @@ map_to_list
         (fun key id value -> (group (prefix 2 1 (int id ^^ dot) (keys key)), vals value))
         m)
