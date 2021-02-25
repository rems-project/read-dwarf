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

(* The documentation is in the mli file *)

type t = int

let length = Sys.int_size

let back = length - 1

let check_index i =
  if i < 0 || i >= length then Raise.inv_arg "Indexing in an int with index %d" i

let check_range i l =
  check_index i;
  if l < 1 || l > length then Raise.inv_arg "Range indexing in an int at %d of len %d" i l

let unsafe_get bf i = (bf lsr i) mod 2 = 1

let unsafe_set bf i = bf lor (1 lsl i)

let unsafe_clear bf i = bf land lnot (1 lsl i)

let unsafe_setb bf i b = if b then unsafe_set bf i else unsafe_clear bf i

let get bf i =
  check_index i;
  unsafe_get bf i

let set bf i =
  check_index i;
  unsafe_set bf i

let clear bf i =
  check_index i;
  unsafe_clear bf i

let setb bf i b =
  check_index i;
  unsafe_setb bf i b

let init b = if b then -1 else 0

let unsafe_mask i l = (-1 lsr (length - l)) lsl i

let mask i l =
  check_range i l;
  unsafe_mask i l

let unsafe_set_range bf i l = bf lor unsafe_mask i l

let set_range bf i l =
  check_range i l;
  unsafe_set_range bf i l

let unsafe_clear_range bf i l = bf land lnot (unsafe_mask i l)

let clear_range bf i l =
  check_range i l;
  unsafe_clear_range bf i l

let unsafe_sub bf i l = (bf lsr i) land (-1 lsr (length - l))

let sub bf i l =
  check_range i l;
  unsafe_sub bf i l

let unsafe_set_sub bf i l data =
  let cleared = unsafe_clear_range bf i l in
  cleared lor (data lsl i)

let set_sub bf i l data =
  check_range i l;
  unsafe_set_sub bf i l (unsafe_sub data 0 l)

let unsafe_blit src isrc dest idest len =
  let data = unsafe_sub src isrc len in
  unsafe_set_sub dest idest len data

let blit src isrc dest idest len =
  check_range isrc len;
  check_range idest len;
  unsafe_blit src isrc dest idest len
