Require Extraction.
From rocqgit Require Import endpoints.

From Stdlib Require Import ExtrOcamlBasic ExtrOcamlNativeString ExtrOCamlInt63.

Extraction Language OCaml.
Unset Extraction Optimize.

(** IO Functions *)
Extract Constant open_out => "(fun s -> Ok (open_out s))".
Extract Constant open_in => "(fun s -> Ok (open_in s))".
Extract Inlined Constant output_string => "output_string".
Extract Inlined Constant mkdir => "Unix.mkdir".

(** Types *)
Extract Inlined Constant out_channel => "out_channel".

(** Extraction *)
(* We have to get out of _build since we're compiling with dune *)
Set Extraction Output Directory "../../rocqgit/lib".

Recursive Extraction init.
Extraction "init.ml" init.
