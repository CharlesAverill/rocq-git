Require Extraction.
From rocqgit Require Import endpoints.

From Stdlib Require Import ExtrOcamlBasic ExtrOcamlNativeString ExtrOCamlInt63.

Extraction Language OCaml.
Unset Extraction Optimize.

(** IO Functions *)
Extract Constant open_out => "(fun s -> Ok (open_out s))".
Extract Inlined Constant output_string => "output_string".
Extract Inlined Constant mkdir => "Unix.mkdir".
Extract Inlined Constant makedirs => "Utils.makedirs".
Extract Inlined Constant file_exists => "Sys.file_exists".
Extract Inlined Constant dirname => "Filename.dirname".
Extract Inlined Constant print_endline => "print_endline".

Extract Inlined Constant compress_string => "Utils.compress_string".

(** Types *)
Extract Inlined Constant out_channel => "out_channel".
Extract Inductive result => "Utils.sresult"
  ["Ok" "Error"].

(** Extraction *)
(* We have to get out of _build since we're compiling with dune *)
Set Extraction Output Directory "../../rocqgit/lib/extr".

Recursive Extraction init.
Extraction "init.ml" init.

Recursive Extraction hash_object.
Extraction "hash.ml" hash_object.
