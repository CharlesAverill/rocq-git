From rocqgit Require Import monads.

From Stdlib Require Export Numbers.Cyclic.Int63.Sint63.

(* File IO types *)
Axiom out_channel : Type.

(* File IO functions *)
Axiom open_out : string -> result out_channel.

Axiom output_string : out_channel -> string -> unit.

Axiom file_exists : string -> bool.
Axiom dirname : string -> string.

Definition file_perm : Set := int.
Definition fp777 : file_perm := 511%uint63.
Definition fp755 : file_perm := 493%uint63.
Definition fp644 : file_perm := 420%uint63.
Definition fp600 : file_perm := 384%uint63.

Axiom mkdir : string -> file_perm -> unit.
Axiom makedirs : string -> file_perm -> unit.

(* IO functions *)
Axiom print_endline : string -> unit.

(* Misc *)
Axiom compress_string : string -> string.