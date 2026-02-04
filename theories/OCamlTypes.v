From rocqgit Require Import monads.

From Stdlib Require Export Numbers.Cyclic.Int63.Sint63.

(* File IO types *)
Axiom out_channel : Type.
Axiom in_channel : Type.

(* File IO functions *)
Axiom open_out : string -> result out_channel.
Axiom open_in : string -> result in_channel.

Axiom output_string : out_channel -> string -> unit.

Definition file_perm : Set := int.
Definition fp777 : file_perm := 511%uint63.
Definition fp755 : file_perm := 493%uint63.
Definition fp644 : file_perm := 420%uint63.
Definition fp600 : file_perm := 384%uint63.

Axiom mkdir : string -> file_perm -> unit.
Axiom rmdir : string -> unit.
Axiom chdir : string -> unit.