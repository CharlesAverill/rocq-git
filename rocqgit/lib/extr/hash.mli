type nat = O | S of nat

val fst : 'a1 * 'a2 -> 'a1

val app : 'a1 list -> 'a1 list -> 'a1 list

val add : nat -> nat -> nat

val sub : nat -> nat -> nat

module Nat : sig
  val leb : nat -> nat -> bool

  val ltb : nat -> nat -> bool

  val divmod : nat -> nat -> nat -> nat -> nat * nat

  val div : nat -> nat -> nat
end

type positive = XI of positive | XO of positive | XH

type n = N0 | Npos of positive

type z = Z0 | Zpos of positive | Zneg of positive

module Pos : sig
  val succ : positive -> positive

  val pred_double : positive -> positive

  val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1

  val to_nat : positive -> nat

  val of_succ_nat : nat -> positive
end

module Coq_Pos : sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val mul : positive -> positive -> positive
end

module N : sig
  val add : n -> n -> n

  val mul : n -> n -> n

  val to_nat : n -> nat

  val of_nat : nat -> n
end

val nth : nat -> 'a1 list -> 'a1 -> 'a1

val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val rev : 'a1 list -> 'a1 list

val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1

module Z : sig
  val double : z -> z

  val succ_double : z -> z

  val opp : z -> z
end

module Coq_Z : sig
  val to_nat : z -> nat

  val of_nat : nat -> z
end

val zero : char

val one : char

val shift : bool -> char -> char

val ascii_of_pos : positive -> char

val ascii_of_N : n -> char

val ascii_of_nat : nat -> char

val n_of_digits : bool list -> n

val n_of_ascii : char -> n

val nat_of_ascii : char -> nat

val length : string -> nat

val substring : nat -> nat -> string -> string

type path = {absolute: bool; parts: string list}

val render : path -> string

val parse_path : string -> path Utils.sresult

val concat_paths : path -> path -> path Utils.sresult

type object0 = nat

type ref = nat

type head = nat

type repo = {root: path; objects: object0 list; refs: ref list; heads: head list}

val lsl0 : Uint63.t -> Uint63.t -> Uint63.t

val lsr0 : Uint63.t -> Uint63.t -> Uint63.t

val land0 : Uint63.t -> Uint63.t -> Uint63.t

val lor0 : Uint63.t -> Uint63.t -> Uint63.t

val lxor0 : Uint63.t -> Uint63.t -> Uint63.t

val asr0 : Uint63.t -> Uint63.t -> Uint63.t

val add0 : Uint63.t -> Uint63.t -> Uint63.t

val sub0 : Uint63.t -> Uint63.t -> Uint63.t

val mul0 : Uint63.t -> Uint63.t -> Uint63.t

val divs : Uint63.t -> Uint63.t -> Uint63.t

val mods : Uint63.t -> Uint63.t -> Uint63.t

val eqb : Uint63.t -> Uint63.t -> bool

val ltb0 : Uint63.t -> Uint63.t -> bool

val ltsb : Uint63.t -> Uint63.t -> bool

val lesb : Uint63.t -> Uint63.t -> bool

val size : nat

val is_zero : Uint63.t -> bool

val is_even : Uint63.t -> bool

val to_Z_rec : nat -> Uint63.t -> z

val to_Z : Uint63.t -> z

val of_pos_rec : nat -> positive -> Uint63.t

val of_pos : positive -> Uint63.t

val of_Z : z -> Uint63.t

val min_int : Uint63.t

val to_Z0 : Uint63.t -> z

val max_int : Uint63.t

val open_out : string -> out_channel Utils.sresult

type file_perm = Uint63.t

val fp755 : file_perm

val lnot : Uint63.t -> Uint63.t

val left_rotate : Uint63.t -> Uint63.t -> Uint63.t

val h0 : Uint63.t

val h1 : Uint63.t

val h2 : Uint63.t

val h3 : Uint63.t

val h4 : Uint63.t

val k0 : Uint63.t

val k1 : Uint63.t

val k2 : Uint63.t

val k3 : Uint63.t

val append_char_n_times : string -> char -> Uint63.t -> string

val byte_of_int_big_endian : Uint63.t -> Uint63.t -> char

val bytes_of_int_big_endian : Uint63.t -> nat -> string

val bytes_to_int : char -> char -> char -> char -> Uint63.t

val get_chunk : string -> Uint63.t list -> nat -> Uint63.t list

type hash = (((Uint63.t * Uint63.t) * Uint63.t) * Uint63.t) * Uint63.t

val process_chunk :
     Uint63.t list
  -> Uint63.t
  -> Uint63.t
  -> Uint63.t
  -> Uint63.t
  -> Uint63.t
  -> hash

val process_message :
     string
  -> Uint63.t
  -> Uint63.t
  -> Uint63.t
  -> Uint63.t
  -> Uint63.t
  -> nat
  -> hash

val sha1 : string -> hash

type object_type = Blob | Commit | Tree

val string_of_object_type : object_type -> string

val string_of_nat_aux : Uint63.t -> string -> string

val string_of_nat : nat -> string

val hash_object : repo -> string -> object_type -> bool -> hash Utils.sresult
