
type nat =
| O
| S of nat

val app : 'a1 list -> 'a1 list -> 'a1 list

val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1

type path = { absolute : bool; parts : string list }

val render : path -> string

val parse_path : string -> path Utils.sresult

val concat_paths : path -> path -> path Utils.sresult

type object0 = nat

type ref = nat

type head = nat

type repo = { root : path; objects : object0 list; refs : ref list;
              heads : head list }

val fresh_repo : string -> repo Utils.sresult

val git_dir : repo -> path

val objects_dir : repo -> path

val refs_dir : repo -> path

val heads_dir : repo -> path

val hEAD_path : repo -> path

val open_out : string -> out_channel Utils.sresult

type file_perm = Uint63.t

val fp755 : file_perm



val mkdir' : string -> unit

val init : string -> repo Utils.sresult
