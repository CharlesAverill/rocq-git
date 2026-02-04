open Rocqgit_lib
open Extr
open Commands
open Logging
open Hash

(* Let-binding *)
let ( let* ) r f = match r with Ok x -> f x | Error e -> err "%s" e

(* Unit-binding *)
let ( let+ ) r f = match r with Ok () -> f () | Error e -> err "%s" e

let hash_to_hex_string ((((h0, h1), h2), h3), h4) =
  Printf.sprintf "%08Lx%08Lx%08Lx%08Lx%08Lx"
    (Int64.of_int h0)
    (Int64.of_int h1)
    (Int64.of_int h2)
    (Int64.of_int h3)
    (Int64.of_int h4)

(** Main function *)
let () =
  let args = Argparse.parse_arguments () in
  match (args.subcommand, args.command_args) with
  | Init, [repo_name] ->
      let* _repo = Init.init repo_name in
      ()
  | Init, _ ->
      fatal rc_CLI
        "usage: %s init <repo_folder>\nNote: repo_folder must not exist"
        Version._NAME
  | Hash, [string] ->
      print_endline (hash_to_hex_string (sha1 string))
  | _ ->
      fatal rc_CLI "Subcommand %s is unimplemented"
        (string_of_command args.subcommand)
