open Rocqgit_lib
open Extr
open Commands
open Logging

(* Let-binding *)
let ( let* ) r f = match r with Ok x -> f x | Error e -> err "%s" e

(* Unit-binding *)
let ( let+ ) r f = match r with Ok () -> f () | Error e -> err "%s" e

(** Main function *)
let () =
  let args = Argparse.parse_arguments () in
  match args.subcommand, args.command_args with
  | Init, [repo_name] ->
      let* _repo = Init.init repo_name in
      ()
  | Init, _ ->
      fatal rc_CLI "usage: %s init <repo_folder>\nNote: repo_folder must not exist" Version._NAME
  | _ ->
      fatal rc_CLI "Subcommand %s is unimplemented"
        (string_of_command args.subcommand)
