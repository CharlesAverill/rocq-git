open Commands
open Rocqgit_lib
open Logging

type arguments = {subcommand: command; command_args: string list}

let commands_help =
  "
Supported Commands
==================

start a working area
  init      Create an empty Git repository or reinitialize an existing one
  "

(** Parse command-line arguments *)
let parse_arguments () =
  let sc = ref None in
  let print_version = ref false in
  (* Parse anonymous arguments, e.g. commands *)
  let anon_args_list = ref [] in
  let anon_args arg =
    if !anon_args_list = [] && !sc = None then (
      match command_of_string arg with
    | None -> fatal rc_CLI "Unknown subcommand %s" arg
    | Some arg -> sc := Some arg
    ) else
    anon_args_list := !anon_args_list @ [arg]
  in
  (* Options *)
  let speclist =
    [ ("-v", Arg.Set print_version, "Print version information")
    ; ("--verbose", Arg.Set print_version, "Print version information") ]
  in
  (* Usage *)
  let usage_msg = Printf.sprintf "usage: %s [-v | --version] <command> args\n%s" Version._NAME commands_help in
  (* Parse *)
  Arg.parse speclist anon_args usage_msg ;
  (* Check for version printing *)
  if !print_version then (
    Printf.printf "%s version %s - %s\n" Version._NAME Version._VERSION
      Version._AUTHOR ;
    exit 0
  ) else
    {subcommand= (match !sc with None -> Help | Some sc' -> sc'); command_args = !anon_args_list}
