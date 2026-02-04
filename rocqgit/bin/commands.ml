type command = Init | Add | Commit | Push | Help | Hash

let string_of_command = function
  | Init ->
      "init"
  | Add ->
      "add"
  | Commit ->
      "commit"
  | Push ->
      "push"
  | Help ->
      "help"
  | Hash ->
      "hash"

let command_of_string = function
  | "init" ->
      Some Init
  | "add" ->
      Some Add
  | "commit" ->
      Some Commit
  | "push" ->
      Some Push
  | "help" ->
      Some Help
  | "hash" ->
      Some Hash
  | _ ->
      None
