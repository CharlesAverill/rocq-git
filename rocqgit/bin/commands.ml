type command = Init | Add | Commit | Push | Help

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
  | _ ->
      None
