open Rocqgit_lib

let () =
  match Init.init "test" with
  | Ok _ -> ()
  | Error e -> print_endline e
