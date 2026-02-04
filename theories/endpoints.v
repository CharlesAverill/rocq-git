From rocqgit Require Export repo monads OCamlTypes.

Definition mkdir' := (fun s => mkdir s fp755).

(** Initialization *)
Definition init (repo_name : string) : result repo :=
  (* Build object *)
  repo <- fresh_repo repo_name ;;

  (* Create directories *)
  let* _ <= mkdir' repo_name #;
  let* _ <= mkdir' (git_dir repo) #;
  let* _ <= mkdir' (objects_dir repo) #;
  let* _ <= mkdir' (refs_dir repo) #;
  let* _ <= mkdir' (heads_dir repo) #;

  (* Write to HEAD *)
  oc <- open_out (HEAD_path repo) ;;
  let* _ <= output_string oc "ref: refs/heads/main
" #;

  let* _ <= print_endline ("Initialized empty Git repository in " ++ repo_name) #;

  return repo.