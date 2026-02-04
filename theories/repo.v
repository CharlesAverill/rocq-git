From Stdlib Require Import String Ascii List Recdef.
Import ListNotations.
Open Scope string_scope.

From rocqgit Require Import monads.

(** File paths *)
Section Paths.

  Record path : Set := {
    absolute : bool;
    parts: list string
  }.

  Definition render (p : path) :=
    (if p.(absolute) then
      "/"
    else "") ++
    String.concat "/" p.(parts).

  Coercion render : path >-> string.

  Definition parse_path (s : string) : result path :=
    (* don't want any path containing whitespace *)
    if in_dec ascii_dec " "%char (list_ascii_of_string s)
    then fail "No spaces allowed in paths" else
    (* acc : idx * absolute * part * parts *)
    match
    fold_left (fun acc (chr : ascii) =>
                match acc, chr with
                | (0, false, None, []), "/" =>
                    (1, true, None, [])
                | (idx, abs, Some part, parts), "/" =>
                    (S idx, abs, None, List.app parts [part])
                | (idx, abs, None, parts), "/" =>
                    (S idx, abs, None, parts)
                | (idx, abs, None, parts), _ =>
                    (S idx, abs, Some (String chr ""), parts)
                | (idx, abs, Some part, parts), _ =>
                    (S idx, abs, Some (part ++ String chr ""), parts)
                end)%char
    (list_ascii_of_string s)
    (0, false, None, []) with
    | (_, absolute, part, parts) =>
      let parts :=
        match part with
        | None => parts
        | Some p => List.app parts [p]
        end in
      return {|absolute := absolute; parts := parts|}
    end.

  Coercion parse_path : string >-> result.

  (* p1 is a parent of p2 *)
  Fixpoint _parent (p1 p2 : list string) : bool :=
    match p1, p2 with
    | h1 :: nil, h2 :: _ =>
        h1 =? h2
    | _ :: _, _ :: nil => false
    | h1 :: t1, h2 :: t2 =>
      if h1 =? h2 then
        _parent t1 t2
      else false
    | nil, _ => true
    | _, _ => false
    end.
  Definition parent (p1 p2 : path) : bool :=
    _parent p1.(parts) p2.(parts).

  Definition concat_paths (p1 p2 : path) : result path :=
    if p2.(absolute) then
      fail "Can't concatenate an absolute path on the right"
    else
      return {|absolute := p1.(absolute);
               parts := p1.(parts) ++ p2.(parts)|}.
End Paths.
Notation "p1 '//' p2" := (x_ <- (p1 : result path);;
                          y_ <- (p2 : result path);;
                          concat_paths x_ y_) (at level 80, right associativity).

(** Git repository structure *)
Section Repository.

  Definition object : Set := nat.
  Definition ref : Set := nat.
  Definition head : Set := nat.

  Record repo : Set := {
    root: path;
    objects: list object;
    refs: list ref;
    heads: list head
  }.

  Definition fresh_repo (name : string) : result repo :=
    root <- name : result path ;;
    return {|root := root; objects := []; refs := []; heads := []|}.

  Definition git_dir (r : repo): path.
    destruct (Ok r.(root) // ".git") eqn:E.
      exact x.
    discriminate.
  Defined.

  Definition objects_dir (r : repo): path.
    destruct (return git_dir r // "objects") eqn:E.
      exact x.
    discriminate.
  Defined.

  Definition refs_dir (r : repo): path.
    destruct (return git_dir r // "refs") eqn:E.
      exact x.
    discriminate.
  Defined.

  Definition heads_dir (r : repo): path.
    destruct (return refs_dir r // "heads") eqn:E.
      exact x.
    discriminate.
  Defined.

  Definition HEAD_path (r : repo): path.
    destruct (return refs_dir r // "HEAD") eqn:E.
      exact x.
    discriminate.
  Defined.
End Repository.
















