From rocqgit Require Export repo monads OCamlTypes sha1.
From Stdlib Require Import Recdef Program.Wf.
From Stdlib Require Import String Ascii ZArith Lia.

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

(** Hashing *)
Inductive object_type :=
  | Blob
  | Commit
  | Tree.

Definition string_of_object_type t : string :=
  match t with
  | Blob => "blob"
  | Commit => "commit"
  | Tree => "tree"
  end.

Function string_of_nat_aux (n : int) (acc : string)
    {measure (fun x => Z.to_nat (to_Z x)) n} : string :=
  if n <=? 0 then acc else
  string_of_nat_aux (n / 10) 
    (String (ascii_of_nat (Z.to_nat (to_Z (48 + n mod 10)))) acc).
Proof.
  intros n acc nz.
  pose proof (lebP n 0). destruct H. discriminate.
  change (to_Z 0) with 0%Z in n0.
  assert (0 < to_Z n)%Z by lia.
  rewrite div_spec.
    change (to_Z 10) with 10%Z.
    pose proof (Z.quot_lt_upper_bound (to_Z n) 10 (to_Z n)).
    lia.
  right. discriminate.
Qed.

Definition string_of_nat (n : nat) : string :=
  match n with
  | O => "0"
  | _ => string_of_nat_aux (of_Z (Z.of_nat n)) ""
  end.

Definition int_to_hex_string (n : int) : string :=
  let fix byte_to_hex (b : nat) : string :=
    let hi := of_Z (Z.of_nat (Nat.div b 16)) in
    let lo := of_Z (Z.of_nat (Nat.modulo b 16)) in
    let hex_digit d :=
      if d <? 10 then ascii_of_nat (Z.to_nat (to_Z (d + 48)))
      else ascii_of_nat (Z.to_nat (to_Z (d - 10 + 97))) in
    String (hex_digit hi) (String (hex_digit lo) "") in

  let fix aux (n : int) (i : nat) (acc : string) : string :=
    match i with
    | 0 => acc
    | S i' =>
        let byte := (n >> (of_Z (Z.of_nat (i' * 8)))) .& 0xff in
        let byte_nat := Z.to_nat (to_Z byte) in
        aux n i' (acc ++ byte_to_hex byte_nat)
    end in
  aux n 8%nat "".

Definition hash_to_hex_string (h : int * int * int * int * int) : string :=
  let '(h0, h1, h2, h3, h4) := h in
  int_to_hex_string h0 ++
  int_to_hex_string h1 ++
  int_to_hex_string h2 ++
  int_to_hex_string h3 ++
  int_to_hex_string h4.

Definition hash_object (repo : repo) (data : string) (t : object_type) (write : bool) : result hash :=
  (* Create header: "type length" *)
  let type_str := string_of_object_type t in
  let len_str := string_of_nat (String.length data) in
  let header := type_str ++ " " ++ len_str in

  (* Create full data: header + '\x00' + data *)
  let full_data := header ++ String (ascii_of_nat 0) data in

  (* Compute SHA-1 hash *)
  let '(sha0, sha1, sha2, sha3, sha4) :=
    sha1 full_data in

  _ <- if write then (
    path <- repo.(root) // ".git" // "objects" ;;
    if file_exists (render path) then
      fail "object path collision"
    else
      let* _ <= makedirs (dirname path) fp755 #;
      oc <- open_out path ;;
      let* _ <= output_string oc (compress_string full_data) #;
      return tt
  ) else return tt ;;

  return (sha0, sha1, sha2, sha3, sha4).
  