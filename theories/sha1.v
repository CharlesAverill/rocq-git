From Stdlib Require Export Numbers.Cyclic.Int63.Sint63.
From Stdlib Require Import String Ascii ZArith List.
From Stdlib Require Import Recdef Program.Wf Lia.
Import ListNotations.

Open Scope sint63_scope.

Infix ".|" := PrimInt63.lor (at level 40, left associativity).
Infix ".&" := PrimInt63.land (at level 40, left associativity).
Infix ".^" := PrimInt63.lxor (at level 40, left associativity).

Definition lnot (x : int) : int :=
  max_int - x.

Definition left_rotate (n b : int) : int :=
  ((n << b) .| (n >> (32 - b))) .& 0xffffffff.

(* Initial hash values *)
Definition h0 : int := 0x67452301.
Definition h1 : int := 0xEFCDAB89.
Definition h2 : int := 0x98BADCFE.
Definition h3 : int := 0x10325476.
Definition h4 : int := 0xC3D2E1F0.

(* SHA-1 constants *)
Definition k0 : int := 0x5A827999.
Definition k1 : int := 0x6ED9EBA1.
Definition k2 : int := 0x8F1BBCDC.
Definition k3 : int := 0xCA62C1D6.

Function append_char_n_times (s : string) (a : ascii) (n : int)
    {measure (fun x => (Z.to_nat (to_Z x))) n} : string :=
  if (n <=? 0) then
    s
  else
    ((append_char_n_times s a (n - 1)) ++ (String a ""))%string.
Proof.
  intros s a n nz.
  pose proof (lebP n 0). destruct H. discriminate.
  change (to_Z 0) with 0%Z in n0.
  assert (0 < to_Z n)%Z by lia.
  rewrite to_Z_sub.
    change (to_Z 1) with 1%Z.
    rewrite Z2Nat.inj_sub by now compute.
    change (Z.to_nat 1) with 1%nat.
    lia.
  split.
    change (to_Z 1) with 1%Z.
    change (to_Z min_int) with (-4611686018427387904)%Z. lia.
  pose proof (to_Z_bounded n). destruct H0.
  change (to_Z 1) with 1%Z. lia.
Qed.

Definition byte_of_int_big_endian (n : int) (i : int) : ascii :=
  let shift := 8 * (7 - i) in
  let byte := (n >> shift) .& 0xff in
  ascii_of_nat (Z.to_nat (to_Z byte)).

Fixpoint bytes_of_int_big_endian (n : int) (i : nat) : string :=
  match i with
  | 0 => EmptyString
  | S i' =>
        bytes_of_int_big_endian n i' ++
        String (byte_of_int_big_endian n (of_Z (Z.of_nat i'))) EmptyString
  end.

(* Convert 4 bytes to a 32-bit integer (big-endian) *)
Definition bytes_to_int (b0 b1 b2 b3 : ascii) : int :=
  let n0 := of_Z (Z.of_nat (nat_of_ascii b0)) in
  let n1 := of_Z (Z.of_nat (nat_of_ascii b1)) in
  let n2 := of_Z (Z.of_nat (nat_of_ascii b2)) in
  let n3 := of_Z (Z.of_nat (nat_of_ascii b3)) in
  ((n0 << 24) .| (n1 << 16) .| (n2 << 8) .| n3) .& 0xffffffff.

(* Extract a 512-bit chunk from the message *)
Fixpoint get_chunk (s : string) (chunk : list int) (count : nat) : list int :=
  match count with
  | 0 => List.rev chunk
  | S count' =>
      match s with
      | String b0 (String b1 (String b2 (String b3 rest))) =>
          get_chunk rest (bytes_to_int b0 b1 b2 b3 :: chunk) count'
      | _ => List.rev chunk (* shouldn't happen with proper padding *)
      end
  end.

Definition hash : Set := int * int * int * int * int.

(* Process a single 512-bit chunk *)
Definition process_chunk (chunk : list int) (h0 h1 h2 h3 h4 : int) : hash :=
  (* Extend the 16 32-bit words into 80 32-bit words *)
  let fix extend_words (w : list int) ( i : nat) : list int :=
    match i with
    | 0 => w
    | S i' =>
        let w_new := List.nth (80 - i) w 0 .^
                     List.nth (80 - i + 2) w 0 .^
                     List.nth (80 - i + 8) w 0 .^
                     List.nth (80 - i + 13) w 0 in
        extend_words (w ++ [left_rotate w_new 1])%list i'
    end
  in
  let w := extend_words chunk (Z.to_nat 64) in

  (* Main loop *)
  let fix main_loop (a b c d e : int) (i : nat) : hash :=
    match i with
    | 0 => (a, b, c, d, e)
    | S i' =>
        let i_val := 80 - (of_Z (Z.of_nat i)) in
        let '(f, k) :=
          if (i_val <? 20)%int63 then
            ((b .& c) .| ((lnot b .& 0xffffffff) .& d), k0)
          else if (i_val <? 40)%int63 then
            (b .^ c .^ d, k1)
          else if (i_val <? 60)%int63 then
            ((b .& c) .| (b .& d) .| (c .& d), k2)
          else
            (b .^ c .^ d, k3)
        in
        let temp := ((left_rotate a 5) + f + e + k + (List.nth (Z.to_nat (to_Z i_val)) w 0)) .& 0xffffffff in
        main_loop temp a (left_rotate b 30) c d i'
    end
  in

  let '(a, b, c, d, e) := main_loop h0 h1 h2 h3 h4 80%nat in
  ((h0 + a) .& 0xffffffff,
   (h1 + b) .& 0xffffffff,
   (h2 + c) .& 0xffffffff,
   (h3 + d) .& 0xffffffff,
   (h4 + e) .& 0xffffffff). 

(* Process all chunks *)
Fixpoint process_message (s : string) (h0 h1 h2 h3 h4 : int) (fuel : nat) : hash :=
  match fuel with
  | 0 => (h0, h1, h2, h3, h4)
  | S fuel' =>
      if (String.length s <? 64)%nat then
        (h0, h1, h2, h3, h4)
      else
        let chunk := get_chunk s [] 16 in
        let '(h0', h1', h2', h3', h4') := process_chunk chunk h0 h1 h2 h3 h4 in
        let remaining := substring 64 (String.length s - 64) s in
        process_message remaining h0' h1' h2' h3' h4' fuel'
  end.

Definition sha1 (message : string) : (hash) :=
  let byte_len := of_Z (Z.of_nat (String.length message)) in
  let bit_len := 8 * byte_len in

  (* Padding *)
  let message := (message ++ String (ascii_of_nat 128) "")%string in
  let message := append_char_n_times message (ascii_of_nat 0) 
                   ((56 - (byte_len + 1) mod 64) mod 64) in
  let message := (message ++ bytes_of_int_big_endian bit_len 8)%string in

  (* Process message *)
  let num_chunks := Nat.div (String.length message) 64 in
  process_message message h0 h1 h2 h3 h4 num_chunks.