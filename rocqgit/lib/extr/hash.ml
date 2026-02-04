let __ =
  let rec f _ = Obj.repr f in
  Obj.repr f

type nat = O | S of nat

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function x, _ -> x

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let app x =
  let rec app0 l m = match l with [] -> m | a :: l1 -> a :: app0 l1 m in
  app0 x

(** val add : nat -> nat -> nat **)

let rec add n0 m = match n0 with O -> m | S p -> S (add p m)

(** val sub : nat -> nat -> nat **)

let rec sub n0 m =
  match n0 with O -> n0 | S k -> ( match m with O -> n0 | S l -> sub k l )

module Nat = struct
  (** val leb : nat -> nat -> bool **)

  let rec leb n0 m =
    match n0 with
    | O ->
        true
    | S n' -> (
      match m with O -> false | S m' -> leb n' m' )

  (** val ltb : nat -> nat -> bool **)

  let ltb n0 m = leb (S n0) m

  (** val divmod : nat -> nat -> nat -> nat -> nat * nat **)

  let rec divmod x y q u =
    match x with
    | O ->
        (q, u)
    | S x' -> (
      match u with O -> divmod x' y (S q) y | S u' -> divmod x' y q u' )

  (** val div : nat -> nat -> nat **)

  let div x y = match y with O -> y | S y' -> fst (divmod x y' O y')
end

type positive = XI of positive | XO of positive | XH

type n = N0 | Npos of positive

type z = Z0 | Zpos of positive | Zneg of positive

module Pos = struct
  (** val succ : positive -> positive **)

  let rec succ = function XI p -> XO (succ p) | XO p -> XI p | XH -> XO XH

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
    | XI p ->
        XI (XO p)
    | XO p ->
        XI (pred_double p)
    | XH ->
        XH

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

  let iter_op op =
    let rec iter p a =
      match p with
      | XI p0 ->
          op a (iter p0 (op a a))
      | XO p0 ->
          iter p0 (op a a)
      | XH ->
          a
    in
    iter

  (** val to_nat : positive -> nat **)

  let to_nat x = iter_op add x (S O)

  (** val of_succ_nat : nat -> positive **)

  let rec of_succ_nat = function O -> XH | S x -> succ (of_succ_nat x)
end

module Coq_Pos = struct
  (** val succ : positive -> positive **)

  let rec succ = function XI p -> XO (succ p) | XO p -> XI p | XH -> XO XH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | XI p -> (
      match y with
      | XI q ->
          XO (add_carry p q)
      | XO q ->
          XI (add p q)
      | XH ->
          XO (succ p) )
    | XO p -> (
      match y with XI q -> XI (add p q) | XO q -> XO (add p q) | XH -> XI p )
    | XH -> (
      match y with XI q -> XO (succ q) | XO q -> XI q | XH -> XO XH )

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | XI p -> (
      match y with
      | XI q ->
          XI (add_carry p q)
      | XO q ->
          XO (add_carry p q)
      | XH ->
          XI (succ p) )
    | XO p -> (
      match y with
      | XI q ->
          XO (add_carry p q)
      | XO q ->
          XI (add p q)
      | XH ->
          XO (succ p) )
    | XH -> (
      match y with XI q -> XI (succ q) | XO q -> XO (succ q) | XH -> XI XH )

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with XI p -> add y (XO (mul p y)) | XO p -> XO (mul p y) | XH -> y
end

module N = struct
  (** val add : n -> n -> n **)

  let add n0 m =
    match n0 with
    | N0 ->
        m
    | Npos p -> (
      match m with N0 -> n0 | Npos q -> Npos (Coq_Pos.add p q) )

  (** val mul : n -> n -> n **)

  let mul n0 m =
    match n0 with
    | N0 ->
        N0
    | Npos p -> (
      match m with N0 -> N0 | Npos q -> Npos (Coq_Pos.mul p q) )

  (** val to_nat : n -> nat **)

  let to_nat = function N0 -> O | Npos p -> Pos.to_nat p

  (** val of_nat : nat -> n **)

  let of_nat = function O -> N0 | S n' -> Npos (Pos.of_succ_nat n')
end

(** val nth : nat -> 'a1 list -> 'a1 -> 'a1 **)

let nth n0 =
  let rec nth0 n1 l default =
    match n1 with
    | O -> (
      match l with [] -> default | x :: _ -> x )
    | S m -> (
      match l with [] -> default | _ :: l' -> nth0 m l' default )
  in
  nth0 n0

(** val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let in_dec h a l =
  let rec f = function
    | [] ->
        false
    | y :: l1 ->
        let s = h y a in
        if s then
          true
        else if f l1 then
          true
        else
          false
  in
  f l

(** val rev : 'a1 list -> 'a1 list **)

let rev l =
  let rec rev0 = function [] -> [] | x :: l' -> app (rev0 l') (x :: []) in
  rev0 l

(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let fold_left f =
  let rec fold_left0 l a0 =
    match l with [] -> a0 | b :: l0 -> fold_left0 l0 (f a0 b)
  in
  fold_left0

module Z = struct
  (** val double : z -> z **)

  let double = function
    | Z0 ->
        Z0
    | Zpos p ->
        Zpos (XO p)
    | Zneg p ->
        Zneg (XO p)

  (** val succ_double : z -> z **)

  let succ_double = function
    | Z0 ->
        Zpos XH
    | Zpos p ->
        Zpos (XI p)
    | Zneg p ->
        Zneg (Pos.pred_double p)

  (** val opp : z -> z **)

  let opp = function Z0 -> Z0 | Zpos x0 -> Zneg x0 | Zneg x0 -> Zpos x0
end

module Coq_Z = struct
  (** val to_nat : z -> nat **)

  let to_nat = function Z0 -> O | Zpos p -> Pos.to_nat p | Zneg _ -> O

  (** val of_nat : nat -> z **)

  let of_nat = function O -> Z0 | S n1 -> Zpos (Pos.of_succ_nat n1)
end

(** val zero : char **)

let zero = '\000'

(** val one : char **)

let one = '\001'

(** val shift : bool -> char -> char **)

let shift =
 fun b c ->
  Char.chr
    ( ((Char.code c lsl 1) land 255)
    +
    if b then
      1
    else
      0 )

(** val ascii_of_pos : positive -> char **)

let ascii_of_pos =
  let rec loop n0 p =
    match n0 with
    | O ->
        zero
    | S n' -> (
      match p with
      | XI p' ->
          shift true (loop n' p')
      | XO p' ->
          shift false (loop n' p')
      | XH ->
          one )
  in
  loop (S (S (S (S (S (S (S (S O))))))))

(** val ascii_of_N : n -> char **)

let ascii_of_N = function N0 -> zero | Npos p -> ascii_of_pos p

(** val ascii_of_nat : nat -> char **)

let ascii_of_nat a = ascii_of_N (N.of_nat a)

(** val n_of_digits : bool list -> n **)

let rec n_of_digits = function
  | [] ->
      N0
  | b :: l' ->
      N.add
        ( if b then
            Npos XH
          else
            N0 )
        (N.mul (Npos (XO XH)) (n_of_digits l'))

(** val n_of_ascii : char -> n **)

let n_of_ascii a =
  (* If this appears, you're using Ascii internals. Please don't *)
  (fun f c ->
    let n = Char.code c in
    let h i = n land (1 lsl i) <> 0 in
    f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7) )
    (fun a0 a1 a2 a3 a4 a5 a6 a7 -> n_of_digits [a0; a1; a2; a3; a4; a5; a6; a7])
    a

(** val nat_of_ascii : char -> nat **)

let nat_of_ascii a = N.to_nat (n_of_ascii a)

(** val length : string -> nat **)

let rec length s =
  (* If this appears, you're using String internals. Please don't *)
  (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then
      f0 ()
    else
      f1 (String.get s 0) (String.sub s 1 (l - 1)) )
    (fun _ -> O)
    (fun _ s' -> S (length s'))
    s

(** val substring : nat -> nat -> string -> string **)

let rec substring n0 m s =
  match n0 with
  | O -> (
    match m with
    | O ->
        ""
    | S m' ->
        (* If this appears, you're using String internals. Please don't *)
        (fun f0 f1 s ->
          let l = String.length s in
          if l = 0 then
            f0 ()
          else
            f1 (String.get s 0) (String.sub s 1 (l - 1)) )
          (fun _ -> s)
          (fun c s' ->
            (* If this appears, you're using String internals. Please don't *)
            (fun (c, s) -> String.make 1 c ^ s) (c, substring O m' s') )
          s )
  | S n' ->
      (* If this appears, you're using String internals. Please don't *)
      (fun f0 f1 s ->
        let l = String.length s in
        if l = 0 then
          f0 ()
        else
          f1 (String.get s 0) (String.sub s 1 (l - 1)) )
        (fun _ -> s)
        (fun _ s' -> substring n' m s')
        s

type path = {absolute: bool; parts: string list}

(** val render : path -> string **)

let render p =
  ( if p.absolute then
      "/"
    else
      "" )
  ^ String.concat "/" p.parts

(** val parse_path : string -> path Utils.sresult **)

let parse_path s =
  if
    in_dec ( = ) ' ' ((fun s -> List.init (String.length s) (fun i -> s.[i])) s)
  then
    Error "No spaces allowed in paths"
  else
    let p, parts0 =
      fold_left
        (fun acc chr ->
          let y, parts0 = acc in
          let y0, y1 = y in
          let idx, abs = y0 in
          match idx with
          | O -> (
              if abs then
                match y1 with
                | Some part ->
                    (* If this appears, you're using Ascii internals. Please don't *)
                    (fun f c ->
                      let n = Char.code c in
                      let h i = n land (1 lsl i) <> 0 in
                      f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7) )
                      (fun b b0 b1 b2 b3 b4 b5 b6 ->
                        if b then
                          if b0 then
                            if b1 then
                              if b2 then
                                if b3 then
                                  ( ( (S idx, abs)
                                    , Some
                                        ( part
                                        ^
                                        (* If this appears, you're using String internals. Please don't *)
                                        (fun (c, s) -> String.make 1 c ^ s)
                                          (chr, "") ) )
                                  , parts0 )
                                else if b4 then
                                  if b5 then
                                    ( ( (S idx, abs)
                                      , Some
                                          ( part
                                          ^
                                          (* If this appears, you're using String internals. Please don't *)
                                          (fun (c, s) -> String.make 1 c ^ s)
                                            (chr, "") ) )
                                    , parts0 )
                                  else if b6 then
                                    ( ( (S idx, abs)
                                      , Some
                                          ( part
                                          ^
                                          (* If this appears, you're using String internals. Please don't *)
                                          (fun (c, s) -> String.make 1 c ^ s)
                                            (chr, "") ) )
                                    , parts0 )
                                  else
                                    ( ((S idx, abs), None)
                                    , app parts0 (part :: []) )
                                else
                                  ( ( (S idx, abs)
                                    , Some
                                        ( part
                                        ^
                                        (* If this appears, you're using String internals. Please don't *)
                                        (fun (c, s) -> String.make 1 c ^ s)
                                          (chr, "") ) )
                                  , parts0 )
                              else
                                ( ( (S idx, abs)
                                  , Some
                                      ( part
                                      ^
                                      (* If this appears, you're using String internals. Please don't *)
                                      (fun (c, s) -> String.make 1 c ^ s)
                                        (chr, "") ) )
                                , parts0 )
                            else
                              ( ( (S idx, abs)
                                , Some
                                    ( part
                                    ^
                                    (* If this appears, you're using String internals. Please don't *)
                                    (fun (c, s) -> String.make 1 c ^ s) (chr, "")
                                    ) )
                              , parts0 )
                          else
                            ( ( (S idx, abs)
                              , Some
                                  ( part
                                  ^
                                  (* If this appears, you're using String internals. Please don't *)
                                  (fun (c, s) -> String.make 1 c ^ s) (chr, "")
                                  ) )
                            , parts0 )
                        else
                          ( ( (S idx, abs)
                            , Some
                                ( part
                                ^
                                (* If this appears, you're using String internals. Please don't *)
                                (fun (c, s) -> String.make 1 c ^ s) (chr, "") )
                            )
                          , parts0 ) )
                      chr
                | None ->
                    (* If this appears, you're using Ascii internals. Please don't *)
                    (fun f c ->
                      let n = Char.code c in
                      let h i = n land (1 lsl i) <> 0 in
                      f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7) )
                      (fun b b0 b1 b2 b3 b4 b5 b6 ->
                        if b then
                          if b0 then
                            if b1 then
                              if b2 then
                                if b3 then
                                  ( ( (S idx, abs)
                                    , Some
                                        ((* If this appears, you're using String internals. Please don't *)
                                         (fun (c, s) -> String.make 1 c ^ s )
                                           (chr, "") ) )
                                  , parts0 )
                                else if b4 then
                                  if b5 then
                                    ( ( (S idx, abs)
                                      , Some
                                          ((* If this appears, you're using String internals. Please don't *)
                                           (fun (c, s) -> String.make 1 c ^ s )
                                             (chr, "") ) )
                                    , parts0 )
                                  else if b6 then
                                    ( ( (S idx, abs)
                                      , Some
                                          ((* If this appears, you're using String internals. Please don't *)
                                           (fun (c, s) -> String.make 1 c ^ s )
                                             (chr, "") ) )
                                    , parts0 )
                                  else
                                    (((S idx, abs), None), parts0)
                                else
                                  ( ( (S idx, abs)
                                    , Some
                                        ((* If this appears, you're using String internals. Please don't *)
                                         (fun (c, s) -> String.make 1 c ^ s )
                                           (chr, "") ) )
                                  , parts0 )
                              else
                                ( ( (S idx, abs)
                                  , Some
                                      ((* If this appears, you're using String internals. Please don't *)
                                       (fun (c, s) -> String.make 1 c ^ s )
                                         (chr, "") ) )
                                , parts0 )
                            else
                              ( ( (S idx, abs)
                                , Some
                                    ((* If this appears, you're using String internals. Please don't *)
                                     (fun (c, s) -> String.make 1 c ^ s )
                                       (chr, "") ) )
                              , parts0 )
                          else
                            ( ( (S idx, abs)
                              , Some
                                  ((* If this appears, you're using String internals. Please don't *)
                                   (fun (c, s) -> String.make 1 c ^ s )
                                     (chr, "") ) )
                            , parts0 )
                        else
                          ( ( (S idx, abs)
                            , Some
                                ((* If this appears, you're using String internals. Please don't *)
                                 (fun (c, s) -> String.make 1 c ^ s )
                                   (chr, "") ) )
                          , parts0 ) )
                      chr
              else
                match y1 with
                | Some part ->
                    (* If this appears, you're using Ascii internals. Please don't *)
                    (fun f c ->
                      let n = Char.code c in
                      let h i = n land (1 lsl i) <> 0 in
                      f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7) )
                      (fun b b0 b1 b2 b3 b4 b5 b6 ->
                        if b then
                          if b0 then
                            if b1 then
                              if b2 then
                                if b3 then
                                  ( ( (S idx, abs)
                                    , Some
                                        ( part
                                        ^
                                        (* If this appears, you're using String internals. Please don't *)
                                        (fun (c, s) -> String.make 1 c ^ s)
                                          (chr, "") ) )
                                  , parts0 )
                                else if b4 then
                                  if b5 then
                                    ( ( (S idx, abs)
                                      , Some
                                          ( part
                                          ^
                                          (* If this appears, you're using String internals. Please don't *)
                                          (fun (c, s) -> String.make 1 c ^ s)
                                            (chr, "") ) )
                                    , parts0 )
                                  else if b6 then
                                    ( ( (S idx, abs)
                                      , Some
                                          ( part
                                          ^
                                          (* If this appears, you're using String internals. Please don't *)
                                          (fun (c, s) -> String.make 1 c ^ s)
                                            (chr, "") ) )
                                    , parts0 )
                                  else
                                    ( ((S idx, abs), None)
                                    , app parts0 (part :: []) )
                                else
                                  ( ( (S idx, abs)
                                    , Some
                                        ( part
                                        ^
                                        (* If this appears, you're using String internals. Please don't *)
                                        (fun (c, s) -> String.make 1 c ^ s)
                                          (chr, "") ) )
                                  , parts0 )
                              else
                                ( ( (S idx, abs)
                                  , Some
                                      ( part
                                      ^
                                      (* If this appears, you're using String internals. Please don't *)
                                      (fun (c, s) -> String.make 1 c ^ s)
                                        (chr, "") ) )
                                , parts0 )
                            else
                              ( ( (S idx, abs)
                                , Some
                                    ( part
                                    ^
                                    (* If this appears, you're using String internals. Please don't *)
                                    (fun (c, s) -> String.make 1 c ^ s) (chr, "")
                                    ) )
                              , parts0 )
                          else
                            ( ( (S idx, abs)
                              , Some
                                  ( part
                                  ^
                                  (* If this appears, you're using String internals. Please don't *)
                                  (fun (c, s) -> String.make 1 c ^ s) (chr, "")
                                  ) )
                            , parts0 )
                        else
                          ( ( (S idx, abs)
                            , Some
                                ( part
                                ^
                                (* If this appears, you're using String internals. Please don't *)
                                (fun (c, s) -> String.make 1 c ^ s) (chr, "") )
                            )
                          , parts0 ) )
                      chr
                | None -> (
                  match parts0 with
                  | [] ->
                      (* If this appears, you're using Ascii internals. Please don't *)
                      (fun f c ->
                        let n = Char.code c in
                        let h i = n land (1 lsl i) <> 0 in
                        f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7) )
                        (fun b b0 b1 b2 b3 b4 b5 b6 ->
                          if b then
                            if b0 then
                              if b1 then
                                if b2 then
                                  if b3 then
                                    ( ( (S idx, abs)
                                      , Some
                                          ((* If this appears, you're using String internals. Please don't *)
                                           (fun (c, s) -> String.make 1 c ^ s )
                                             (chr, "") ) )
                                    , parts0 )
                                  else if b4 then
                                    if b5 then
                                      ( ( (S idx, abs)
                                        , Some
                                            ((* If this appears, you're using String internals. Please don't *)
                                             (fun (c, s) -> String.make 1 c ^ s )
                                               (chr, "") ) )
                                      , parts0 )
                                    else if b6 then
                                      ( ( (S idx, abs)
                                        , Some
                                            ((* If this appears, you're using String internals. Please don't *)
                                             (fun (c, s) -> String.make 1 c ^ s )
                                               (chr, "") ) )
                                      , parts0 )
                                    else
                                      (((S O, true), None), [])
                                  else
                                    ( ( (S idx, abs)
                                      , Some
                                          ((* If this appears, you're using String internals. Please don't *)
                                           (fun (c, s) -> String.make 1 c ^ s )
                                             (chr, "") ) )
                                    , parts0 )
                                else
                                  ( ( (S idx, abs)
                                    , Some
                                        ((* If this appears, you're using String internals. Please don't *)
                                         (fun (c, s) -> String.make 1 c ^ s )
                                           (chr, "") ) )
                                  , parts0 )
                              else
                                ( ( (S idx, abs)
                                  , Some
                                      ((* If this appears, you're using String internals. Please don't *)
                                       (fun (c, s) -> String.make 1 c ^ s )
                                         (chr, "") ) )
                                , parts0 )
                            else
                              ( ( (S idx, abs)
                                , Some
                                    ((* If this appears, you're using String internals. Please don't *)
                                     (fun (c, s) -> String.make 1 c ^ s )
                                       (chr, "") ) )
                              , parts0 )
                          else
                            ( ( (S idx, abs)
                              , Some
                                  ((* If this appears, you're using String internals. Please don't *)
                                   (fun (c, s) -> String.make 1 c ^ s )
                                     (chr, "") ) )
                            , parts0 ) )
                        chr
                  | _ :: _ ->
                      (* If this appears, you're using Ascii internals. Please don't *)
                      (fun f c ->
                        let n = Char.code c in
                        let h i = n land (1 lsl i) <> 0 in
                        f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7) )
                        (fun b b0 b1 b2 b3 b4 b5 b6 ->
                          if b then
                            if b0 then
                              if b1 then
                                if b2 then
                                  if b3 then
                                    ( ( (S idx, abs)
                                      , Some
                                          ((* If this appears, you're using String internals. Please don't *)
                                           (fun (c, s) -> String.make 1 c ^ s )
                                             (chr, "") ) )
                                    , parts0 )
                                  else if b4 then
                                    if b5 then
                                      ( ( (S idx, abs)
                                        , Some
                                            ((* If this appears, you're using String internals. Please don't *)
                                             (fun (c, s) -> String.make 1 c ^ s )
                                               (chr, "") ) )
                                      , parts0 )
                                    else if b6 then
                                      ( ( (S idx, abs)
                                        , Some
                                            ((* If this appears, you're using String internals. Please don't *)
                                             (fun (c, s) -> String.make 1 c ^ s )
                                               (chr, "") ) )
                                      , parts0 )
                                    else
                                      (((S idx, abs), None), parts0)
                                  else
                                    ( ( (S idx, abs)
                                      , Some
                                          ((* If this appears, you're using String internals. Please don't *)
                                           (fun (c, s) -> String.make 1 c ^ s )
                                             (chr, "") ) )
                                    , parts0 )
                                else
                                  ( ( (S idx, abs)
                                    , Some
                                        ((* If this appears, you're using String internals. Please don't *)
                                         (fun (c, s) -> String.make 1 c ^ s )
                                           (chr, "") ) )
                                  , parts0 )
                              else
                                ( ( (S idx, abs)
                                  , Some
                                      ((* If this appears, you're using String internals. Please don't *)
                                       (fun (c, s) -> String.make 1 c ^ s )
                                         (chr, "") ) )
                                , parts0 )
                            else
                              ( ( (S idx, abs)
                                , Some
                                    ((* If this appears, you're using String internals. Please don't *)
                                     (fun (c, s) -> String.make 1 c ^ s )
                                       (chr, "") ) )
                              , parts0 )
                          else
                            ( ( (S idx, abs)
                              , Some
                                  ((* If this appears, you're using String internals. Please don't *)
                                   (fun (c, s) -> String.make 1 c ^ s )
                                     (chr, "") ) )
                            , parts0 ) )
                        chr ) )
          | S _ -> (
            match y1 with
            | Some part ->
                (* If this appears, you're using Ascii internals. Please don't *)
                (fun f c ->
                  let n = Char.code c in
                  let h i = n land (1 lsl i) <> 0 in
                  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7) )
                  (fun b b0 b1 b2 b3 b4 b5 b6 ->
                    if b then
                      if b0 then
                        if b1 then
                          if b2 then
                            if b3 then
                              ( ( (S idx, abs)
                                , Some
                                    ( part
                                    ^
                                    (* If this appears, you're using String internals. Please don't *)
                                    (fun (c, s) -> String.make 1 c ^ s) (chr, "")
                                    ) )
                              , parts0 )
                            else if b4 then
                              if b5 then
                                ( ( (S idx, abs)
                                  , Some
                                      ( part
                                      ^
                                      (* If this appears, you're using String internals. Please don't *)
                                      (fun (c, s) -> String.make 1 c ^ s)
                                        (chr, "") ) )
                                , parts0 )
                              else if b6 then
                                ( ( (S idx, abs)
                                  , Some
                                      ( part
                                      ^
                                      (* If this appears, you're using String internals. Please don't *)
                                      (fun (c, s) -> String.make 1 c ^ s)
                                        (chr, "") ) )
                                , parts0 )
                              else
                                (((S idx, abs), None), app parts0 (part :: []))
                            else
                              ( ( (S idx, abs)
                                , Some
                                    ( part
                                    ^
                                    (* If this appears, you're using String internals. Please don't *)
                                    (fun (c, s) -> String.make 1 c ^ s) (chr, "")
                                    ) )
                              , parts0 )
                          else
                            ( ( (S idx, abs)
                              , Some
                                  ( part
                                  ^
                                  (* If this appears, you're using String internals. Please don't *)
                                  (fun (c, s) -> String.make 1 c ^ s) (chr, "")
                                  ) )
                            , parts0 )
                        else
                          ( ( (S idx, abs)
                            , Some
                                ( part
                                ^
                                (* If this appears, you're using String internals. Please don't *)
                                (fun (c, s) -> String.make 1 c ^ s) (chr, "") )
                            )
                          , parts0 )
                      else
                        ( ( (S idx, abs)
                          , Some
                              ( part
                              ^
                              (* If this appears, you're using String internals. Please don't *)
                              (fun (c, s) -> String.make 1 c ^ s) (chr, "") ) )
                        , parts0 )
                    else
                      ( ( (S idx, abs)
                        , Some
                            ( part
                            ^
                            (* If this appears, you're using String internals. Please don't *)
                            (fun (c, s) -> String.make 1 c ^ s) (chr, "") ) )
                      , parts0 ) )
                  chr
            | None ->
                (* If this appears, you're using Ascii internals. Please don't *)
                (fun f c ->
                  let n = Char.code c in
                  let h i = n land (1 lsl i) <> 0 in
                  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7) )
                  (fun b b0 b1 b2 b3 b4 b5 b6 ->
                    if b then
                      if b0 then
                        if b1 then
                          if b2 then
                            if b3 then
                              ( ( (S idx, abs)
                                , Some
                                    ((* If this appears, you're using String internals. Please don't *)
                                     (fun (c, s) -> String.make 1 c ^ s )
                                       (chr, "") ) )
                              , parts0 )
                            else if b4 then
                              if b5 then
                                ( ( (S idx, abs)
                                  , Some
                                      ((* If this appears, you're using String internals. Please don't *)
                                       (fun (c, s) -> String.make 1 c ^ s )
                                         (chr, "") ) )
                                , parts0 )
                              else if b6 then
                                ( ( (S idx, abs)
                                  , Some
                                      ((* If this appears, you're using String internals. Please don't *)
                                       (fun (c, s) -> String.make 1 c ^ s )
                                         (chr, "") ) )
                                , parts0 )
                              else
                                (((S idx, abs), None), parts0)
                            else
                              ( ( (S idx, abs)
                                , Some
                                    ((* If this appears, you're using String internals. Please don't *)
                                     (fun (c, s) -> String.make 1 c ^ s )
                                       (chr, "") ) )
                              , parts0 )
                          else
                            ( ( (S idx, abs)
                              , Some
                                  ((* If this appears, you're using String internals. Please don't *)
                                   (fun (c, s) -> String.make 1 c ^ s )
                                     (chr, "") ) )
                            , parts0 )
                        else
                          ( ( (S idx, abs)
                            , Some
                                ((* If this appears, you're using String internals. Please don't *)
                                 (fun (c, s) -> String.make 1 c ^ s )
                                   (chr, "") ) )
                          , parts0 )
                      else
                        ( ( (S idx, abs)
                          , Some
                              ((* If this appears, you're using String internals. Please don't *)
                               (fun (c, s) -> String.make 1 c ^ s )
                                 (chr, "") ) )
                        , parts0 )
                    else
                      ( ( (S idx, abs)
                        , Some
                            ((* If this appears, you're using String internals. Please don't *)
                             (fun (c, s) -> String.make 1 c ^ s )
                               (chr, "") ) )
                      , parts0 ) )
                  chr ) )
        ((fun s -> List.init (String.length s) (fun i -> s.[i])) s)
        (((O, false), None), [])
    in
    let p0, part = p in
    let _, absolute0 = p0 in
    let parts1 =
      match part with Some p1 -> app parts0 (p1 :: []) | None -> parts0
    in
    Ok {absolute= absolute0; parts= parts1}

(** val concat_paths : path -> path -> path Utils.sresult **)

let concat_paths p1 p2 =
  if p2.absolute then
    Error "Can't concatenate an absolute path on the right"
  else
    Ok {absolute= p1.absolute; parts= app p1.parts p2.parts}

type object0 = nat

type ref = nat

type head = nat

type repo = {root: path; objects: object0 list; refs: ref list; heads: head list}

(** val lsl0 : Uint63.t -> Uint63.t -> Uint63.t **)

let lsl0 = Uint63.l_sl

(** val lsr0 : Uint63.t -> Uint63.t -> Uint63.t **)

let lsr0 = Uint63.l_sr

(** val land0 : Uint63.t -> Uint63.t -> Uint63.t **)

let land0 = Uint63.l_and

(** val lor0 : Uint63.t -> Uint63.t -> Uint63.t **)

let lor0 = Uint63.l_or

(** val lxor0 : Uint63.t -> Uint63.t -> Uint63.t **)

let lxor0 = Uint63.l_xor

(** val asr0 : Uint63.t -> Uint63.t -> Uint63.t **)

let asr0 = Uint63.a_sr

(** val add0 : Uint63.t -> Uint63.t -> Uint63.t **)

let add0 = Uint63.add

(** val sub0 : Uint63.t -> Uint63.t -> Uint63.t **)

let sub0 = Uint63.sub

(** val mul0 : Uint63.t -> Uint63.t -> Uint63.t **)

let mul0 = Uint63.mul

(** val divs : Uint63.t -> Uint63.t -> Uint63.t **)

let divs = Uint63.divs

(** val mods : Uint63.t -> Uint63.t -> Uint63.t **)

let mods = Uint63.rems

(** val eqb : Uint63.t -> Uint63.t -> bool **)

let eqb = Uint63.equal

(** val ltb0 : Uint63.t -> Uint63.t -> bool **)

let ltb0 = Uint63.lt

(** val ltsb : Uint63.t -> Uint63.t -> bool **)

let ltsb = Uint63.lts

(** val lesb : Uint63.t -> Uint63.t -> bool **)

let lesb = Uint63.les

(** val size : nat **)

let size =
  S
    (S
       (S
          (S
             (S
                (S
                   (S
                      (S
                         (S
                            (S
                               (S
                                  (S
                                     (S
                                        (S
                                           (S
                                              (S
                                                 (S
                                                    (S
                                                       (S
                                                          (S
                                                             (S
                                                                (S
                                                                   (S
                                                                      (S
                                                                         (S
                                                                            (S
                                                                               (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                               )
                                                                            ) )
                                                                      ) ) ) ) )
                                                       ) ) ) ) ) ) ) ) ) ) ) )
                   ) ) ) ) ) )

(** val is_zero : Uint63.t -> bool **)

let is_zero i = eqb i (Uint63.of_int 0)

(** val is_even : Uint63.t -> bool **)

let is_even i = is_zero (land0 i (Uint63.of_int 1))

(** val to_Z_rec : nat -> Uint63.t -> z **)

let rec to_Z_rec n0 i =
  match n0 with
  | O ->
      Z0
  | S n1 ->
      ( if is_even i then
          Z.double
        else
          Z.succ_double )
        (to_Z_rec n1 (lsr0 i (Uint63.of_int 1)))

(** val to_Z : Uint63.t -> z **)

let to_Z = to_Z_rec size

(** val of_pos_rec : nat -> positive -> Uint63.t **)

let rec of_pos_rec n0 p =
  match n0 with
  | O ->
      Uint63.of_int 0
  | S n1 -> (
    match p with
    | XI p0 ->
        lor0 (lsl0 (of_pos_rec n1 p0) (Uint63.of_int 1)) (Uint63.of_int 1)
    | XO p0 ->
        lsl0 (of_pos_rec n1 p0) (Uint63.of_int 1)
    | XH ->
        Uint63.of_int 1 )

(** val of_pos : positive -> Uint63.t **)

let of_pos = of_pos_rec size

(** val of_Z : z -> Uint63.t **)

let of_Z = function
  | Z0 ->
      Uint63.of_int 0
  | Zpos p ->
      of_pos p
  | Zneg p ->
      sub0 (Uint63.of_int 0) (of_pos p)

(** val min_int : Uint63.t **)

let min_int = Uint63.of_int (-4611686018427387904)

(** val to_Z0 : Uint63.t -> z **)

let to_Z0 i =
  if ltb0 i min_int then
    to_Z i
  else
    Z.opp (to_Z (sub0 (Uint63.of_int 0) i))

(** val max_int : Uint63.t **)

let max_int = Uint63.of_int 4611686018427387903

(** val open_out : string -> out_channel Utils.sresult **)

let open_out = fun s -> Ok (open_out s)

type file_perm = Uint63.t

(** val fp755 : file_perm **)

let fp755 = Uint63.of_int 493

(** val lnot : Uint63.t -> Uint63.t **)

let lnot x = sub0 max_int x

(** val left_rotate : Uint63.t -> Uint63.t -> Uint63.t **)

let left_rotate n0 b =
  land0
    (lor0 (lsl0 n0 b) (asr0 n0 (sub0 (Uint63.of_int 32) b)))
    (Uint63.of_int 4294967295)

(** val h0 : Uint63.t **)

let h0 = Uint63.of_int 1732584193

(** val h1 : Uint63.t **)

let h1 = Uint63.of_int 4023233417

(** val h2 : Uint63.t **)

let h2 = Uint63.of_int 2562383102

(** val h3 : Uint63.t **)

let h3 = Uint63.of_int 271733878

(** val h4 : Uint63.t **)

let h4 = Uint63.of_int 3285377520

(** val k0 : Uint63.t **)

let k0 = Uint63.of_int 1518500249

(** val k1 : Uint63.t **)

let k1 = Uint63.of_int 1859775393

(** val k2 : Uint63.t **)

let k2 = Uint63.of_int 2400959708

(** val k3 : Uint63.t **)

let k3 = Uint63.of_int 3395469782

(** val append_char_n_times : string -> char -> Uint63.t -> string **)

let append_char_n_times s a n0 =
  let rec hrec s0 a0 n1 _ =
    ( if lesb n1 (Uint63.of_int 0) then
        fun _ ->
      s0
      else
        fun _ ->
      hrec s0 a0 (sub0 n1 (Uint63.of_int 1)) __
      ^
      (* If this appears, you're using String internals. Please don't *)
      (fun (c, s) -> String.make 1 c ^ s) (a0, "") )
      __
  in
  hrec s a n0 __

(** val byte_of_int_big_endian : Uint63.t -> Uint63.t -> char **)

let byte_of_int_big_endian n0 i =
  let shift0 = mul0 (Uint63.of_int 8) (sub0 (Uint63.of_int 7) i) in
  let byte = land0 (asr0 n0 shift0) (Uint63.of_int 255) in
  ascii_of_nat (Coq_Z.to_nat (to_Z0 byte))

(** val bytes_of_int_big_endian : Uint63.t -> nat -> string **)

let rec bytes_of_int_big_endian n0 = function
  | O ->
      ""
  | S i' ->
      bytes_of_int_big_endian n0 i'
      ^
      (* If this appears, you're using String internals. Please don't *)
      (fun (c, s) -> String.make 1 c ^ s)
        (byte_of_int_big_endian n0 (of_Z (Coq_Z.of_nat i')), "")

(** val bytes_to_int : char -> char -> char -> char -> Uint63.t **)

let bytes_to_int b0 b1 b2 b3 =
  let n0 = of_Z (Coq_Z.of_nat (nat_of_ascii b0)) in
  let n1 = of_Z (Coq_Z.of_nat (nat_of_ascii b1)) in
  let n2 = of_Z (Coq_Z.of_nat (nat_of_ascii b2)) in
  let n3 = of_Z (Coq_Z.of_nat (nat_of_ascii b3)) in
  land0
    (lor0
       (lor0
          (lor0 (lsl0 n0 (Uint63.of_int 24)) (lsl0 n1 (Uint63.of_int 16)))
          (lsl0 n2 (Uint63.of_int 8)) )
       n3 )
    (Uint63.of_int 4294967295)

(** val get_chunk : string -> Uint63.t list -> nat -> Uint63.t list **)

let rec get_chunk s chunk = function
  | O ->
      rev chunk
  | S count' ->
      (* If this appears, you're using String internals. Please don't *)
      (fun f0 f1 s ->
        let l = String.length s in
        if l = 0 then
          f0 ()
        else
          f1 (String.get s 0) (String.sub s 1 (l - 1)) )
        (fun _ -> rev chunk)
        (fun b0 s0 ->
          (* If this appears, you're using String internals. Please don't *)
          (fun f0 f1 s ->
            let l = String.length s in
            if l = 0 then
              f0 ()
            else
              f1 (String.get s 0) (String.sub s 1 (l - 1)) )
            (fun _ -> rev chunk)
            (fun b1 s1 ->
              (* If this appears, you're using String internals. Please don't *)
              (fun f0 f1 s ->
                let l = String.length s in
                if l = 0 then
                  f0 ()
                else
                  f1 (String.get s 0) (String.sub s 1 (l - 1)) )
                (fun _ -> rev chunk)
                (fun b2 s2 ->
                  (* If this appears, you're using String internals. Please don't *)
                  (fun f0 f1 s ->
                    let l = String.length s in
                    if l = 0 then
                      f0 ()
                    else
                      f1 (String.get s 0) (String.sub s 1 (l - 1)) )
                    (fun _ -> rev chunk)
                    (fun b3 rest ->
                      get_chunk rest (bytes_to_int b0 b1 b2 b3 :: chunk) count' )
                    s2 )
                s1 )
            s0 )
        s

type hash = (((Uint63.t * Uint63.t) * Uint63.t) * Uint63.t) * Uint63.t

(** val process_chunk :
    Uint63.t list -> Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t
    -> hash **)

let process_chunk chunk h5 h6 h7 h8 h9 =
  let extend_words =
    let rec extend_words w i =
      match i with
      | O ->
          w
      | S i' ->
          let w_new =
            lxor0
              (lxor0
                 (lxor0
                    (nth
                       (sub
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (S
                                                  (S
                                                     (S
                                                        (S
                                                           (S
                                                              (S
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                             )
                                                                          ) ) )
                                                                 ) ) ) ) ) ) )
                                            ) ) ) ) ) ) )
                          i )
                       w (Uint63.of_int 0) )
                    (nth
                       (add
                          (sub
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (S
                                                  (S
                                                     (S
                                                        (S
                                                           (S
                                                              (S
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                             )
                                                                          ) ) )
                                                                 ) ) ) ) ) ) )
                                            ) ) ) ) ) )
                             i )
                          (S (S O)) )
                       w (Uint63.of_int 0) ) )
                 (nth
                    (add
                       (sub
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (S
                                                  (S
                                                     (S
                                                        (S
                                                           (S
                                                              (S
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                             )
                                                                          ) ) )
                                                                 ) ) ) ) ) ) )
                                            ) ) ) ) ) ) )
                          i )
                       (S (S (S (S (S (S (S (S O)))))))) )
                    w (Uint63.of_int 0) ) )
              (nth
                 (add
                    (sub
                       (S
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (S
                                                  (S
                                                     (S
                                                        (S
                                                           (S
                                                              (S
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                             )
                                                                          ) ) )
                                                                 ) ) ) ) ) ) )
                                            ) ) ) ) ) ) ) )
                       i )
                    (S (S (S (S (S (S (S (S (S (S (S (S (S O))))))))))))) )
                 w (Uint63.of_int 0) )
          in
          extend_words (app w (left_rotate w_new (Uint63.of_int 1) :: [])) i'
    in
    extend_words
  in
  let w =
    extend_words chunk (Coq_Z.to_nat (Zpos (XO (XO (XO (XO (XO (XO XH))))))))
  in
  let main_loop =
    let rec main_loop a b c d e i =
      match i with
      | O ->
          ((((a, b), c), d), e)
      | S i' ->
          let i_val = sub0 (Uint63.of_int 80) (of_Z (Coq_Z.of_nat i)) in
          let f, k =
            if ltsb i_val (Uint63.of_int 20) then
              ( lor0 (land0 b c)
                  (land0 (land0 (lnot b) (Uint63.of_int 4294967295)) d)
              , k0 )
            else if ltsb i_val (Uint63.of_int 40) then
              (lxor0 (lxor0 b c) d, k1)
            else if ltsb i_val (Uint63.of_int 60) then
              (lor0 (lor0 (land0 b c) (land0 b d)) (land0 c d), k2)
            else
              (lxor0 (lxor0 b c) d, k3)
          in
          let temp =
            land0
              (add0
                 (add0 (add0 (add0 (left_rotate a (Uint63.of_int 5)) f) e) k)
                 (nth (Coq_Z.to_nat (to_Z0 i_val)) w (Uint63.of_int 0)) )
              (Uint63.of_int 4294967295)
          in
          main_loop temp a (left_rotate b (Uint63.of_int 30)) c d i'
    in
    main_loop
  in
  let p, e =
    main_loop h5 h6 h7 h8 h9
      (S
         (S
            (S
               (S
                  (S
                     (S
                        (S
                           (S
                              (S
                                 (S
                                    (S
                                       (S
                                          (S
                                             (S
                                                (S
                                                   (S
                                                      (S
                                                         (S
                                                            (S
                                                               (S
                                                                  (S
                                                                     (S
                                                                        (S
                                                                           (S
                                                                              (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                              )
                                                                           ) )
                                                                     ) ) ) ) )
                                                      ) ) ) ) ) ) ) ) ) ) ) ) )
               ) ) ) )
  in
  let p0, d = p in
  let p1, c = p0 in
  let a, b = p1 in
  ( ( ( ( land0 (add0 h5 a) (Uint63.of_int 4294967295)
        , land0 (add0 h6 b) (Uint63.of_int 4294967295) )
      , land0 (add0 h7 c) (Uint63.of_int 4294967295) )
    , land0 (add0 h8 d) (Uint63.of_int 4294967295) )
  , land0 (add0 h9 e) (Uint63.of_int 4294967295) )

(** val process_message :
    string -> Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t -> nat
    -> hash **)

let rec process_message s h5 h6 h7 h8 h9 = function
  | O ->
      ((((h5, h6), h7), h8), h9)
  | S fuel' ->
      if
        Nat.ltb (length s)
          (S
             (S
                (S
                   (S
                      (S
                         (S
                            (S
                               (S
                                  (S
                                     (S
                                        (S
                                           (S
                                              (S
                                                 (S
                                                    (S
                                                       (S
                                                          (S
                                                             (S
                                                                (S
                                                                   (S
                                                                      (S
                                                                         (S
                                                                            (S
                                                                               (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                               )
                                                                            ) )
                                                                      ) ) ) ) )
                                                       ) ) ) ) ) ) ) ) ) ) ) )
                   ) ) ) )
      then
        ((((h5, h6), h7), h8), h9)
      else
        let chunk =
          get_chunk s []
            (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S O))))))))))))))))
        in
        let p, h4' = process_chunk chunk h5 h6 h7 h8 h9 in
        let p0, h3' = p in
        let p1, h2' = p0 in
        let h0', h1' = p1 in
        let remaining =
          substring
            (S
               (S
                  (S
                     (S
                        (S
                           (S
                              (S
                                 (S
                                    (S
                                       (S
                                          (S
                                             (S
                                                (S
                                                   (S
                                                      (S
                                                         (S
                                                            (S
                                                               (S
                                                                  (S
                                                                     (S
                                                                        (S
                                                                           (S
                                                                              (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                              )
                                                                           ) )
                                                                     ) ) ) ) )
                                                      ) ) ) ) ) ) ) ) ) ) ) ) )
               ) )
            (sub (length s)
               (S
                  (S
                     (S
                        (S
                           (S
                              (S
                                 (S
                                    (S
                                       (S
                                          (S
                                             (S
                                                (S
                                                   (S
                                                      (S
                                                         (S
                                                            (S
                                                               (S
                                                                  (S
                                                                     (S
                                                                        (S
                                                                           (S
                                                                              (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                              )
                                                                           ) )
                                                                     ) ) ) ) )
                                                      ) ) ) ) ) ) ) ) ) ) ) ) )
               ) )
            s
        in
        process_message remaining h0' h1' h2' h3' h4' fuel'

(** val sha1 : string -> hash **)

let sha1 message =
  let byte_len = of_Z (Coq_Z.of_nat (length message)) in
  let bit_len = mul0 (Uint63.of_int 8) byte_len in
  let message0 =
    message
    ^
    (* If this appears, you're using String internals. Please don't *)
    (fun (c, s) -> String.make 1 c ^ s)
      ( ascii_of_nat
          (S
             (S
                (S
                   (S
                      (S
                         (S
                            (S
                               (S
                                  (S
                                     (S
                                        (S
                                           (S
                                              (S
                                                 (S
                                                    (S
                                                       (S
                                                          (S
                                                             (S
                                                                (S
                                                                   (S
                                                                      (S
                                                                         (S
                                                                            (S
                                                                               (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                               )
                                                                            ) )
                                                                      ) ) ) ) )
                                                       ) ) ) ) ) ) ) ) ) ) ) )
                   ) ) ) )
      , "" )
  in
  let message1 =
    append_char_n_times message0 (ascii_of_nat O)
      (mods
         (sub0 (Uint63.of_int 56)
            (mods (add0 byte_len (Uint63.of_int 1)) (Uint63.of_int 64)) )
         (Uint63.of_int 64) )
  in
  let message2 =
    message1 ^ bytes_of_int_big_endian bit_len (S (S (S (S (S (S (S (S O))))))))
  in
  let num_chunks =
    Nat.div (length message2)
      (S
         (S
            (S
               (S
                  (S
                     (S
                        (S
                           (S
                              (S
                                 (S
                                    (S
                                       (S
                                          (S
                                             (S
                                                (S
                                                   (S
                                                      (S
                                                         (S
                                                            (S
                                                               (S
                                                                  (S
                                                                     (S
                                                                        (S
                                                                           (S
                                                                              (S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                (
                                                                                S
                                                                                O
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                              )
                                                                           ) )
                                                                     ) ) ) ) )
                                                      ) ) ) ) ) ) ) ) ) ) ) ) )
               ) ) ) )
  in
  process_message message2 h0 h1 h2 h3 h4 num_chunks

type object_type = Blob | Commit | Tree

(** val string_of_object_type : object_type -> string **)

let string_of_object_type = function
  | Blob ->
      "blob"
  | Commit ->
      "commit"
  | Tree ->
      "tree"

(** val string_of_nat_aux : Uint63.t -> string -> string **)

let string_of_nat_aux n0 acc =
  let rec hrec n1 acc0 _ =
    ( if lesb n1 (Uint63.of_int 0) then
        fun _ ->
      acc0
      else
        fun _ ->
      hrec
        (divs n1 (Uint63.of_int 10))
        ((* If this appears, you're using String internals. Please don't *)
         (fun (c, s) -> String.make 1 c ^ s )
           ( ascii_of_nat
               (Coq_Z.to_nat
                  (to_Z0
                     (add0 (Uint63.of_int 48) (mods n1 (Uint63.of_int 10))) ) )
           , acc0 ) )
        __ )
      __
  in
  hrec n0 acc __

(** val string_of_nat : nat -> string **)

let string_of_nat n0 =
  match n0 with
  | O ->
      "0"
  | S _ ->
      string_of_nat_aux (of_Z (Coq_Z.of_nat n0)) ""

(** val hash_object :
    repo -> string -> object_type -> bool -> hash Utils.sresult **)

let hash_object repo0 data t write =
  let type_str = string_of_object_type t in
  let len_str = string_of_nat (length data) in
  let header = type_str ^ " " ^ len_str in
  let full_data =
    header
    ^
    (* If this appears, you're using String internals. Please don't *)
    (fun (c, s) -> String.make 1 c ^ s) (ascii_of_nat O, data)
  in
  let p, sha4 = sha1 full_data in
  let p0, sha3 = p in
  let p1, sha2 = p0 in
  let sha0, sha5 = p1 in
  match
    if write then
      match
        match parse_path (render repo0.root) with
        | Ok x_ -> (
          match
            match parse_path ".git" with
            | Ok x_0 -> (
              match parse_path "objects" with
              | Ok y_ ->
                  concat_paths x_0 y_
              | Error err ->
                  Error err )
            | Error err ->
                Error err
          with
          | Ok y_ ->
              concat_paths x_ y_
          | Error err ->
              Error err )
        | Error err ->
            Error err
      with
      | Ok path0 -> (
          if Sys.file_exists (render path0) then
            Error "object path collision"
          else
            let () = Utils.makedirs (Filename.dirname (render path0)) fp755 in
            match open_out (render path0) with
            | Ok oc ->
                let () = output_string oc (Utils.compress_string full_data) in
                Ok ()
            | Error err ->
                Error err )
      | Error err ->
          Error err
    else
      Ok ()
  with
  | Ok _ ->
      Ok ((((sha0, sha5), sha2), sha3), sha4)
  | Error err ->
      Error err
