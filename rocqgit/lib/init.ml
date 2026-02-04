
let __ = let rec f _ = Obj.repr f in Obj.repr f

type nat =
| O
| S of nat

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let app x =
  let rec app0 l m =
    match l with
    | [] -> m
    | a :: l1 -> a :: (app0 l1 m)
  in app0 x

(** val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let in_dec h a l =
  let rec f = function
  | [] -> false
  | y :: l1 ->
    let s = h y a in if s then true else if f l1 then true else false
  in f l

(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let fold_left f =
  let rec fold_left0 l a0 =
    match l with
    | [] -> a0
    | b :: l0 -> fold_left0 l0 (f a0 b)
  in fold_left0

type 'x result =
| Ok of 'x
| Error of string

type path = { absolute : bool; parts : string list }

(** val render : path -> string **)

let render p =
  (^) (if p.absolute then "/" else "") (String.concat "/" p.parts)

(** val parse_path : string -> path result **)

let parse_path s =
  if in_dec (=) ' '
       ((fun s -> List.init (String.length s) (fun i -> s.[i])) s)
  then Error "No spaces allowed in paths"
  else let (p, parts0) =
         fold_left (fun acc chr ->
           let (y, parts0) = acc in
           let (y0, y1) = y in
           let (idx, abs) = y0 in
           (match idx with
            | O ->
              if abs
              then (match y1 with
                    | Some part ->
                      (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                        (fun b b0 b1 b2 b3 b4 b5 b6 ->
                        if b
                        then if b0
                             then if b1
                                  then if b2
                                       then if b3
                                            then ((((S idx), abs), (Some
                                                   ((^) part
                                                     ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                     (chr, ""))))),
                                                   parts0)
                                            else if b4
                                                 then if b5
                                                      then ((((S idx), abs),
                                                             (Some
                                                             ((^) part
                                                               ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                               (chr, ""))))),
                                                             parts0)
                                                      else if b6
                                                           then ((((S idx),
                                                                  abs), (Some
                                                                  ((^) part
                                                                    ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                                    (chr,
                                                                    ""))))),
                                                                  parts0)
                                                           else ((((S idx),
                                                                  abs),
                                                                  None),
                                                                  (app parts0
                                                                    (part :: [])))
                                                 else ((((S idx), abs), (Some
                                                        ((^) part
                                                          ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                          (chr, ""))))),
                                                        parts0)
                                       else ((((S idx), abs), (Some
                                              ((^) part
                                                ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                (chr, ""))))),
                                              parts0)
                                  else ((((S idx), abs), (Some
                                         ((^) part
                                           ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                           (chr, ""))))),
                                         parts0)
                             else ((((S idx), abs), (Some
                                    ((^) part
                                      ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                      (chr, ""))))),
                                    parts0)
                        else ((((S idx), abs), (Some
                               ((^) part
                                 ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                 (chr, ""))))),
                               parts0))
                        chr
                    | None ->
                      (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                        (fun b b0 b1 b2 b3 b4 b5 b6 ->
                        if b
                        then if b0
                             then if b1
                                  then if b2
                                       then if b3
                                            then ((((S idx), abs), (Some
                                                   ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                   (chr, "")))), parts0)
                                            else if b4
                                                 then if b5
                                                      then ((((S idx), abs),
                                                             (Some
                                                             ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                             (chr, "")))),
                                                             parts0)
                                                      else if b6
                                                           then ((((S idx),
                                                                  abs), (Some
                                                                  ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                                  (chr,
                                                                  "")))),
                                                                  parts0)
                                                           else ((((S idx),
                                                                  abs),
                                                                  None),
                                                                  parts0)
                                                 else ((((S idx), abs), (Some
                                                        ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                        (chr, "")))), parts0)
                                       else ((((S idx), abs), (Some
                                              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                              (chr, "")))), parts0)
                                  else ((((S idx), abs), (Some
                                         ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                         (chr, "")))), parts0)
                             else ((((S idx), abs), (Some
                                    ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                    (chr, "")))), parts0)
                        else ((((S idx), abs), (Some
                               ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                               (chr, "")))), parts0))
                        chr)
              else (match y1 with
                    | Some part ->
                      (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                        (fun b b0 b1 b2 b3 b4 b5 b6 ->
                        if b
                        then if b0
                             then if b1
                                  then if b2
                                       then if b3
                                            then ((((S idx), abs), (Some
                                                   ((^) part
                                                     ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                     (chr, ""))))),
                                                   parts0)
                                            else if b4
                                                 then if b5
                                                      then ((((S idx), abs),
                                                             (Some
                                                             ((^) part
                                                               ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                               (chr, ""))))),
                                                             parts0)
                                                      else if b6
                                                           then ((((S idx),
                                                                  abs), (Some
                                                                  ((^) part
                                                                    ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                                    (chr,
                                                                    ""))))),
                                                                  parts0)
                                                           else ((((S idx),
                                                                  abs),
                                                                  None),
                                                                  (app parts0
                                                                    (part :: [])))
                                                 else ((((S idx), abs), (Some
                                                        ((^) part
                                                          ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                          (chr, ""))))),
                                                        parts0)
                                       else ((((S idx), abs), (Some
                                              ((^) part
                                                ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                (chr, ""))))),
                                              parts0)
                                  else ((((S idx), abs), (Some
                                         ((^) part
                                           ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                           (chr, ""))))),
                                         parts0)
                             else ((((S idx), abs), (Some
                                    ((^) part
                                      ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                      (chr, ""))))),
                                    parts0)
                        else ((((S idx), abs), (Some
                               ((^) part
                                 ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                 (chr, ""))))),
                               parts0))
                        chr
                    | None ->
                      (match parts0 with
                       | [] ->
                         (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                           (fun b b0 b1 b2 b3 b4 b5 b6 ->
                           if b
                           then if b0
                                then if b1
                                     then if b2
                                          then if b3
                                               then ((((S idx), abs), (Some
                                                      ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                      (chr, "")))), parts0)
                                               else if b4
                                                    then if b5
                                                         then ((((S idx),
                                                                abs), (Some
                                                                ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                                (chr, "")))),
                                                                parts0)
                                                         else if b6
                                                              then ((((S
                                                                    idx),
                                                                    abs),
                                                                    (Some
                                                                    ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                                    (chr,
                                                                    "")))),
                                                                    parts0)
                                                              else ((((S O),
                                                                    true),
                                                                    None), [])
                                                    else ((((S idx), abs),
                                                           (Some
                                                           ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                           (chr, "")))),
                                                           parts0)
                                          else ((((S idx), abs), (Some
                                                 ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                 (chr, "")))), parts0)
                                     else ((((S idx), abs), (Some
                                            ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                            (chr, "")))), parts0)
                                else ((((S idx), abs), (Some
                                       ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                       (chr, "")))), parts0)
                           else ((((S idx), abs), (Some
                                  ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                  (chr, "")))), parts0))
                           chr
                       | _ :: _ ->
                         (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                           (fun b b0 b1 b2 b3 b4 b5 b6 ->
                           if b
                           then if b0
                                then if b1
                                     then if b2
                                          then if b3
                                               then ((((S idx), abs), (Some
                                                      ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                      (chr, "")))), parts0)
                                               else if b4
                                                    then if b5
                                                         then ((((S idx),
                                                                abs), (Some
                                                                ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                                (chr, "")))),
                                                                parts0)
                                                         else if b6
                                                              then ((((S
                                                                    idx),
                                                                    abs),
                                                                    (Some
                                                                    ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                                    (chr,
                                                                    "")))),
                                                                    parts0)
                                                              else ((((S
                                                                    idx),
                                                                    abs),
                                                                    None),
                                                                    parts0)
                                                    else ((((S idx), abs),
                                                           (Some
                                                           ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                           (chr, "")))),
                                                           parts0)
                                          else ((((S idx), abs), (Some
                                                 ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                 (chr, "")))), parts0)
                                     else ((((S idx), abs), (Some
                                            ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                            (chr, "")))), parts0)
                                else ((((S idx), abs), (Some
                                       ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                       (chr, "")))), parts0)
                           else ((((S idx), abs), (Some
                                  ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                  (chr, "")))), parts0))
                           chr))
            | S _ ->
              (match y1 with
               | Some part ->
                 (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                   (fun b b0 b1 b2 b3 b4 b5 b6 ->
                   if b
                   then if b0
                        then if b1
                             then if b2
                                  then if b3
                                       then ((((S idx), abs), (Some
                                              ((^) part
                                                ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                (chr, ""))))),
                                              parts0)
                                       else if b4
                                            then if b5
                                                 then ((((S idx), abs), (Some
                                                        ((^) part
                                                          ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                          (chr, ""))))),
                                                        parts0)
                                                 else if b6
                                                      then ((((S idx), abs),
                                                             (Some
                                                             ((^) part
                                                               ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                               (chr, ""))))),
                                                             parts0)
                                                      else ((((S idx), abs),
                                                             None),
                                                             (app parts0
                                                               (part :: [])))
                                            else ((((S idx), abs), (Some
                                                   ((^) part
                                                     ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                     (chr, ""))))),
                                                   parts0)
                                  else ((((S idx), abs), (Some
                                         ((^) part
                                           ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                           (chr, ""))))),
                                         parts0)
                             else ((((S idx), abs), (Some
                                    ((^) part
                                      ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                      (chr, ""))))),
                                    parts0)
                        else ((((S idx), abs), (Some
                               ((^) part
                                 ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                 (chr, ""))))),
                               parts0)
                   else ((((S idx), abs), (Some
                          ((^) part
                            ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                            (chr, ""))))),
                          parts0))
                   chr
               | None ->
                 (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                   (fun b b0 b1 b2 b3 b4 b5 b6 ->
                   if b
                   then if b0
                        then if b1
                             then if b2
                                  then if b3
                                       then ((((S idx), abs), (Some
                                              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                              (chr, "")))), parts0)
                                       else if b4
                                            then if b5
                                                 then ((((S idx), abs), (Some
                                                        ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                        (chr, "")))), parts0)
                                                 else if b6
                                                      then ((((S idx), abs),
                                                             (Some
                                                             ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                             (chr, "")))),
                                                             parts0)
                                                      else ((((S idx), abs),
                                                             None), parts0)
                                            else ((((S idx), abs), (Some
                                                   ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                                   (chr, "")))), parts0)
                                  else ((((S idx), abs), (Some
                                         ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                         (chr, "")))), parts0)
                             else ((((S idx), abs), (Some
                                    ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                                    (chr, "")))), parts0)
                        else ((((S idx), abs), (Some
                               ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                               (chr, "")))), parts0)
                   else ((((S idx), abs), (Some
                          ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

                          (chr, "")))), parts0))
                   chr)))
           ((fun s -> List.init (String.length s) (fun i -> s.[i])) s) (((O,
           false), None), [])
       in
       let (p0, part) = p in
       let (_, absolute0) = p0 in
       let parts1 =
         match part with
         | Some p1 -> app parts0 (p1 :: [])
         | None -> parts0
       in
       Ok { absolute = absolute0; parts = parts1 }

(** val concat_paths : path -> path -> path result **)

let concat_paths p1 p2 =
  if p2.absolute
  then Error "Can't concatenate an absolute path on the right"
  else Ok { absolute = p1.absolute; parts = (app p1.parts p2.parts) }

type object0 = nat

type ref = nat

type head = nat

type repo = { root : path; objects : object0 list; refs : ref list;
              heads : head list }

(** val fresh_repo : string -> repo result **)

let fresh_repo name =
  match parse_path name with
  | Ok root0 -> Ok { root = root0; objects = []; refs = []; heads = [] }
  | Error err -> Error err

(** val git_dir : repo -> path **)

let git_dir r =
  let r0 =
    match Ok
    r.root with
    | Ok x_ ->
      (match parse_path ".git" with
       | Ok y_ -> concat_paths x_ y_
       | Error err -> Error err)
    | Error err -> Error err
  in
  (match r0 with
   | Ok x -> (fun _ -> x)
   | Error _ -> (fun _ -> assert false (* absurd case *))) __

(** val objects_dir : repo -> path **)

let objects_dir r =
  let r0 =
    match Ok
    (git_dir r) with
    | Ok x_ ->
      (match parse_path "objects" with
       | Ok y_ -> concat_paths x_ y_
       | Error err -> Error err)
    | Error err -> Error err
  in
  (match r0 with
   | Ok x -> (fun _ -> x)
   | Error _ -> (fun _ -> assert false (* absurd case *))) __

(** val refs_dir : repo -> path **)

let refs_dir r =
  let r0 =
    match Ok
    (git_dir r) with
    | Ok x_ ->
      (match parse_path "refs" with
       | Ok y_ -> concat_paths x_ y_
       | Error err -> Error err)
    | Error err -> Error err
  in
  (match r0 with
   | Ok x -> (fun _ -> x)
   | Error _ -> (fun _ -> assert false (* absurd case *))) __

(** val heads_dir : repo -> path **)

let heads_dir r =
  let r0 =
    match Ok
    (refs_dir r) with
    | Ok x_ ->
      (match parse_path "heads" with
       | Ok y_ -> concat_paths x_ y_
       | Error err -> Error err)
    | Error err -> Error err
  in
  (match r0 with
   | Ok x -> (fun _ -> x)
   | Error _ -> (fun _ -> assert false (* absurd case *))) __

(** val hEAD_path : repo -> path **)

let hEAD_path r =
  let r0 =
    match Ok
    (refs_dir r) with
    | Ok x_ ->
      (match parse_path "HEAD" with
       | Ok y_ -> concat_paths x_ y_
       | Error err -> Error err)
    | Error err -> Error err
  in
  (match r0 with
   | Ok x -> (fun _ -> x)
   | Error _ -> (fun _ -> assert false (* absurd case *))) __

(** val open_out : string -> out_channel result **)

let open_out = (fun s -> Ok (open_out s))

type file_perm = Uint63.t

(** val fp755 : file_perm **)

let fp755 =
  (Uint63.of_int (493))



(** val mkdir' : string -> unit **)

let mkdir' s =
  Unix.mkdir s fp755

(** val init : string -> repo result **)

let init repo_name =
  match fresh_repo repo_name with
  | Ok repo0 ->
    let () = mkdir' repo_name in
    let () = mkdir' (render (git_dir repo0)) in
    let () = mkdir' (render (objects_dir repo0)) in
    let () = mkdir' (render (refs_dir repo0)) in
    let () = mkdir' (render (heads_dir repo0)) in
    (match open_out (render (hEAD_path repo0)) with
     | Ok oc -> let () = output_string oc "ref: refs/heads/main" in Ok repo0
     | Error err -> Error err)
  | Error err -> Error err
