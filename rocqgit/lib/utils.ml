let l2s l = String.of_seq (List.to_seq l)

let s2l s = List.of_seq (String.to_seq s)

type 'a sresult = ('a, string) result

let rec makedirs ?(exist_ok = true) path perm =
  if Sys.file_exists path then begin
    if exist_ok && Sys.is_directory path then
      ()
    (* Directory exists, that's fine *)
    else if not exist_ok then
      failwith ("Directory already exists: " ^ path)
    else
      failwith (path ^ " exists but is not a directory")
  end else begin
    (* Create parent first *)
      let parent = Filename.dirname path in
      if parent <> path then makedirs ~exist_ok parent perm ;
      Unix.mkdir path perm
  end

let compress_string ?(level = 6) data =
  let stream = Zlib.deflate_init level true in
  (* true = include zlib header *)
  let input_len = String.length data in
  let output_size = input_len + 256 in
  (* Extra space for compression overhead *)
  let output = Bytes.create output_size in
  let finished, used_in, used_out =
    Zlib.deflate_string stream data 0 input_len output 0 output_size
      Zlib.Z_FINISH
  in
  Zlib.deflate_end stream ;
  Bytes.sub_string output 0 used_out

(* Usage *)
let compressed = compress_string "hello world"
