open Tree_sitter_c
open Tree_sitter_run
open CST
module A = PAst
open Mlsem.Common

let token_to_string (_loc, s) = s

(* Taken from E-Sh4rk/typed-r *)
let line_length = 0x10000

let conv_pos tspos =
  let bol = tspos.Loc.row * line_length in
  {
    Lexing.pos_fname = "";
    pos_lnum = tspos.Loc.row;
    pos_bol = bol;
    pos_cnum = bol + tspos.Loc.column;
  }

let loc_to_pos (loc : Loc.t) : Position.t =
  let start_pos = conv_pos loc.start in
  let end_pos = conv_pos loc.end_ in
  Position.lex_join start_pos end_pos

let locs_to_pos (loc1 : Loc.t) (loc2 : Loc.t) : Position.t =
  let start_pos = conv_pos loc1.start in
  let end_pos = conv_pos loc2.end_ in
  Position.lex_join start_pos end_pos

let string_of_loc_start (loc : Loc.t) =
  Position.string_of_lex_pos (conv_pos loc.start)

let strip_int_suffix s =
  let i = ref (String.length s - 1) in
  while
    !i >= 0
    && match s.[!i] with 'u' | 'U' | 'l' | 'L' -> true | _ -> false
  do
    decr i
  done;
  String.sub s 0 (!i + 1)

let normalize_int_literal s =
  let len = String.length s in
  if len >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X' || s.[1] = 'b' || s.[1] = 'B') then s
  else if len > 1 && s.[0] = '0' then
    let rec all_octal i =
      i >= len || ((s.[i] >= '0' && s.[i] <= '7') && all_octal (i + 1))
    in
    if all_octal 1 then "0o" ^ String.sub s 1 (len - 1) else s
  else s

let parse_c_int_literal s =
  let core = s |> strip_int_suffix |> normalize_int_literal in
  try int_of_string core
  with Failure _ ->
    (* OCaml int is 63-bit, so values like 0xffffffffffffffff overflow
       int_of_string. Reinterpret via Int64 with C's modular semantics
       (the result is unspecified at the C level too, so any 63-bit
       representative is acceptable for typing purposes). *)
    Int64.to_int (Int64.of_string core)

let decode_c_escape_sequence s =
  let fail () = failwith ("Invalid C escape sequence: " ^ s) in
  if String.length s < 2 || s.[0] <> '\\' then fail ()
  else
    let parse_int base digits =
      if String.length digits = 0 then fail () else int_of_string (base ^ digits)
    in
    match s.[1] with
    | 'a' when String.length s = 2 -> 7
    | 'b' when String.length s = 2 -> 8
    | 'f' when String.length s = 2 -> 12
    | 'n' when String.length s = 2 -> 10
    | 'r' when String.length s = 2 -> 13
    | 't' when String.length s = 2 -> 9
    | 'v' when String.length s = 2 -> 11
    | '\\' when String.length s = 2 -> Char.code '\\'
    | '\'' when String.length s = 2 -> Char.code '\''
    | '"' when String.length s = 2 -> Char.code '"'
    | '?' when String.length s = 2 -> Char.code '?'
    | 'x' -> parse_int "0x" (String.sub s 2 (String.length s - 2))
    | 'u' when String.length s = 6 -> parse_int "0x" (String.sub s 2 4)
    | 'U' when String.length s = 10 -> parse_int "0x" (String.sub s 2 8)
    | c2 when '0' <= c2 && c2 <= '7' -> parse_int "0o" (String.sub s 1 (String.length s - 1))
    | c2 when String.length s = 2 -> Char.code c2
    | _ -> fail ()

let decode_c_char_literal_values chars =
  List.map
    (function
      | `Imm_tok_pat_36637e2 (_, s) ->
          assert (String.length s = 1);
          Char.code s.[0]
      | `Esc_seq (_, s) -> decode_c_escape_sequence s)
    chars

let int_of_char_literal (char_lit : char_literal) =
  let _, chars, _ = char_lit in
  let values = decode_c_char_literal_values chars in
  match values with
  | [value] -> value
  | _ -> List.fold_left (fun acc value -> (acc lsl 8) lor (value land 0xFF)) 0 values

let parse_number_literal s =
  (* In a hexadecimal literal, [e] and [E] are digits: only [.] and the binary
     exponent [p] / [P] make it a float ([0x9e3779b9] is an integer). *)
  let has_float_syntax s =
    let hex = String.length s > 1 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X') in
    let rec loop i =
      if i >= String.length s then false
      else
        match s.[i] with
        | '.' | 'p' | 'P' -> true
        | 'e' | 'E' when not hex -> true
        | _ -> loop (i + 1)
    in
    loop 0
  in
  let strip_float_suffix s =
    if String.length s = 0 then s
    else
      match s.[String.length s - 1] with
      | 'f' | 'F' | 'l' | 'L' -> String.sub s 0 (String.length s - 1)
      | _ -> s
  in
  let parse_float s =
    let core = strip_float_suffix s in
    Float.of_string core
  in
  if has_float_syntax s then
    A.CFloat (parse_float s)
  else
    try A.CInt (parse_c_int_literal s)
    with Failure _ -> A.CFloat (parse_float s)

let parse_define_literal s =
  Some (parse_number_literal s)

let eval_char_literal char_lit =
  Some (int_of_char_literal char_lit)


let test_loc start_row start_col end_row end_col =
  {
    Loc.start = { row = start_row; column = start_col };
    end_ = { row = end_row; column = end_col };
  }


let test_token text =
  (test_loc 0 0 0 (String.length text), text)


let test_char_literal values : char_literal =
  (`SQUOT (test_token "'"), values, test_token "'")


let%test "conv_pos computes line-based absolute offsets" =
  let pos = conv_pos { Loc.row = 3; column = 12 } in
  pos.Lexing.pos_lnum = 3
  && pos.pos_bol = 3 * line_length
  && pos.pos_cnum = (3 * line_length) + 12


let%test "loc_to_pos preserves start and end coordinates" =
  let pos = loc_to_pos (test_loc 2 4 2 9) in
  let start_ = Position.start_of_position pos in
  let end_ = Position.end_of_position pos in
  start_.Lexing.pos_lnum = 2
  && (start_.pos_cnum - start_.pos_bol) = 4
  && end_.Lexing.pos_lnum = 2
  && (end_.pos_cnum - end_.pos_bol) = 9


let%test "locs_to_pos spans from first start to second end" =
  let pos = locs_to_pos (test_loc 1 2 1 5) (test_loc 3 7 4 1) in
  let start_ = Position.start_of_position pos in
  let end_ = Position.end_of_position pos in
  start_.Lexing.pos_lnum = 1
  && (start_.pos_cnum - start_.pos_bol) = 2
  && end_.Lexing.pos_lnum = 4
  && (end_.pos_cnum - end_.pos_bol) = 1


let%test "string_of_loc_start depends only on the start position" =
  string_of_loc_start (test_loc 5 8 5 10)
  = string_of_loc_start (test_loc 5 8 9 3)


let%test "strip_int_suffix removes standard integer suffixes" =
  strip_int_suffix "123uLL" = "123"


let%test "normalize_int_literal rewrites octal but preserves non-octal and prefixed literals" =
  normalize_int_literal "077" = "0o77"
  && normalize_int_literal "089" = "089"
  && normalize_int_literal "0xff" = "0xff"
  && normalize_int_literal "0b1010" = "0b1010"


let%test "parse_c_int_literal handles suffixes hexadecimal and octal" =
  parse_c_int_literal "42UL" = 42
  && parse_c_int_literal "0x10" = 16
  && parse_c_int_literal "077" = 63

let%test "parse_c_int_literal handles 64-bit hex literals via Int64" =
  (* Int64.to_int truncates modulo 2^63 on 64-bit OCaml: the high bit is
     dropped. Exact value matters less than not raising. *)
  parse_c_int_literal "0xffffffffffffffff" = -1
  && parse_c_int_literal "0xffffffffffffffffu" = -1
  && parse_c_int_literal "0x8000000000000000" = 0


let%test "decode_c_escape_sequence handles simple hexadecimal unicode and octal escapes" =
  decode_c_escape_sequence "\\n" = 10
  && decode_c_escape_sequence "\\x41" = 65
  && decode_c_escape_sequence "\\101" = 65
  && decode_c_escape_sequence "\\u0041" = 65


let%test "decode_c_escape_sequence rejects malformed escapes" =
  try
    ignore (decode_c_escape_sequence "\\x");
    false
  with Failure _ -> true


let%test "decode_c_char_literal_values handles raw and escaped characters" =
  decode_c_char_literal_values
    [ `Imm_tok_pat_36637e2 (test_token "A");
      `Esc_seq (test_token "\\n") ]
  = [65; 10]


let%test "int_of_char_literal folds multi-byte character literals" =
  int_of_char_literal
    (test_char_literal
       [ `Imm_tok_pat_36637e2 (test_token "A");
         `Imm_tok_pat_36637e2 (test_token "B") ])
  = ((Char.code 'A' lsl 8) lor Char.code 'B')


let%test "parse_number_literal distinguishes ints and floats" =
  parse_number_literal "17" = A.CInt 17
  && parse_number_literal "2.5f" = A.CFloat 2.5
  && parse_number_literal "1e3" = A.CFloat 1000.


let%test "parse_define_literal and eval_char_literal wrap parsed values in options" =
  parse_define_literal "0x20" = Some (A.CInt 32)
  && eval_char_literal (test_char_literal [ `Esc_seq (test_token "\\n") ]) = Some 10