{
  (*
   * - `ocamllex lexer.mll` pour produire le fichier lexer.ml
   * - `ocaml lexer.ml <expr>` pour l'exécuter
   *)
  open Lexing;;

  exception Lexer of string;;

  type token =
    Eof
  | And
  | Or
  | Not
  | Opar
  | Cpar
  | Assign
  | Semicol
  | Var of string

  let string_of_token t =
    match t with
      Eof     -> "Eof"
    | And     -> "And"
    | Or      -> "Or"
    | Not     -> "Not"
    | Opar    -> "Opar"
    | Cpar    -> "Cpar"
    | Assign  -> "Assign"
    | Semicol -> "Semicol"
    | Var (s) -> "Var(" ^ s ^ ")"
}

rule tokenize = parse
| eof { Eof }
| [ ' ' '\t' '\n' ] { tokenize lexbuf }
| "&&" { And }
| "||" { Or }
| "!"  { Not }
| "("  { Opar }
| ")"  { Cpar }
| "="  { Assign }
| ";"  { Semicol }
| ['a'-'z' 'A'-'Z']+ as l { Var (l) }
| _ as c { raise (Lexer (Printf.sprintf "Unrecognized char '%c' at offset %d."
                                           c lexbuf.lex_curr_p.pos_cnum)) }

{
  let expr = Lexing.from_string Sys.argv.(1) in
  let rec loop t =
    if t = Eof then ()
    else
      begin
        Printf.printf "%s\n" (string_of_token t) ;
        loop (tokenize expr)
      end
  in loop (tokenize expr)
}
