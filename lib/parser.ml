open Printf
open Ast
open Parser_combinator
open Parsing_context

external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"

exception Syntax_error of string

let syntax_error msg = raise @@ Syntax_error msg

let is_space_char = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_digit_char = function
  | '0' .. '9' -> true
  | _ -> false

let is_ident_init_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let line_comment =
  string "//" *> skip_till ((=) '\n')

let space =
  skip_many (skip_while1 is_space_char <|> line_comment)

(* let space = skip_while is_space_char *)

let symbol s =
  string s <* space

let keyword s =
  string s <* not_followed_by is_ident_char <* space

let ident =
  satisfy is_ident_init_char >>= fun c ->
  take_while is_ident_char >>= fun s ->
  space *> return (sprintf "%c%s" c s)

let paren p =
  symbol "(" *> p <* symbol ")"

let action f st = (return @@ f ()) st

let int_literal ctx =
  take_while1 is_digit_char
    <* space
    >>| Sema.int_literal ctx

let string_literal ctx =
  char '"' *> take_till ((=) '"') <* char '"'
    >>| Sema.string_literal ctx

let char_literal ctx =
  char '\'' *> not_char '\'' <* char '\''
    <* space
    >>| Sema.char_literal ctx

let rec primary ctx = fun st -> (|>) st &
  choice [
    paren (expr ctx)
  ; int_literal ctx
  ; char_literal ctx
  ; string_literal ctx
  ; ident >>| Sema.ident_expr ctx
  ]

and postfix ctx = fun st -> (|>) st &
  primary ctx >>= fun expr ->
  choice [
  (* function call *)
    paren (sep_by (symbol ",") (assign ctx))
      >>| Sema.function_call ctx expr
  (* array subscript *)
  ; (symbol "[" *> assign ctx <* symbol "]"
      >>| Sema.subscript_expr ctx expr)
  (* increment, decrement *)
  ; (symbol "++" <|> symbol "--"
      >>| Sema.postfix_incdec_expr ctx expr)
  (* member access *)
  ; lift2 (Sema.member_expr ctx expr)
      (symbol "." <|> symbol "->")
      ident
  ; return expr
  ]

and unary ctx = fun st -> (|>) st &
  choice [
  (* increment, decrement *)
    lift2 (Sema.prefix_incdec_expr ctx)
      (symbol "++" <|> symbol "--")
      (unary ctx)
  (* unary minus *)
  ; symbol "-" *> unary ctx
      >>| Sema.neg_expr ctx
  (* bit-not *)
  ; symbol "~" *> unary ctx
      >>| Sema.bit_not_expr ctx
  (* logical-not *)
  ; symbol "!" *> unary ctx
      >>| Sema.logical_not_expr ctx
  (* address *)
  ; symbol "&" *> unary ctx
      >>| Sema.addr_expr ctx
  (* dereference *)
  ; symbol "*" *> unary ctx
      >>| Sema.deref_expr ctx
  (* sizeof *)
  ; keyword "sizeof" *> unary ctx
      >>| Sema.sizeof_expr ctx
  ; postfix ctx
  ]

and mul_op ctx = fun st -> (|>) st &
  string "/" <|> string "*" <|> string "%"
    <* not_followed_by ((=) '=')
    <* space
    >>| Sema.binary_expr ctx

and add_op ctx = fun st -> (|>) st &
  string "+" <|> string "-"
    <* not_followed_by ((=) '=')
    <* space
    >>| Sema.binary_expr ctx

and shift_op ctx = fun st -> (|>) st &
  string "<<" <|> string ">>"
    <* not_followed_by ((=) '=')
    <* space
    >>| Sema.binary_expr ctx

and rel_op ctx = fun st -> (|>) st &
  choice [symbol "<=" ; symbol ">="; symbol "<"; symbol ">"]
    >>| Sema.binary_expr ctx

and eq_op ctx = fun st -> (|>) st &
  symbol "==" <|> symbol "!="
    >>| Sema.binary_expr ctx

and bit_and_op ctx = fun st -> (|>) st &
  string "&"
    <* not_followed_by (function '&' | '=' -> true | _ -> false)
    <* space
    >>| Sema.binary_expr ctx

and bit_xor_op ctx = fun st -> (|>) st &
  string "^"
    <* not_followed_by ((=) '=')
    <* space
    >>| Sema.binary_expr ctx

and bit_or_op ctx = fun st -> (|>) st &
  string "|"
    <* not_followed_by (function '|' | '=' -> true | _ -> false)
    <* space
    >>| Sema.binary_expr ctx

and logical_and_op ctx = fun st -> (|>) st &
  symbol "&&"
    >>| Sema.binary_expr ctx

and logical_or_op ctx = fun st -> (|>) st &
  symbol "||"
    >>| Sema.binary_expr ctx

and conditional_op ctx = fun st -> (|>) st &
  symbol "?" *>
  expr ctx
    <* symbol ":"
    >>| Sema.conditional_expr ctx

and assign_op ctx = fun st -> (|>) st &
  choice [
    symbol "=" ; symbol "+="; symbol "-=" ; symbol "*=" ;
    symbol "/="; symbol "%="; symbol ">>="; symbol "<<=";
    symbol "&="; symbol "^="; symbol "|="
  ] >>| Sema.binary_expr ctx

and comma_op ctx = fun st -> (|>) st &
  symbol ","
    >>| Sema.binary_expr ctx

and mul ctx = fun st -> (|>) st & chainl (unary ctx) (mul_op ctx)
and add ctx = fun st -> (|>) st & chainl (mul ctx) (add_op ctx)
and shift ctx = fun st -> (|>) st & chainl (add ctx) (shift_op ctx)
and rel ctx = fun st -> (|>) st & chainl (shift ctx) (rel_op ctx)
and eq ctx = fun st -> (|>) st & chainl (rel ctx) (eq_op ctx)
and bit_and ctx = fun st -> (|>) st & chainl (eq ctx) (bit_and_op ctx)
and bit_xor ctx = fun st -> (|>) st & chainl (bit_and ctx) (bit_xor_op ctx)
and bit_or ctx = fun st -> (|>) st & chainl (bit_xor ctx) (bit_or_op ctx)
and logical_and ctx = fun st -> (|>) st & chainl (bit_or ctx) (logical_and_op ctx)
and logical_or ctx = fun st -> (|>) st & chainl (logical_and ctx) (logical_or_op ctx)
and conditional ctx = fun st -> (|>) st & chainr (logical_or ctx) (conditional_op ctx)
and assign ctx = fun st -> (|>) st & chainr (conditional ctx) (assign_op ctx)
and comma ctx = fun st -> (|>) st & chainl (assign ctx) (comma_op ctx)
and expr ctx = fun st -> (|>) st & comma ctx

and const_expr ctx = fun st -> (|>) st &
  expr ctx
    >>| Sema.const_expr ctx

let find_typedef ctx name =
  match Env.find ctx.typedef_env name with
  | Some typ -> return typ
  | None -> fail "type specifier expected"

let rec decl_specifiers ctx = fun st -> (|>) st &
  choice [
    keyword "int" *> return Type.Int
  ; keyword "char" *> return Type.Char
  ; keyword "void" *> return Type.Void
  ; keyword "struct" *>
    lift2 (Sema.struct_decl ctx)
      (option ident)
      (option (symbol "{" *> many (declaration ctx <* symbol ";") <* symbol "}"))
  ; ident >>= find_typedef ctx
  ]

and read_array ctx typ =
  many (symbol "[" *> const_expr ctx <* symbol "]")
    >>| List.fold_left Type.array_of typ

and direct_decl ctx typ =
  lift2 (Sema.variable_decl ctx)
    (ident <|>
      (symbol "(" *> declarator ctx typ >>= fun decl ->
        match Decl.desc decl with
        | Decl.Var (Local (_, name, _)) -> return name
        | Decl.Var (Global (_, name)) -> return name
        | _ -> failwith "direct_decl: unreachable"))
    (read_array ctx typ)

and declarator ctx typ =
  many (symbol "*" *> return Type.pointer_to)
    >>| List.fold_left (|>) typ
    >>= direct_decl ctx

and declaration ctx = fun st -> (|>) st &
  decl_specifiers ctx >>= declarator ctx

let rec compound_stmt ctx = fun st -> (|>) st &
  action (fun _ -> Sema.compound_stmt_enter ctx) *>
  many (stmt ctx)
   <* symbol "}"
   >>| Sema.compound_stmt_leave ctx

and null_stmt ctx = fun st -> (|>) st &
  symbol ";" *> return (Sema.null_stmt ctx)

and decl_stmt ctx = fun st -> (|>) st &
  lift2 (Sema.decl_stmt ctx)
    (declaration ctx)
    (option (symbol "=" *> assign ctx)) <* symbol ";"

and expr_stmt ctx = fun st -> (|>) st &
  expr ctx <* symbol ";"
    >>| Sema.expr_stmt ctx

and stmt ctx = fun st -> (|>) st &
  choice [
    keyword "return" *>
    expr ctx
      <* symbol ";"
      >>| Sema.return_stmt ctx
  ; (keyword "if" *>
      paren (expr ctx) >>= fun cond ->
      stmt ctx >>= fun then_ ->
      option (keyword "else" *> stmt ctx)
        >>| Sema.if_stmt ctx cond then_)
  ; (keyword "while" *>
      paren (expr ctx) >>= fun cond ->
      action (fun _ -> Sema.while_stmt_enter ctx) *>
      stmt ctx
        >>| Sema.while_stmt_leave ctx cond)
  ; (keyword "do" *>
      action (fun _ -> Sema.do_stmt_enter ctx) *>
      stmt ctx >>= fun body ->
      keyword "while" *>
      paren (expr ctx)
        <* symbol ";"
        >>| Sema.do_stmt_leave ctx body)
  ; keyword "for" *>
    action (fun _ -> Sema.for_stmt_enter ctx) *>
    symbol "(" *>
    lift4 (Sema.for_stmt_leave ctx)
      (null_stmt ctx <|> decl_stmt ctx <|> expr_stmt ctx)
      (option (expr ctx) <* symbol ";")
      (option (expr ctx) <* symbol ")")
      (stmt ctx)
  ; (keyword "switch" *>
      paren (expr ctx) >>= fun cond ->
      action (fun _ -> Sema.switch_stmt_enter ctx) *>
      stmt ctx
        >>| Sema.switch_stmt_leave ctx cond)
  ; (keyword "case" *>
      const_expr ctx >>= fun value ->
      symbol ":" *>
      stmt ctx
        >>| Sema.case_stmt ctx value)
  ; keyword "default" *>
    symbol ":" *>
    stmt ctx
      >>| Sema.default_stmt ctx
  ; keyword "break" *>
    symbol ";" *>
    action (fun _ -> Sema.break_stmt ctx)
  ; keyword "continue" *>
    symbol ";" *>
    action (fun _ -> Sema.continue_stmt ctx)
  ; keyword "goto" *>
    ident
      <* symbol ";"
      >>| Sema.goto_stmt ctx
  ; lift2 (Sema.labeled_stmt ctx)
      (ident <* symbol ":")
      (stmt ctx)
  ; symbol "{" *> compound_stmt ctx
  ; decl_stmt ctx
  ; null_stmt ctx
  ; expr_stmt ctx
  ]

let param_decl ctx =
  decl_specifiers ctx >>= declarator ctx

let function_definition ctx decl =
  choice [
    symbol ";" *> return decl
  ; symbol "{" *>
    action (fun _ -> Sema.function_decl_enter ctx decl) *>
    compound_stmt ctx
      >>| Sema.function_decl_leave ctx decl
  ]

let function_decl ctx typ name =
  action (fun _ -> Sema.function_params_enter ctx) *>
  sep_by (symbol ",") (param_decl ctx)
    <* symbol ")"
    >>| Sema.function_params_leave ctx name typ
    >>= function_definition ctx

let toplevel_typedef ctx =
  lift2 (Sema.toplevel_typedef ctx)
    (decl_specifiers ctx)
    ident

let toplevel ctx =
  choice [
    keyword "typedef" *> toplevel_typedef ctx
  ; decl_specifiers ctx >>= fun typ ->
    ident >>= fun name ->
    choice [
      symbol "(" *> function_decl ctx typ name
    ; return (Sema.global_variable ctx typ name)
    ]
  ]

let transl_unit ctx =
  space *>
  many1 (toplevel ctx)
    <* end_of_input
    >>| Sema.transl_unit ctx

let run str =
  let ctx = Parsing_context.create () in
  match parse_string (transl_unit ctx) str with
  | Ok ast -> ast
  | Error msg -> syntax_error msg
