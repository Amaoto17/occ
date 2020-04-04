open Ast
open Parsing_context

exception Semantic_error of string

let semantic_error fmt =
  let error msg = raise @@ Semantic_error msg in
  Printf.ksprintf error fmt

let int_literal _ctx lit =
  Expr.number ~typ:Type.Int (int_of_string lit)

let string_literal ctx str =
  let index = add_string_literal ctx str in
  Expr.string ~typ:(Type.(pointer_to Char)) str index

let char_literal _ctx c =
  Expr.number ~typ:Type.Char @@ int_of_char c

let is_compatible_int = function
  | Type.Int | Type.Char -> true
  | _ -> false

let ident_expr ctx name = match Env.find ctx.var_env name with
  | None -> semantic_error "undefined symbol %s" name
  | Some (Var.Local (typ, _, _) as var) -> Expr.ident ~typ var
  | Some (Var.Global (typ, _) as var) -> Expr.ident ~typ var

(* let ident_expr ctx name = match Env.find ctx.var_env name with
  | None -> semantic_error "undefined symbol %s" name
  | Some { typ; desc } ->
      let index = match desc with
        | Var (_, index_opt, _) -> index_opt
        | Function -> None
        | _ -> semantic_error "ident_expr: unexpected declaration"
      in Expr.ident ~typ name index *)

(* let function_call ctx expr args =
  match Expr.desc expr, Type.returning_of Expr.typ expr with
  | Expr.Ident (Global (_, name)), Some rettyp ->
      Expr.call ~typ:rettyp name args
  | _ -> semantic_error "function_call: not a function" *)

let function_call _ctx expr args = match Expr.desc expr with
  | Expr.Ident (Global (Type.Function rettyp, name)) ->
      Expr.call ~typ:rettyp name args
  | _ -> semantic_error "function_call: not a function"

let implicit_cast ~typ expr = match Expr.typ expr with
  | t when t = typ -> expr
  | _ -> Expr.implicit_cast ~typ expr

let decay_type_conversion expr = match Expr.typ expr with
  | Type.Array (underlying, _) ->
      let outtyp = Type.pointer_to underlying in
      implicit_cast ~typ:outtyp expr
  | _ -> expr

let deref_expr _ctx expr =
  let expr = decay_type_conversion expr in
  match Expr.typ expr with
  | Type.Pointer typ -> Expr.unary_op ~typ ~op:"*" expr
  | _ -> semantic_error "deref_expr: not a pointer"

let scale_pointer ~op size expr =
  let left = expr
  and right = Expr.number ~typ:Type.Int size in
  Expr.binary_op ~typ:Type.Int ~op left right

let rec check_type_add left right = match Expr.typ left, Expr.typ right with
  | Type.Pointer underlying, typ when is_compatible_int typ ->
      let size = Type.size_of underlying in
      let right = right |> implicit_cast ~typ:Type.Int |> scale_pointer ~op:"*" size in
      left, right
  | _, Type.Pointer _ -> check_type_add right left
  | _, _ when is_compatible_int (Expr.typ left) && is_compatible_int (Expr.typ right) ->
      let left = implicit_cast ~typ:Type.Int left
      and right = implicit_cast ~typ:Type.Int right in
      left, right
  | _ -> semantic_error "check_type_add: invalid operand"

let check_type_sub left right = match Expr.typ left, Expr.typ right with
  | Type.Pointer underlying, typ when is_compatible_int typ ->
      let size = Type.size_of underlying in
      let right = right |> implicit_cast ~typ:Type.Int |> scale_pointer ~op:"*" size in
      Expr.binary_op ~typ:(Expr.typ left) ~op:"-" left right
  | Type.Pointer underlying, Type.Pointer other when underlying = other ->
      let size = Type.size_of underlying in
      Expr.binary_op ~typ:Type.Int ~op:"-" left right
        |> scale_pointer ~op:"/" size
  | _, _ when is_compatible_int (Expr.typ left) && is_compatible_int (Expr.typ right) ->
      let left = implicit_cast ~typ:Type.Int left
      and right = implicit_cast ~typ:Type.Int right in
      Expr.binary_op ~typ:(Expr.typ left) ~op:"-" left right
  | _ -> semantic_error "check_type_sub: invalid operand"

let check_type_assign left right = match Expr.typ left with
  | Type.Int | Type.Char when is_compatible_int (Expr.typ right) ->
      let right = implicit_cast ~typ:(Expr.typ left) right in
      left, right
  | _ when Expr.typ left = Expr.typ right -> left, right
  | _ -> semantic_error "check_type_assign: type mismatch"

let check_lval expr = match Expr.desc expr with
  | Expr.Ident _ -> ()
  | Expr.Unary_op (op, _) when op = "*" -> ()
  | Expr.Member _ -> ()
  | _ -> semantic_error "check_lval: not an lvalue"

let rec binary_expr ctx op left right = match op with
  | "=" ->
      check_lval left;
      let right = decay_type_conversion right in
      let (left, right) = check_type_assign left right in
      Expr.binary_op ~typ:(Expr.typ right) ~op left right
  | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "^=" | "|=" ->
      check_lval left;
      let op = String.sub op 0 @@ String.length op - 1 in
      let right = binary_expr ctx op left right in
      Expr.binary_op ~typ:(Expr.typ right) ~op:"=" left right
  | "+" ->
      let left = decay_type_conversion left
      and right = decay_type_conversion right in
      let (left, right) = check_type_add left right in
      Expr.binary_op ~typ:(Expr.typ left) ~op left right
  | "-" ->
      let left = decay_type_conversion left
      and right = decay_type_conversion right in
      check_type_sub left right
  | "," ->
      Expr.binary_op ~typ:(Expr.typ right) ~op left right
  | _ ->
      let left = decay_type_conversion left
      and right = decay_type_conversion right in
      match Expr.typ left, Expr.typ right with
      | Type.Int, Type.Int -> Expr.binary_op ~typ:Type.Int ~op left right
      | _ -> semantic_error "binary_expr: expected integer"

let subscript_expr ctx operand expr =
  deref_expr ctx @@ binary_expr ctx "+" operand expr

let postfix_incdec_expr _ctx expr op =
  check_lval expr;
  Expr.unary_op ~typ:Type.Int ~op:(Printf.sprintf "postfix %s" op) expr

let struct_member_access op operand member_name = function
  | Type.Struct (members, _, _) ->
      begin match Type.Members.find_opt member_name members with
      | None -> semantic_error "undefined symbol %s" member_name
      | Some (typ, offset) ->
          Expr.member ~typ op operand member_name offset
      end
  | _ -> semantic_error "struct expected"

let member_expr _ctx operand op member_name =
  struct_member_access op operand member_name @@ match Expr.typ operand with
    | Type.Pointer typ when op = "->" -> typ
    | typ -> typ

let prefix_incdec_expr _ctx op expr =
  check_lval expr;
  Expr.unary_op ~typ:Type.Int ~op:(Printf.sprintf "prefix %s" op) expr

let neg_expr _ctx expr =
  let typ = Expr.typ expr in
  if not @@ is_compatible_int typ then semantic_error "neg_expr: not an integer";
  Expr.unary_op ~typ ~op:"-" expr

let bit_not_expr _ctx expr =
  Expr.unary_op ~typ:(Expr.typ expr) ~op:"~" expr

let logical_not_expr _ctx expr =
  Expr.unary_op ~typ:(Expr.typ expr) ~op:"!" expr

let addr_expr _ctx expr =
  Expr.unary_op ~typ:(Type.pointer_to @@ Expr.typ expr) ~op:"&" expr

let sizeof_expr _ctx expr =
  Expr.unary_op ~typ:Type.Int ~op:"sizeof" expr

let conditional_expr _ctx then_ cond else_ =
  Expr.conditional ~typ:(Expr.typ then_) cond then_ else_

let const_expr _ctx expr = match Expr.desc expr with
  | Expr.Number value -> value
  | _ -> semantic_error "constant expression expected"

let roundup n align =
  (n + align - 1) land lnot (align - 1)

let rec make_struct_type ?(offset=0) ?(align=0) ?(members=Type.Members.empty) = function
  | [] ->
      let size = roundup offset align in
      Type.Struct (members, size, align)
  | decl :: rest -> match Decl.desc decl with
      | Decl.Var (Local (typ, name, _)) ->
          let member_offset = roundup offset @@ Type.align_of typ in
          let offset = member_offset + Type.size_of typ in
          let align = max align @@ Type.align_of typ in
          let members = Type.Members.add name (typ, member_offset) members in
          make_struct_type ~offset ~align ~members rest
      | _ -> semantic_error "struct_decl: variable definition expected"

let anonymous_struct_decl _ctx = function
  | None -> semantic_error "struct_decl: bad struct definition"
  | Some decl_list -> make_struct_type decl_list

let struct_decl ctx struct_name_opt decl_list_opt =
  match struct_name_opt with
  | None -> anonymous_struct_decl ctx decl_list_opt
  | Some struct_name -> match Env.find ctx.struct_tag_env struct_name with
      | Some _ when Option.is_some decl_list_opt ->
          semantic_error "struct_decl: redefinition of struct %s" struct_name
      | Some typ -> typ
      | None ->
          let decl_list = Option.value ~default:[] decl_list_opt in
          let typ = make_struct_type decl_list in
          Env.add ctx.struct_tag_env struct_name typ; typ

(* let variable_decl ctx name typ init =
  match Env.find ctx.var_env ~recursive:false name with
  | Some _ -> semantic_error "%s has already been declared" name
  | None ->
      let index = Option.bind ctx.current_function (fun _ ->
        Option.some @@ List.length ctx.local_vars) in
      let decl = Decl.var ~typ name init index in
      Option.iter (fun _ -> add_local_var ctx decl) index;
      Env.add ctx.var_env name decl; decl *)

let variable_decl ctx name typ =
  match Env.find ctx.var_env ~recursive:false name with
    | Some _ -> semantic_error "%s has already been declared" name
    | None ->
        let id = new_id ctx in
        let var = Var.local ~typ name id in
        Env.add ctx.var_env name var;
        add_lvar ctx (typ, name, id);
        Decl.var var

let compound_stmt_enter ctx = push_scope ctx

let compound_stmt_leave ctx stmt_list =
  pop_scope ctx;
  Stmt.compound stmt_list

let return_stmt _ctx expr = Stmt.return expr

let if_stmt _ctx cond then_ else_ = Stmt.if_ cond then_ else_

let while_stmt_enter ctx =
  push_scope ctx;
  push_contflow ctx `Loop

let while_stmt_leave ctx cond body =
  pop_scope ctx;
  pop_contflow ctx;
  Stmt.while_ cond body

let do_stmt_enter ctx =
  push_scope ctx;
  push_contflow ctx `Loop

let do_stmt_leave ctx body cond =
  pop_scope ctx;
  pop_contflow ctx;
  Stmt.do_ body cond

let for_stmt_enter ctx =
  push_scope ctx;
  push_contflow ctx `Loop

let for_stmt_leave ctx init cond post body =
  pop_scope ctx;
  pop_contflow ctx;
  Stmt.for_ init cond post body

let switch_stmt_enter ctx =
  push_scope ctx;
  push_contflow ctx `Switch

let switch_stmt_leave ctx cond body =
  pop_scope ctx;
  pop_contflow ctx;
  Stmt.switch cond body

let case_stmt _ctx value body = Stmt.case value body

let default_stmt _ctx body = Stmt.default body

let break_stmt ctx =
  if not @@ break_is_allowed ctx then
    semantic_error "break outside of loop";
  Stmt.break

let continue_stmt ctx =
  if not @@ continue_is_allowed ctx then
    semantic_error "continue outside of loop";
  Stmt.continue

let goto_stmt ctx label = match find_label ctx label with
  | None ->
      let node = Stmt.goto label (ref (-1)) in
      add_unresolved_goto ctx node; node
  | Some index -> Stmt.goto label (ref index)

let labeled_stmt ctx name body =
  let index = add_label ctx name in
  Stmt.label name index body

let decl_stmt _ctx decl init = match Decl.desc decl with
  | Decl.Var _ -> Stmt.decl decl init
  | _ -> semantic_error "unexpected initialization"

let null_stmt _ctx = Stmt.null

let expr_stmt _ctx expr = Stmt.expr expr

let function_params_enter ctx =
  push_scope ctx

let function_params_leave ctx func_name rettyp params =
  pop_scope ctx;
  let typ = Type.Function rettyp in
  let var = Var.global ~typ func_name in
  let params = ListLabels.map params ~f:(fun decl ->
    match Decl.desc decl with
    | Decl.Var (Local var) -> var
    | _ -> failwith "unreachable") in
  Env.add ctx.var_env func_name var;
  let decl = Decl.function_ ~typ func_name params in
  Transl_unit.decl_only decl

let function_decl_enter ctx = function
  | Transl_unit.Decl_only decl ->
      (match Decl.desc decl with
        | Decl.Function (_, { name; params }) ->
            set_current_function ctx name;
            push_scope ctx;
            ListLabels.iter params ~f:(fun ((_, name, _) as var) ->
              add_lvar ctx var;
              Env.add ctx.var_env name (Local var))
        | _ -> semantic_error "function_decl_enter: not a function")
  | _ -> semantic_error "function_decl_enter: not a function"

let function_decl_leave ctx def body =
  let lvars = List.rev ctx.lvars in
  pop_scope ctx;
  unset_current_function ctx;
  ctx.lvars <- [];
  match def with
  | Transl_unit.Decl_only decl ->
      (match Decl.desc decl with
        | Decl.Function (_, func) ->
            Transl_unit.function_ func body lvars
        | _ -> semantic_error "function_decl_leave: not a function")
  | _ -> semantic_error "function_decl_leave: not a function"

let toplevel_typedef ctx typ name =
  Env.add ctx.typedef_env name typ;
  let decl = Decl.typedef ~typ name in
  Transl_unit.decl_only decl

let global_variable ctx typ name =
  let var = Var.global ~typ name in
  Env.add ctx.var_env name var;
  Transl_unit.global_var (typ, name)

let resolve_goto_target ctx =
  ListLabels.iter ctx.unresolved_goto ~f:(fun stmt ->
    match Stmt.desc stmt with
    | Stmt.Goto (label, target) ->
        (match find_label ctx label with
          | None -> semantic_error "undeclared label %s" label
          | Some index -> target := index)
    | _ -> semantic_error "resolve_goto_target: unreachable")

let transl_unit ctx definitions =
  resolve_goto_target ctx;
  Transl_unit.{ definitions; str_literals = List.rev ctx.string_literals }
