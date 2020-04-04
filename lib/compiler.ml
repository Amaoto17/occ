open Ast
open Insn

type compile_context = {
  label_generator : int Stream.t;
  return_target : int;
  symtable : (Var.local, int) Hashtbl.t;
  label_map : (string, int) Hashtbl.t;
}

let new_label ctx = Stream.next ctx.label_generator

let compile_opt node_opt f k = match node_opt with
  | None -> k
  | Some node -> f node k

let rec compile_expr ctx expr k = match Expr.desc expr with
  | Expr.Number value -> Push value :: k
  | Expr.String (_, index) -> Get_string index :: k
  | Expr.Ident (Global (_, name)) -> Get_global name :: k
  | Expr.Ident (Local (typ, _, _)) ->
      compile_lval ctx expr
        (Load_addr (Type.size_of typ) :: k)
  | Expr.Call (name, args) ->
      let nargs = List.length args in
      compile_args ctx args (Call (name, nargs) :: k)
  | Expr.Implicit_cast expr' when Type.is_pointer (Expr.typ expr) ->
      compile_lval ctx expr' k
  | Expr.Implicit_cast expr -> compile_expr ctx expr k
  | Expr.Conditional (cond, then_, else_) ->
      let lelse = new_label ctx in
      let lend = new_label ctx in
      compile_expr ctx cond
        (Branch lelse :: compile_expr ctx then_
        (Jmp lend :: Label lelse :: compile_expr ctx else_
        (Label lend :: k)))
  | Expr.Unary_op (op, expr) -> compile_unary ctx op expr k
  | Expr.Binary_op ("=", left, right) ->
      compile_lval ctx left
        (compile_expr ctx right
        (Assign :: k))
  | Expr.Binary_op (op, left, right) -> compile_binary ctx op left right k
  | Expr.Member _ ->
      compile_lval ctx expr
        (Load_addr (Type.size_of @@ Expr.typ expr) :: k)

and compile_args ctx args k =
  List.fold_left (Fun.flip @@ compile_expr ctx) k args

and compile_unary ctx op expr k = match op with
  | "-" -> compile_expr ctx expr (Neg :: k)
  | "!" -> compile_expr ctx expr (Logical_not :: k)
  | "~" -> compile_expr ctx expr (Bit_not :: k)
  | "&" -> compile_lval ctx expr k
  | "*" -> compile_expr ctx expr (Load_addr (Type.size_of @@ Expr.typ expr) :: k)
  | "sizeof" -> Push (Type.size_of @@ Expr.typ expr) :: k
  | "postfix ++" -> compile_lval ctx expr (Postfix_inc :: k)
  | "postfix --" -> compile_lval ctx expr (Postfix_dec :: k)
  | "prefix ++" -> compile_lval ctx expr (Prefix_inc :: k)
  | "prefix --" -> compile_lval ctx expr (Prefix_dec :: k)
  | _ -> invalid_arg "compile_unary: invalid operator"

and compile_binary ctx op left right k =
  compile_expr ctx left
    (compile_expr ctx right
    (match op with
      | "+" -> Add :: k
      | "-" -> Sub :: k
      | "*" -> Mul :: k
      | "/" -> Div :: k
      | "%" -> Mod :: k
      | "<<" -> Asl :: k
      | ">>" -> Asr :: k
      | "<" -> Cmp Setl :: k
      | "<=" -> Cmp Setle :: k
      | ">" -> Cmp Setg :: k
      | ">=" -> Cmp Setge :: k
      | "==" -> Cmp Sete :: k
      | "!=" -> Cmp Setne :: k
      | "&" -> Bit_and :: k
      | "|" -> Bit_or :: k
      | "^" -> Bit_xor :: k
      | "&&" -> Logical_and :: k
      | "||" -> Logical_or :: k
      | "," -> Comma :: k
      | _ -> failwith "compile_binary: invalid operator"))

and compile_lval ctx expr k = match Expr.desc expr with
  | Expr.Unary_op ("*", expr) -> compile_expr ctx expr k
  | Expr.Ident (Global (_, name)) -> Get_global_addr name :: k
  | Expr.Ident (Local (typ, name, id)) ->
      let offset = Hashtbl.find ctx.symtable (typ, name, id) in
      Get_local_addr offset :: k
  | Expr.Member (".", expr, _, offset) ->
      compile_lval ctx expr (Offset offset :: k)
  | Expr.Member ("->", expr, _, offset) ->
      let size = Type.size_of @@ Expr.typ expr in
      compile_lval ctx expr (Load_addr size :: Offset offset :: k)
  | _ -> failwith "compile_lval: not an lvalue"

let rec compile_stmt ctx ct bt stmt k = match Stmt.desc stmt with
  | Stmt.Return expr ->
      compile_expr ctx expr
        (Return ctx.return_target :: k)
  | Stmt.If (cond, then_, None) ->
      let lend = new_label ctx in
      compile_expr ctx cond
        (Branch lend :: compile_stmt ctx ct bt then_
        (Label lend :: k))
  | Stmt.If (cond, then_, Some else_) ->
      let lelse = new_label ctx in
      let lend = new_label ctx in
      compile_expr ctx cond
        (Branch lelse :: compile_stmt ctx ct bt then_
        (Jmp lend :: Label lelse :: compile_stmt ctx ct bt else_
        (Label lend :: k)))
  | Stmt.For (init, cond, post, body) ->
      let lbody = new_label ctx in
      let lpost = new_label ctx in
      let lcond = new_label ctx in
      let lend = new_label ctx in
      compile_stmt ctx ct bt init
        (Jmp lcond :: Label lbody :: compile_stmt ctx lpost lend body
        (Label lpost :: compile_opt post (fun expr k -> compile_expr ctx expr (Pop :: k))
        (Label lcond :: compile_opt cond (fun expr k -> compile_expr ctx expr (Branch lend :: k))
        (Jmp lbody :: Label lend :: k))))
  | Stmt.While (cond, body) ->
      let lbegin = new_label ctx in
      let lend = new_label ctx in
      Label lbegin :: compile_expr ctx cond
        (Branch lend :: compile_stmt ctx lbegin lend body
        (Jmp lbegin :: Label lend :: k))
  | Stmt.Do (body, cond) ->
      let lbegin = new_label ctx in
      let lend = new_label ctx in
      Label lbegin :: compile_stmt ctx lbegin lend body
        (compile_expr ctx cond
        (Branch lend :: Jmp lbegin :: Label lend :: k))
  | Stmt.Switch (cond, body) ->
      compile_switch ctx ct bt cond body k
  | Stmt.Case _ -> failwith "case label not within a switch statement"
  | Stmt.Default _ -> failwith "'default' label not within a switch statement"
  | Stmt.Break -> Jmp bt :: k
  | Stmt.Continue -> Jmp ct :: k
  | Stmt.Label (name, _index, body) ->
      (match Hashtbl.find_opt ctx.label_map name with
        | None ->
            let ltarget = new_label ctx in
            Hashtbl.add ctx.label_map name ltarget;
            Label ltarget :: compile_stmt ctx ct bt body k
        | Some ltarget -> Label ltarget :: compile_stmt ctx ct bt body k)
  | Stmt.Goto (name, _target) ->
      (match Hashtbl.find_opt ctx.label_map name with
        | None ->
            let ltarget = new_label ctx in
            Hashtbl.add ctx.label_map name ltarget;
            Jmp ltarget :: k
        | Some ltarget -> Jmp ltarget :: k)
  | Stmt.Compound stmt_list ->
      List.fold_right (compile_stmt ctx ct bt) stmt_list k
  | Stmt.Decl (decl, Some init) ->
      (match Decl.desc decl with
        | Decl.Var (Local var) ->
            let offset = Hashtbl.find ctx.symtable var in
            Get_local_addr offset :: compile_expr ctx init
              (Assign :: Pop :: k)
        | _ -> k)
  | Stmt.Decl (_, None) -> k
  | Stmt.Expr expr -> compile_expr ctx expr (Pop :: k)
  | Stmt.Null -> k

and compile_switch ctx ct bt cond body k =
  compile_expr ctx cond
    (Switch :: (match Stmt.desc body with
      | Stmt.Compound cases -> skip_before_first_case ctx ct bt cases k
      | _ -> skip_before_first_case ctx ct bt [body] k))

and skip_before_first_case ctx ct bt cases k = match cases with
  | [] -> compile_case_branch ctx ct bt [] k
  | stmt :: rest -> match Stmt.desc stmt with
      | Stmt.Case _ | Stmt.Default _ ->
          compile_case_branch ctx ct bt cases k
      | _ -> skip_before_first_case ctx ct bt rest k

and compile_case_branch ctx ct _bt cases k =
  let lend = new_label ctx in
  let rec loop ldef_opt accum = function
    | [] ->
        compile_opt ldef_opt (fun ldef k -> Jmp ldef :: k)
          (List.fold_right (@@) (List.rev accum)
          (Label lend :: k))
    | stmt :: rest -> match Stmt.desc stmt with
        | Stmt.Case (value, body) ->
            let lcase = new_label ctx in
            let f k = Label lcase :: compile_stmt ctx ct lend body k in
            Case (lcase, value) :: loop ldef_opt (f :: accum) rest
        | Stmt.Default _ when Option.is_some ldef_opt ->
            failwith "deprecated default"
        | Stmt.Default body ->
            let lcase = new_label ctx in
            let f k = Label lcase :: compile_stmt ctx ct lend body k in
            loop (Some lcase) (f :: accum) rest
        | _ ->
            let f k = compile_stmt ctx ct lend stmt k in
            loop ldef_opt (f :: accum) rest
  in loop None [] cases

let roundup n align =
  (n + align - 1) land lnot (align - 1)

let make_symtable symtable lvars =
  let rec loop ?(offset=0) = function
    | [] -> roundup offset 16
    | ((typ, _, _) as lvar) :: rest ->
        let offset = offset + Type.size_of typ in
        Hashtbl.add symtable lvar offset;
        loop ~offset:(roundup offset @@ Type.align_of typ) rest
  in loop lvars

let compile_strings _ctx strings k =
  ListLabels.fold_right strings ~init:k ~f:(fun str k ->
    { k with rodata = Str str :: k.rodata })

let compile_params ctx params k =
  let offsets = ListLabels.map params ~f:(Hashtbl.find ctx.symtable) in
  Load_params offsets :: k

let compile_definitions ctx definitions k =
  ListLabels.fold_right definitions ~init:k ~f:(fun def k ->
    match def with
    | Transl_unit.Decl_only _ -> k
    | Transl_unit.Global_var (typ, _) ->
        let size = Type.size_of typ in
        { k with bss = Bytes size :: k.bss }
    | Transl_unit.Function ({ name; params; _ }, body, lvars) ->
        let return_target = new_label ctx in
        let symtable = Hashtbl.create 8 in
        let framesize = make_symtable symtable lvars in
        let ctx = { ctx with return_target; symtable } in
        let insn_seq =
          compile_params ctx params (compile_stmt ctx (-1) (-1) body []) in
        { k with text = Function (name, return_target, framesize, insn_seq) :: k.text })

let compile_transl_unit ctx Transl_unit.{ definitions; str_literals } =
  compile_strings ctx str_literals
    (compile_definitions ctx definitions
    { data = []; rodata = []; bss = []; text = [] })

let run transl_unit =
  let ctx = {
    label_generator = Stream.from Option.some;
    return_target = -1;
    symtable = Hashtbl.create 1;
    label_map = Hashtbl.create 8;
  } in compile_transl_unit ctx transl_unit
