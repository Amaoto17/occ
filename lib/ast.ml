module Var = struct
  type t = Local of local | Global of global
  and local = Type.t * string * int
  and global = Type.t * string

  let local ~typ name id = Local (typ, name, id)
  let global ~typ name = Global (typ, name)
end

module Expr = struct
  type t = {
    desc : desc;
    typ : Type.t;
  }
  
  and desc =
    | Number of int
    | String of string * int
    | Ident of Var.t
    | Call of string * t list
    | Implicit_cast of t
    | Conditional of t * t * t
    | Binary_op of string * t * t
    | Unary_op of string * t
    | Member of string * t * string * int

  let typ t = t.typ
  let desc t = t.desc

  let number ~typ i = { typ; desc = Number i }
  let string ~typ str index_opt = { typ; desc = String (str, index_opt) }
  let ident ~typ var = { typ; desc = Ident var }
  let call ~typ name args = { typ; desc = Call (name, args) }
  let implicit_cast ~typ t = { typ; desc = Implicit_cast t }
  let conditional ~typ cond then_ else_ = { typ; desc = Conditional (cond, then_, else_) }
  let binary_op ~typ ~op left right = { typ; desc = Binary_op (op, left, right) }
  let unary_op ~typ ~op t = { typ; desc = Unary_op (op, t) }
  let member ~typ op t name offset = { typ; desc = Member (op, t, name, offset) }
end

module Decl = struct
  type t = {
    desc : desc;
  }

  and desc =
    | Var of Var.t
    | Typedef of Type.t * string
    | Function of Type.t * func

  and func = {
    name : string;
    params : Var.local list;
  }

  let desc t = t.desc

  let var v = { desc = Var v }
  let typedef ~typ name = { desc = Typedef (typ, name) }
  let function_ ~typ name params = { desc = Function (typ, { name; params }) }
end

module Stmt = struct
  type t = {
    desc : desc;
  }
  
  and desc =
    | Return of Expr.t
    | If of Expr.t * t * t option
    | While of Expr.t * t
    | Do of t * Expr.t
    | For of t * Expr.t option * Expr.t option * t
    | Switch of Expr.t * t
    | Case of int * t
    | Default of t
    | Break
    | Continue
    | Decl of Decl.t * Expr.t option
    | Label of string * int * t
    | Goto of string * int ref
    | Expr of Expr.t
    | Compound of t list
    | Null

  let desc t = t.desc

  let return expr = { desc = Return expr }
  let if_ cond then_ else_ = { desc = If (cond, then_, else_) }
  let while_ cond body = { desc = While (cond, body) }
  let do_ body cond = { desc = Do (body, cond) }
  let for_ init cond post body = { desc = For (init, cond, post, body) }
  let switch cond cases = { desc = Switch (cond, cases) }
  let case value body = { desc = Case (value, body) }
  let default body = { desc = Default body }
  let break = { desc = Break }
  let continue = { desc = Continue }
  let decl d init = { desc = Decl (d, init) }
  let label name index body = { desc = Label (name, index, body) }
  let goto label target = { desc = Goto (label, target) }
  let expr e = { desc = Expr e }
  let compound stmts = { desc = Compound stmts }
  let null = { desc = Null }
end

module Transl_unit = struct
  type t = {
    definitions : definition list;
    str_literals : string list;
  }

  and definition =
    | Decl_only of Decl.t
    | Global_var of Var.global
    | Function of Decl.func * Stmt.t * Var.local list

  let decl_only decl = Decl_only decl
  let global_var gvar = Global_var gvar
  let function_ func body lvars = Function (func, body, lvars)
end
