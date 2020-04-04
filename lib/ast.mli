module Var : sig
  type t = Local of local | Global of global
  and local = Type.t * string * int
  and global = Type.t * string

  val local : typ:Type.t -> string -> int -> t
  val global : typ:Type.t -> string -> t
end

module Expr : sig
  type t
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

  val typ : t -> Type.t
  val desc : t -> desc

  val number : typ:Type.t -> int -> t
  val string : typ:Type.t -> string -> int -> t
  val ident : typ:Type.t -> Var.t -> t
  val call : typ:Type.t -> string -> t list -> t
  val implicit_cast : typ:Type.t -> t -> t
  val conditional : typ:Type.t -> t -> t -> t -> t
  val binary_op : typ:Type.t -> op:string -> t -> t -> t
  val unary_op : typ:Type.t -> op:string -> t -> t
  val member : typ:Type.t -> string -> t -> string -> int -> t
end

module Decl : sig
  type t
  and desc =
    | Var of Var.t
    | Typedef of Type.t * string
    | Function of Type.t * func
  and func = {
    name : string;
    params : Var.local list;
  }

  val desc : t -> desc

  val var : Var.t -> t
  val typedef : typ:Type.t -> string -> t
  val function_ : typ:Type.t -> string -> Var.local list -> t
end

module Stmt : sig
  type t
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

  val desc : t -> desc

  val return : Expr.t -> t
  val if_ : Expr.t -> t -> t option -> t
  val while_ : Expr.t -> t -> t
  val do_ : t -> Expr.t -> t
  val for_ : t -> Expr.t option -> Expr.t option -> t -> t
  val switch : Expr.t -> t -> t
  val case : int -> t -> t
  val default : t -> t
  val break : t
  val continue : t
  val decl : Decl.t -> Expr.t option -> t
  val label : string -> int -> t -> t
  val goto : string -> int ref -> t
  val expr : Expr.t -> t
  val compound : t list -> t
  val null : t
end

module Transl_unit : sig
  type t = {
    definitions : definition list;
    str_literals : string list;
  }

  and definition =
    | Decl_only of Decl.t
    | Global_var of Var.global
    | Function of Decl.func * Stmt.t * Var.local list

  val decl_only : Decl.t -> definition
  val global_var : Var.global -> definition
  val function_ : Decl.func -> Stmt.t -> Var.local list -> definition
end
