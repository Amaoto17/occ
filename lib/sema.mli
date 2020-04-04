exception Semantic_error of string

open Ast
open Parsing_context

val int_literal : t -> string -> Expr.t
val string_literal : t -> string -> Expr.t
val char_literal : t -> char -> Expr.t
val ident_expr : t -> string -> Expr.t
val function_call : t -> Expr.t -> Expr.t list -> Expr.t
val deref_expr : t -> Expr.t -> Expr.t
val binary_expr : t -> string -> Expr.t -> Expr.t -> Expr.t
val subscript_expr : t -> Expr.t -> Expr.t -> Expr.t
val postfix_incdec_expr : t -> Expr.t -> string -> Expr.t
val member_expr : t -> Expr.t -> string -> string -> Expr.t
val prefix_incdec_expr : t -> string -> Expr.t -> Expr.t
val neg_expr : t -> Expr.t -> Expr.t
val bit_not_expr : t -> Expr.t -> Expr.t
val logical_not_expr : t -> Expr.t -> Expr.t
val addr_expr : t -> Expr.t -> Expr.t
val sizeof_expr : t -> Expr.t -> Expr.t
val conditional_expr : t -> Expr.t -> Expr.t -> Expr.t -> Expr.t
val const_expr : t -> Expr.t -> int

val struct_decl : t -> string option -> Decl.t list option -> Type.t
val variable_decl : t -> string -> Type.t -> Decl.t

val compound_stmt_enter : t -> unit
val compound_stmt_leave : t -> Stmt.t list -> Stmt.t
val return_stmt : t -> Expr.t -> Stmt.t
val if_stmt : t -> Expr.t -> Stmt.t -> Stmt.t option -> Stmt.t
val while_stmt_enter : t -> unit
val while_stmt_leave : t -> Expr.t -> Stmt.t -> Stmt.t
val do_stmt_enter : t -> unit
val do_stmt_leave : t -> Stmt.t -> Expr.t -> Stmt.t
val for_stmt_enter : t -> unit
val for_stmt_leave : t -> Stmt.t -> Expr.t option -> Expr.t option -> Stmt.t -> Stmt.t
val switch_stmt_enter : t -> unit
val switch_stmt_leave : t -> Expr.t -> Stmt.t -> Stmt.t
val case_stmt : t -> int -> Stmt.t -> Stmt.t
val default_stmt : t -> Stmt.t -> Stmt.t
val break_stmt : t -> Stmt.t
val continue_stmt : t -> Stmt.t
val goto_stmt : t -> string -> Stmt.t
val labeled_stmt : t -> string -> Stmt.t -> Stmt.t
val decl_stmt : t -> Decl.t -> Expr.t option -> Stmt.t
val null_stmt : t -> Stmt.t
val expr_stmt : t -> Expr.t -> Stmt.t

val function_params_enter : t -> unit
val function_params_leave : t -> string -> Type.t -> Decl.t list -> Transl_unit.definition
val function_decl_enter : t -> Transl_unit.definition -> unit
val function_decl_leave : t -> Transl_unit.definition -> Stmt.t -> Transl_unit.definition
val toplevel_typedef : t -> Type.t -> string -> Transl_unit.definition
val global_variable : t -> Type.t -> string -> Transl_unit.definition
val transl_unit : t -> Transl_unit.definition list -> Transl_unit.t
