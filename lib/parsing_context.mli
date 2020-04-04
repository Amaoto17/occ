open Ast

module Env : sig
  type 'a t
  val create : unit -> 'a t
  val push : 'a t -> unit
  val pop : 'a t -> unit
  val find : ?recursive:bool -> 'a t -> string -> 'a option
  val add : 'a t -> string -> 'a -> unit
end

type t = {
  var_env : Var.t Env.t;
  typedef_env : Type.t Env.t;
  struct_tag_env : Type.t Env.t;
  labels : (string, int) Hashtbl.t;
  id_generator : int Stream.t;
  mutable unresolved_goto : Stmt.t list;
  mutable current_function : string option;
  mutable lvars : Var.local list;
  mutable string_literals : string list;
  mutable contflow_stack : [ `Loop | `Switch | `Loop_and_switch ] list;
}

val create : unit -> t
val push_scope : t -> unit
val pop_scope : t -> unit
val find_label : t -> string -> int option
val add_label : t -> string -> int
val new_id : t -> int
val add_lvar : t -> Var.local -> unit
val add_string_literal : t -> string -> int
val add_unresolved_goto : t -> Stmt.t -> unit
val set_current_function : t -> string -> unit
val unset_current_function : t -> unit
val push_contflow : t -> [ `Loop | `Switch | `Loop_and_switch ] -> unit
val pop_contflow : t -> unit
val break_is_allowed : t -> bool
val continue_is_allowed : t -> bool
val is_inside_switch : t -> bool
