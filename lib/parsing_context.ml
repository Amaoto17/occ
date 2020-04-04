open Ast

module Env = struct
  type 'a t = {
    mutable current_scope : (string, 'a) Hashtbl.t;
    mutable scope_stack : (string, 'a) Hashtbl.t list;
  }

  let create () = {
    current_scope = Hashtbl.create 16;
    scope_stack = [];
  }

  let push t =
    t.scope_stack <- t.current_scope :: t.scope_stack;
    t.current_scope <- Hashtbl.create 16

  let pop t =
    let (scope, rest) = match t.scope_stack with
      | [] -> failwith "outer environment is none"
      | top :: rest -> top, rest
    in
    t.current_scope <- scope;
    t.scope_stack <- rest

  let find ?(recursive=true) t key =
    let rec loop = function
      | [] -> None
      | tbl :: rest -> match Hashtbl.find_opt tbl key with
          | None when recursive -> loop rest
          | other -> other
    in
    loop @@ t.current_scope :: t.scope_stack

  let add t key v =
    Hashtbl.add t.current_scope key v
end

type t = {
  var_env : Var.t Env.t;
  typedef_env : Type.t Env.t;
  struct_tag_env : Type.t Env.t;
  (* label_generator : int Stream.t; *)
  labels : (string, int) Hashtbl.t;
  id_generator : int Stream.t;
  mutable unresolved_goto : Stmt.t list;
  mutable current_function : string option;
  mutable lvars : Var.local list;
  mutable string_literals : string list;
  mutable contflow_stack : [ `Loop | `Switch | `Loop_and_switch ] list;
}

let create () = {
  var_env = Env.create ();
  typedef_env = Env.create ();
  struct_tag_env = Env.create ();
  (* label_generator = Stream.from Option.some; *)
  labels = Hashtbl.create 8;
  id_generator = Stream.from Option.some;
  unresolved_goto = [];
  current_function = None;
  lvars = [];
  string_literals = [];
  contflow_stack = [];
}

let push_scope t =
  Env.push t.var_env;
  Env.push t.typedef_env;
  Env.push t.struct_tag_env

let pop_scope t =
  Env.pop t.var_env;
  Env.pop t.typedef_env;
  Env.pop t.struct_tag_env

(* let new_label t =
  Stream.next t.label_generator *)

let find_label t =
  Hashtbl.find_opt t.labels

let add_label t name = match find_label t name with
  | Some _ -> failwith "label is deprecated"
  | None ->
      let index = Hashtbl.length t.labels in
      Hashtbl.add t.labels name index; index

let new_id t =
  Stream.next t.id_generator

let add_lvar t var =
  t.lvars <- var :: t.lvars

let add_string_literal t lit =
  let len = List.length t.string_literals in
  t.string_literals <- lit :: t.string_literals; len

let add_unresolved_goto t node =
  t.unresolved_goto <- node :: t.unresolved_goto

let set_current_function t func_name =
  t.current_function <- Some func_name

let unset_current_function t =
  t.current_function <- None

let push_contflow t flag =
  let new_flag = match t.contflow_stack with
    | `Loop :: _ when flag = `Switch -> `Loop_and_switch
    | `Switch :: _ when flag = `Loop -> `Loop_and_switch
    | `Loop_and_switch :: _ -> `Loop_and_switch
    | _ -> flag
  in t.contflow_stack <- new_flag :: t.contflow_stack

let pop_contflow t =
  t.contflow_stack <- List.tl t.contflow_stack

let break_is_allowed t =
  List.length t.contflow_stack <> 0

let continue_is_allowed t = match t.contflow_stack with
  | [] | `Switch :: _ -> false
  | _ -> true

let is_inside_switch t = match t.contflow_stack with
  | [] | `Loop :: _ -> false
  | _ -> true
