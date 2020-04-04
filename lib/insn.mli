type t =
  | Push of int
  | Offset of int
  | Pop
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Asl
  | Asr
  | Cmp of cmp
  | Neg
  | Bit_not
  | Bit_and
  | Bit_or
  | Bit_xor
  | Call of string * int
  | Logical_not
  | Logical_and
  | Logical_or
  | Assign
  | Comma
  | Prefix_inc
  | Prefix_dec
  | Postfix_inc
  | Postfix_dec
  | Branch of int
  | Jmp of int
  | Get_local_addr of int
  | Get_global_addr of string
  | Get_string of int
  | Load_addr of int
  | Get_global of string
  | Label of int
  | User_defined_label of int
  | Case_label of int
  | Return of int
  | Switch
  | Case of int * int
  | Load_params of int list

and cmp = Setl | Setle | Setg | Setge | Sete | Setne

type data = Str of string | Bytes of int

type text = Function of string * int * int * t list

type transl_unit = {
  data : data list;
  rodata : data list;
  bss : data list;
  text : text list;
}

module Printer : sig
  val run : transl_unit -> string
end
