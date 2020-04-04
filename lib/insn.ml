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

module Printer = struct
  let dumpln buf fmt =
    fmt |> Printf.ksprintf (fun str ->
      Buffer.add_string buf str;
      Buffer.add_char buf '\n')

  let dump_data buf data =
    ListLabels.iter data ~f:(function
      | Str str -> dumpln buf "STRING %s" str
      | Bytes size -> dumpln buf "BYTES %d" size)

  let dump_insn buf = function
    | Push i -> dumpln buf "PUSH %d" i
    | Offset i -> dumpln buf "OFFSET %d" i
    | Pop -> dumpln buf "POP"
    | Add -> dumpln buf "ADD"
    | Sub -> dumpln buf "SUB"
    | Mul -> dumpln buf "MUL"
    | Div -> dumpln buf "DIV"
    | Mod -> dumpln buf "MOD"
    | Asl -> dumpln buf "ASL"
    | Asr -> dumpln buf "ASR"
    | Cmp Setl -> dumpln buf "SETL"
    | Cmp Setle -> dumpln buf "SETLE"
    | Cmp Setg -> dumpln buf "SETG"
    | Cmp Setge -> dumpln buf "SETGE"
    | Cmp Sete -> dumpln buf "SETE"
    | Cmp Setne -> dumpln buf "SETNE"
    | Neg -> dumpln buf "NEG"
    | Bit_not -> dumpln buf "BITNOT"
    | Bit_and -> dumpln buf "BITAND"
    | Bit_or -> dumpln buf "BITOR"
    | Bit_xor -> dumpln buf "BITXOR"
    | Call (name, nargs) -> dumpln buf "CALL %s, %d" name nargs
    | Logical_not -> dumpln buf "LOGNOT"
    | Logical_and -> dumpln buf "LOGAND"
    | Logical_or -> dumpln buf "LOGOR"
    | Assign -> dumpln buf "ASSIGN"
    | Comma -> dumpln buf "COMMA"
    | Prefix_inc -> dumpln buf "PREFIX_INC"
    | Prefix_dec -> dumpln buf "PREFIX_DEC"
    | Postfix_inc -> dumpln buf "POSTFIX_INC"
    | Postfix_dec -> dumpln buf "POSTFIX_DEC"
    | Branch target -> dumpln buf "BRANCH %d" target
    | Jmp target -> dumpln buf "JMP %d" target
    | Get_local_addr offset -> dumpln buf "GET_LOCAL_ADDR %d" offset
    | Get_global_addr name -> dumpln buf "GET_GLOBAL_ADDR %s" name
    | Get_string index -> dumpln buf "GET_STRING %d" index
    | Load_addr size -> dumpln buf "LOAD_ADDR %d" size
    | Get_global name -> dumpln buf "GET_GLOBAL %s" name
    | Label label -> dumpln buf "LABEL %d" label
    | User_defined_label label -> dumpln buf "USER_DEFINED_LABEL %d" label
    | Case_label label -> dumpln buf "CASE_LABEL %d" label
    | Return target -> dumpln buf "RETURN %d" target
    | Switch -> dumpln buf "SWITCH"
    | Case (target, const) -> dumpln buf "CASE %d, %d" target const 
    | Load_params _offsets -> dumpln buf "LOAD_PARAMS"

  let dump_text buf text =
    ListLabels.iter text ~f:(function
      Function (name, _, framesize, insn_seq) ->
        dumpln buf "FUNCTION %s, %d" name framesize;
        List.iter (dump_insn buf) insn_seq)

  let run transl_unit =
    let buf = Buffer.create 256 in
    dump_data buf transl_unit.data;
    dump_data buf transl_unit.rodata;
    dump_data buf transl_unit.bss;
    dump_text buf transl_unit.text;
    Buffer.contents buf
end
