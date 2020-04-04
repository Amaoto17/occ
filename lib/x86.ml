open Insn

let emitln ?(indent=4) fmt =
  fmt |> Printf.ksprintf (fun str ->
    print_string (String.make indent ' ');
    print_endline str)

let emit_label fmt = emitln ~indent:0 fmt

let emit_rodata rodata =
  let id = ref 0 in
  if List.length rodata > 0 then emitln ".data";
  ListLabels.iter rodata ~f:(function
    | Str str ->
        emit_label ".LC%d:" !id; incr id;
        emitln ".string \"%s\"" str
    | _ -> failwith "")

let argregs = ["%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9"]

let rec emit_args ?(regs=argregs) ?(k=(fun _ -> ())) n =
  match n, regs with
  | 0, _ -> k ()
  | n, reg :: regs -> emit_args ~regs ~k:(fun _ -> k (); emitln "pop %s" reg) (n - 1)
  | _ -> failwith "emit_args"

let rec emit_params ?(regs=argregs) ?(k=(fun _ -> ())) offsets =
  match offsets, regs with
  | [], _ -> k ()
  | offset :: rest, reg :: regs ->
      emit_params ~regs ~k:(fun _ -> k (); emitln "mov %s, -%d(%%rbp)" reg offset) rest
  | _, [] -> failwith "emit_params"

let emit_cmp = function
  | Setl -> emitln "setl %%al"
  | Setle -> emitln "setle %%al"
  | Setg -> emitln "setg %%al"
  | Setge -> emitln "setge %%al"
  | Sete -> emitln "sete %%al"
  | Setne -> emitln "setne %%al"

let emit_insn = function
  | Push i ->
      emitln "push $%d" i
  | Offset i ->
      emitln "pop %%rax";
      emitln "add $%d, %%rax" i;
      emitln "push %%rax"
  | Pop ->
      emitln "pop %%rax"
  | Add ->
      emitln "pop %%rdx";
      emitln "pop %%rax";
      emitln "add %%rdx, %%rax";
      emitln "push %%rax"
  | Sub ->
      emitln "pop %%rdx";
      emitln "pop %%rax";
      emitln "sub %%rdx, %%rax";
      emitln "push %%rax"
  | Mul ->
      emitln "pop %%rcx";
      emitln "pop %%rax";
      emitln "imul %%rcx";
      emitln "push %%rax"
  | Div ->
      emitln "pop %%rcx";
      emitln "pop %%rax";
      emitln "cqo";
      emitln "idiv %%rcx";
      emitln "push %%rax"
  | Mod ->
      emitln "pop %%rcx";
      emitln "pop %%rax";
      emitln "cqo";
      emitln "idiv %%rcx";
      emitln "push %%rdx"
  | Asl ->
      emitln "pop %%rcx";
      emitln "pop %%rax";
      emitln "sal %%cl, %%rax"
  | Asr ->
      emitln "pop %%rcx";
      emitln "pop %%rax";
      emitln "sar %%cl, %%rax"
  | Cmp cmp ->
      emitln "pop %%rdx";
      emitln "pop %%rax";
      emitln "cmp %%rdx, %%rax";
      emit_cmp cmp;
      emitln "movzbq %%al, %%rax";
      emitln "push %%rax"
  | Neg ->
      emitln "pop %%rax";
      emitln "neg %%rax";
      emitln "push %%rax"
  | Bit_not ->
      emitln "pop %%rax";
      emitln "not %%rax";
      emitln "push %%rax"
  | Bit_and ->
      emitln "pop %%rdx";
      emitln "pop %%rax";
      emitln "and %%rdx, %%rax";
      emitln "push %%rax"
  | Bit_or ->
      emitln "pop %%rdx";
      emitln "pop %%rax";
      emitln "or %%rdx, %%rax";
      emitln "push %%rax"
  | Bit_xor ->
      emitln "pop %%rdx";
      emitln "pop %%rax";
      emitln "xor %%rdx, %%rax";
      emitln "push %%rax"
  | Logical_not ->
      emitln "pop %%rax";
      emitln "cmp $0, %%rax";
      emitln "sete %%al";
      emitln "movzbq %%al, %%rax";
      emitln "push %%rax"
  | Comma ->
      emitln "pop %%rax";
      emitln "pop %%rdx"
  | Prefix_inc ->
      emitln "pop %%rax";
      emitln "mov (%%rax), %%rdx";
      emitln "inc %%rdx";
      emitln "mov %%rdx, (%%rax)";
      emitln "push %%rdx"
  | Prefix_dec ->
      emitln "pop %%rax";
      emitln "mov (%%rax), %%rdx";
      emitln "dec %%rdx";
      emitln "mov %%rdx, (%%rax)";
      emitln "push %%rdx"
  | Postfix_inc ->
      emitln "pop %%rax";
      emitln "mov (%%rax), %%rdx";
      emitln "push %%rdx";
      emitln "inc %%rdx";
      emitln "mov %%rdx, (%%rax)"
  | Postfix_dec ->
      emitln "pop %%rax";
      emitln "mov (%%rax), %%rdx";
      emitln "push %%rdx";
      emitln "dec %%rdx";
      emitln "mov %%rdx, (%%rax)"
  | Assign ->
      emitln "pop %%rdx";
      emitln "pop %%rax";
      emitln "mov %%rdx, (%%rax)";
      emitln "push %%rdx"
  | Branch target ->
      emitln "pop %%rax";
      emitln "cmp $0, %%rax";
      emitln "je .L%d" target
  | Jmp target ->
      emitln "jmp .L%d" target
  | Get_local_addr offset ->
      emitln "lea -%d(%%rbp), %%rax" offset;
      emitln "push %%rax"
  | Get_global_addr name ->
      emitln "lea %s(%%rip), %%rax" name;
      emitln "push %%rax"
  | Get_string index ->
      emitln "lea .LC%d(%%rip), %%rax" index;
      emitln "push %%rax"
  | Load_addr 1 ->
      emitln "pop %%rax";
      emitln "movb (%%rax), %%al";
      emitln "movzbq %%al, %%rax";
      emitln "push %%rax"
  | Load_addr _ ->
      emitln "pop %%rax";
      emitln "mov (%%rax), %%rax";
      emitln "push %%rax"
  | Get_global name ->
      emitln "push %s(%%rip)" name
  | Label i ->
      emit_label ".L%d:" i
  | User_defined_label i ->
      emit_label ".LL%d:" i
  | Case_label i ->
      emit_label ".Lcase%d:" i
  | Return target ->
      emitln "pop %%rax";
      emitln "jmp .L%d" target
  | Switch ->
      emitln "pop %%r10"
  | Case (label, value) ->
      emitln "cmp $%d, %%r10" value;
      emitln "je .L%d" label
  | Call (name, nargs) ->
      emit_args nargs;
      emitln "push %%r10";
      emitln "push %%r11";
      emitln "xor %%rax, %%rax";
      emitln "call _%s" name;
      emitln "pop %%r11";
      emitln "pop %%r10";
      emitln "push %%rax"
  | Load_params offsets ->
      emit_params offsets
  | _ -> failwith "TODO"

let emit_prologue framesize =
  emitln "push %%rbp";
  emitln "mov %%rsp, %%rbp";
  emitln "sub $%d, %%rsp" framesize

let emit_epilogue return_target =
  emit_label ".L%d:" return_target;
  emitln "leave";
  emitln "ret"

let emit_text text =
  emitln ".text";
  ListLabels.iter text ~f:(function
    Function (name, return_target, framesize, insn_seq) ->
      emitln ".global _%s" name;
      emit_label "_%s:" name;
      emit_prologue framesize;
      List.iter emit_insn insn_seq;
      emit_epilogue return_target)

let emit transl_unit =
  emit_rodata transl_unit.rodata;
  emit_text transl_unit.text
