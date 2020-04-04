open Lib

let compile path =
  let source_file = open_in path in
  let buf = Bytes.create 4096 in
  let len = input source_file buf 0 @@ Bytes.length buf in
  let input_flat_string = Bytes.sub buf 0 len |> Bytes.to_string in
  let ast = Parser.run input_flat_string in
  ast |> Compiler.run |> X86.emit

let usage () =
  prerr_endline "usage: occ <file>";
  exit 1

let () =
  let argc = Array.length Sys.argv in
  match argc with
  | 2 -> compile Sys.argv.(1)
  | _ -> usage ()
