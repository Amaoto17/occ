module Members = Map.Make(String)

type t =
  | Void
  | Int
  | Char
  | Pointer of t
  | Array of t * int
  | Struct of (t * int) Members.t * int * int
  | Function of t

let pointer_to t = Pointer t
let array_of t len = Array (t, len)

let is_pointer = function Pointer _ -> true | _ -> false

let rec size_of = function
  | Void -> 0
  | Int -> 8
  | Char -> 1
  | Pointer _ -> 8
  | Array (t, len) -> len * size_of t
  | Struct (_, size, _) -> size
  | _ -> failwith "size_of: this type doesn't have size"

let rec align_of = function
  | Void -> 0
  | Int -> 8
  | Char -> 1
  | Pointer _ -> 8
  | Array (t, _) -> align_of t
  | Struct (_, _, align) -> align
  | _ -> failwith "align_of: this type doesn't have alignment"
