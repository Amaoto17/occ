module Members : Map.S with type key = string

type t =
  | Void
  | Int
  | Char
  | Pointer of t
  | Array of t * int
  | Struct of (t * int) Members.t * int * int
  | Function of t

val pointer_to : t -> t
val array_of : t -> int -> t
val is_pointer : t -> bool
val size_of : t -> int
val align_of : t -> int
