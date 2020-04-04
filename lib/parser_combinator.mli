module Stream : sig
  type t
  val create : string -> t
  val offset : t -> int
  val set_offset : t -> int -> t
  val source : t -> string
  val uncons : t -> int -> (string * t) option
  val uncons1 : t -> (char * t) option
end

module Error : sig
  type t
  val empty : int -> t
  val unexpected : string -> int -> t
  val expected : string -> int -> t
  val problem : string -> int -> t
  val message : t -> string
end

type 'a parse_result
type 'a t = Stream.t -> 'a parse_result

val parse_string : 'a t -> string -> ('a, string) result
val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val ( *>) : _ t -> 'a t -> 'a t
val (<*) : 'a t -> _ t -> 'a t
val (<$>) : ('a -> 'b) -> 'a t -> 'b t
val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
val (>>|) : 'a t -> ('a -> 'b) -> 'b t
val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
val get : Stream.t t
val put : Stream.t -> unit t
val expected : string -> 'a t
val unexpected : string -> 'a t
val problem : string -> 'a t
val (<|>) : 'a t -> 'a t -> 'a t
val satisfy : (char -> bool) -> char t
val string : string -> string t
val fix : ('a t -> 'a t) -> 'a t
val many : 'a t -> 'a list t
val many1 : 'a t -> 'a list t
val skip_many : _ t -> unit t
val skip_many1 : _ t -> unit t
val not_followed_by : (char -> bool) -> unit t
val any : char t
val end_of_input : unit t
val choice : 'a t list -> 'a t
val option : 'a t -> 'a option t
val chainl : 'a t -> ('a -> 'a -> 'a) t -> 'a t
val chainr : 'a t -> ('a -> 'a -> 'a) t -> 'a t
val char : char -> char t
val not_char : char -> char t
val take_while : (char -> bool) -> string t
val take_while1 : (char -> bool) -> string t
val skip_while : (char -> bool) -> unit t
val skip_while1 : (char -> bool) -> unit t
val take_till : (char -> bool) -> string t
val skip_till : (char -> bool) -> unit t
val sep_by : _ t -> 'a t -> 'a list t
val fail : string -> 'a t
