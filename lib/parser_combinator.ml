open Printf

module Stream = struct
  type t = {
    source : string;
    offset : int;
  }

  let create source = { source; offset = 0 }

  let offset t = t.offset

  let set_offset t offset = { t with offset }

  let source t = t.source

  let uncons t len =
    if offset t + len > String.length (source t) then None
    else
      let str = String.sub (source t) (offset t) len in
      let offset = offset t + len in
      Some (str, { t with offset })

  let uncons1 t =
    if offset t >= String.length (source t) then None
    else
      let c = (source t).[offset t] in
      let offset = offset t + 1 in
      Some (c, { t with offset })
end

module Error = struct
  type message =
    | Expected of string
    | Unexpected of string
    | Problem of string

  type t = {
    err_pos : int;
    messages : message list;
  }

  let empty err_pos = { err_pos; messages = [] }

  let unexpected msg err_pos = { err_pos; messages = [Unexpected msg] }

  let expected msg err_pos = { err_pos; messages = [Expected msg] }

  let problem msg err_pos = { err_pos; messages = [Problem msg] }

  let message t =
    let buf = Buffer.create 32 in
    let f = function
      | Expected msg -> Buffer.add_string buf (sprintf "expected %s\n" msg)
      | Unexpected msg -> Buffer.add_string buf (sprintf "unexpected %s\n" msg)
      | Problem msg -> Buffer.add_string buf (sprintf "problem %s\n" msg)
    in List.iter f t.messages; Buffer.contents buf
end

type 'a parse_result = ('a * Stream.t * Error.t, Error.t) result

type 'a t = Stream.t -> 'a parse_result

let parse_string p str =
  let st = Stream.create str in
  match p st with
  | Ok (x, _, _) -> Ok x
  | Error err -> Error (Error.message err)

let return x st = Ok (x, st, Error.empty (Stream.offset st))

let bind p f st =
  Result.bind (p st) @@ fun (x, st, _) -> (f x) st

let (>>=) = bind

let ( *>) p q = p >>= fun _ -> q

let (<*) p q = p >>= fun x -> q *> return x

let (<$>) f p = p >>= fun x -> return (f x)

let (<*>) p q = p >>= fun f -> f <$> q

let (>>|) p f = f <$> p

let lift2 f p q = f <$> p <*> q

let lift3 f p q r = f <$> p <*> q <*> r

let lift4 f p q r s = f <$> p <*> q <*> r <*> s

let emptyerr st = Error.empty (Stream.offset st)

let get st = Ok (st, st, emptyerr st)

let put st _ = Ok ((), st, emptyerr st)

let expected msg st = Error (Error.expected msg (Stream.offset st))

let unexpected msg st = Error (Error.unexpected msg (Stream.offset st))

let problem msg st = Error (Error.problem msg (Stream.offset st))

let empty st = Error (emptyerr st)

let (<|>) p q st = match p st with
  | Ok v -> Ok v
  | Error _ -> q st

let satisfy pred st = match Stream.uncons1 st with
  | None -> Error (Error.unexpected "end of input" (Stream.offset st))
  | Some (c, st) when pred c -> Ok (c, st, emptyerr st)
  | Some (c, st) -> Error (Error.unexpected (sprintf "%C" c) (Stream.offset st))

let string str st = match Stream.uncons st (String.length str) with
  | None -> Error (Error.unexpected "end of input" (Stream.offset st))
  | Some (s, st) when s = str -> Ok (s, st, emptyerr st)
  | Some (s, st) -> Error (Error.expected (sprintf "%S" s) (Stream.offset st))

let fix f =
  let rec p = lazy (f r)
  and r st = (Lazy.force p) st in r

let many p = fix (fun many ->
  (lift2 List.cons p many) <|> return [])

let many1 p =
  lift2 List.cons p (many p)

let skip_many p = fix (fun skip_many ->
  (p *> skip_many) <|> return ())

let skip_many1 p = p *> skip_many p

let not_followed_by pred st =
  match Stream.uncons1 st with
  | None -> Ok ((), st, emptyerr st)
  | Some (c, st) when pred c -> Error (Error.unexpected (sprintf "%C" c) (Stream.offset st))
  | Some (_, _) -> Ok ((), st, emptyerr st)

let any = satisfy (Fun.const true)

let end_of_input = not_followed_by (Fun.const true)

let choice ps = List.fold_left (<|>) empty ps

let option p = (Option.some <$> p) <|> return None

let chainl p op =
  let rec loop acc =
    (lift2 (fun f x -> f acc x) op p >>= loop) <|> return acc
  in p >>= loop

let chainr p op =
  let rec loop acc =
    (lift2 (fun f x -> f acc x) op (p >>= loop)) <|> return acc
  in p >>= loop

let char c = satisfy ((=) c)

let not_char c = satisfy ((<>) c)

let take_while1 pred =
  many1 (satisfy pred)
    >>| List.map (fun c -> sprintf "%c" c)
    >>| String.concat ""

let take_while pred =
  many (satisfy pred)
    >>| List.map (fun c -> sprintf "%c" c)
    >>| String.concat ""

let skip_while1 pred = skip_many1 (satisfy pred)

let skip_while pred = skip_many (satisfy pred)

let take_till pred =
  many (satisfy (Fun.negate pred))
    >>| List.map (fun c -> sprintf "%c" c)
    >>| String.concat ""

let skip_till pred =
  skip_many (satisfy (Fun.negate pred))

let sep_by sep p =
  (lift2 List.cons p (many (sep *> p))) <|> return []

let fail = problem
