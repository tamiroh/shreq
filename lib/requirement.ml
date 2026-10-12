type kind = Command of string | Bash | Gnu_grep | Unsupported

type t = {
  name : string;
  kind : kind;
  minimum : Version.t option;
  minimum_text : string option;
  line : int;
}

let command_name s =
  s <> "" && s <> "." && s <> ".."
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | '+' -> true
         | _ -> false)
       s

let words s =
  String.split_on_char ' ' (String.map (function '\t' -> ' ' | c -> c) s)
  |> List.filter (fun s -> s <> "")

let parse ~line text =
  let tokens = words text in
  let rec split names = function
    | [] -> Ok (List.rev names, None)
    | ">=" :: [v] ->
        (match Version.parse v with
         | Some version -> Ok (List.rev names, Some (v, version))
         | None -> Error "invalid version (expected dotted non-negative integers)")
    | token :: rest when command_name token -> split (token :: names) rest
    | _ -> Error "invalid declaration (only >= is supported)"
  in
  match split [] tokens with
  | Error _ as error -> error
  | Ok ([], _) -> Error "empty requirement"
  | Ok (names, minimum) ->
      let name = String.concat " " names in
      let kind =
        match names, minimum with
        | ["Bash"], _ -> Bash
        | ["GNU"; "grep"], _ -> Gnu_grep
        | [command], None -> Command command
        | _ -> Unsupported
      in
      Ok { name; kind; minimum = Option.map snd minimum;
           minimum_text = Option.map fst minimum; line }

let key r =
  match r.kind with
  | Bash -> "bash"
  | Gnu_grep -> "grep"
  | Command name -> name
  | Unsupported ->
      (match List.rev (words r.name) with name :: _ -> name | [] -> r.name)
