type t = string list

let digit c = c >= '0' && c <= '9'

let normalize s =
  let rec first i =
    if i < String.length s - 1 && s.[i] = '0' then first (i + 1) else i
  in
  let i = first 0 in
  String.sub s i (String.length s - i)

let parse s =
  let parts = String.split_on_char '.' s in
  if List.for_all (fun p -> p <> "" && String.for_all digit p) parts then
    Some (List.map normalize parts)
  else None

let rec compare a b =
  match a, b with
  | [], [] -> 0
  | [], rest -> compare ["0"] rest
  | rest, [] -> compare rest ["0"]
  | x :: xs, y :: ys ->
      let c = Int.compare (String.length x) (String.length y) in
      let c = if c = 0 then String.compare x y else c in
      if c = 0 then compare xs ys else c
