let parse_channel channel =
  let seen = Hashtbl.create 8 in
  let rec loop line requirements =
    match input_line channel with
    | text ->
        let text = String.trim text in
        if text = "" then loop (line + 1) requirements
        else if text.[0] <> '#' then finish requirements
        else
          let comment = String.trim (String.sub text 1 (String.length text - 1)) in
          let prefix = "Requires:" in
          if String.starts_with ~prefix comment then
            let body = String.sub comment (String.length prefix)
                (String.length comment - String.length prefix) |> String.trim in
            (match Requirement.parse ~line body with
             | Error message -> Error (Printf.sprintf "line %d: %s" line message)
             | Ok requirement ->
                 let key = Requirement.key requirement in
                 if Hashtbl.mem seen key then
                   Error (Printf.sprintf "line %d: duplicate requirement: %s" line requirement.name)
                 else begin
                   Hashtbl.add seen key ();
                   loop (line + 1) (requirement :: requirements)
                 end)
          else loop (line + 1) requirements
    | exception End_of_file -> finish requirements
  and finish = function
    | [] -> Error "no Requires declarations found"
    | requirements -> Ok (List.rev requirements)
  in
  loop 1 []

let parse_file path =
  try
    let channel = open_in path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel)
      (fun () -> parse_channel channel)
  with Sys_error message -> Error message
