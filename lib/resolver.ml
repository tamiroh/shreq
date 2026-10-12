let executable path =
  try
    (Unix.stat path).Unix.st_kind = Unix.S_REG
    && (Unix.access path [Unix.X_OK]; true)
  with Unix.Unix_error _ -> false

let resolve command =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path ->
      String.split_on_char ':' path
      |> List.find_map (fun directory ->
          let directory = if directory = "" then "." else directory in
          let path = Filename.concat directory command in
          if executable path then Some path else None)
