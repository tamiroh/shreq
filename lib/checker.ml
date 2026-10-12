type check_result = Satisfied | Unsatisfied of string | Unknown of string

type report = { requirement : Requirement.t; result : check_result; version : string option }

let probe path =
  let input, output = Unix.pipe ~cloexec:true () in
  let channel = Unix.in_channel_of_descr input in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      let null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0 in
      let pid =
        Fun.protect ~finally:(fun () -> Unix.close output; Unix.close null)
          (fun () -> Unix.create_process path [|path; "--version"|] null output null)
      in
      let first = ref None in
      (try while true do
         let line = input_line channel in
         if !first = None then first := Some line
       done with End_of_file -> ());
      let _, status = Unix.waitpid [] pid in
      match status, !first with
      | Unix.WEXITED 0, Some line -> Ok line
      | _ -> Error "version probe failed")

let extract kind line =
  let prefix = match kind with
    | Requirement.Bash -> "GNU bash, version "
    | _ -> "grep (GNU grep) "
  in
  if not (String.starts_with ~prefix line) then Error "implementation could not be identified"
  else
    let tail = String.sub line (String.length prefix) (String.length line - String.length prefix) in
    let token = match Requirement.words tail with word :: _ -> word | [] -> "" in
    let numeric =
      match kind, String.index_opt token '(' with
      | Requirement.Bash, Some index ->
          let suffix = String.sub token index (String.length token - index) in
          let valid_suffix =
            match String.index_opt suffix ')' with
            | Some close ->
                let build = String.sub suffix 1 (close - 1) in
                build <> "" && String.for_all Version.digit build
                && String.sub suffix (close + 1) (String.length suffix - close - 1) = "-release"
            | None -> false
          in
          if valid_suffix then String.sub token 0 index else token
      | _ -> token
    in
    match Version.parse numeric with
    | Some version -> Ok (numeric, version)
    | None -> Error "version could not be safely parsed"

let check requirement =
  let ( let* ) = Result.bind in
  let report ?version result = { requirement; result; version } in
  let unknown result = Result.map_error (fun message -> Unknown message) result in
  let evaluate () =
    let* command, needs_version =
      match requirement.Requirement.kind with
      | Requirement.Command name -> Ok (name, false)
      | Requirement.Bash -> Ok ("bash", requirement.minimum <> None)
      | Requirement.Gnu_grep -> Ok ("grep", true)
      | Requirement.Unsupported -> Error (Unknown "unsupported requirement")
    in
    let* path =
      Resolver.resolve command
      |> Option.to_result ~none:(Unsatisfied "command not found")
    in
    if not needs_version then Ok (report Satisfied)
    else
      let* line = probe path |> unknown in
      let* text, version = extract requirement.kind line |> unknown in
      let result =
        match requirement.minimum with
        | Some minimum when Version.compare version minimum < 0 ->
            Unsatisfied "version is too old"
        | _ -> Satisfied
      in
      Ok (report ~version:text result)
  in
  try
    match evaluate () with
    | Ok report -> report
    | Error result -> report result
  with Unix.Unix_error (error, _, _) -> report (Unknown (Unix.error_message error))

let format report =
  let r = report.requirement in
  let label = r.name
      ^ (match report.version with None -> "" | Some v -> " " ^ v)
      ^ (match r.minimum_text with None -> "" | Some v -> " (requires >= " ^ v ^ ")") in
  match report.result with
  | Satisfied -> "✓ " ^ label
  | Unsatisfied message -> "✗ " ^ label ^ ": " ^ message
  | Unknown message -> "? " ^ label ^ ": " ^ message
