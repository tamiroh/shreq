let usage = "Usage: shreq SCRIPT\nCheck Requires declarations against the environment at CLI startup."

let run file =
  match Shreq.Parser.parse_file file with
  | Error message -> prerr_endline ("Error: " ^ message); 2
  | Ok requirements ->
      let reports = List.map Shreq.Checker.check requirements in
      List.iter (fun report -> print_endline (Shreq.Checker.format report)) reports;
      if List.for_all (fun report -> report.Shreq.Checker.result = Shreq.Checker.Satisfied) reports
      then 0 else 1

let () =
  let code = match Array.to_list Sys.argv with
    | [_; ("--help" | "-h")] -> print_endline usage; 0
    | [_; "--"; file] -> run file
    | [_; file] when not (String.starts_with ~prefix:"-" file) -> run file
    | _ -> prerr_endline usage; 2
  in
  exit code
