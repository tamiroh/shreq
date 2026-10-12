open Shreq

let write path text =
  let channel = open_out path in
  Fun.protect ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel text)

let read path =
  let channel = open_in path in
  Fun.protect ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in loop 0

let rec remove path =
  if Sys.is_directory path then begin
    Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
    Unix.rmdir path
  end else Sys.remove path

let () =
  let executable = Sys.argv.(1) in
  let root = Filename.temp_file "shreq-test-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove root) (fun () ->
      let a = Filename.concat root "a" and b = Filename.concat root "b" in
      Unix.mkdir a 0o700; Unix.mkdir b 0o700;
      let stub directory name output status =
        let path = Filename.concat directory name in
        write path (Printf.sprintf "#!/bin/sh\n[ \"$#\" -eq 1 ] && [ \"$1\" = --version ] || exit 99\nprintf '%%s\\n' '%s'\nexit %d\n" output status);
        Unix.chmod path 0o700
      in
      stub a "grep" "grep (GNU grep) 3.11" 0;
      stub b "grep" "grep (BSD grep) 2.6" 0;
      stub a "bash" "GNU bash, version 5.2.37(1)-release (test)" 0;
      let script = Filename.concat root "script.sh" in
      let stdout = Filename.concat root "stdout" and stderr = Filename.concat root "stderr" in
      let count = ref 0 in
      let invoke ~path args =
        let out = Unix.openfile stdout [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
        let err = Unix.openfile stderr [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
        let env = [|"PATH=" ^ path; "LC_ALL=C"|] in
        let pid = Fun.protect ~finally:(fun () -> Unix.close out; Unix.close err)
            (fun () -> Unix.create_process_env executable
                (Array.of_list (executable :: args)) env Unix.stdin out err) in
        let _, status = Unix.waitpid [] pid in
        let code = match status with Unix.WEXITED code -> code | _ -> -1 in
        code, read stdout, read stderr
      in
      let case ?(path=a) name body expected fragment =
        incr count;
        write script body;
        let code, out, err = invoke ~path [script] in
        if code <> expected || not (contains (out ^ err) fragment)
           || (if expected = 2 then out <> "" else err <> "") then
          failwith (Printf.sprintf "%s: expected %d, got %d\nstdout: %s\nstderr: %s" name expected code out err)
      in
      case "plain command" "# Requires: grep\n" 0 "✓ grep";
      case "missing command" "# Requires: missing-command\n" 1 "command not found";
      case "no declarations" "#!/bin/sh\necho hello\n" 2 "no Requires declarations found";
      case "empty" "# Requires:\n" 2 "line 1";
      case "bad operator" "# Requires: Bash > 5.2\n" 2 "line 1";
      case "bad version" "# Requires: Bash >= 5..2\n" 2 "invalid version";
      case "GNU implementation" "# Requires: GNU grep\n" 0 "GNU grep 3.11";
      case ~path:b "BSD implementation" "# Requires: GNU grep\n" 1 "? GNU grep";
      case "old" "# Requires: GNU grep >= 3.12\n" 1 "too old";
      case "equal" "# Requires: GNU grep >= 3.11\n" 0 "✓";
      case "newer" "# Requires: GNU grep >= 3.9\n" 0 "✓";
      List.iter (fun name -> case name ("# Requires: " ^ name ^ "\n") 1 "unsupported requirement")
        ["POSIX sh"; "BSD grep"; "GNU sed"; "jq >= 1.7"];
      case "duplicate" "# Requires: grep\n# Requires: grep\n" 2 "line 2: duplicate";
      case "duplicate implementation" "# Requires: grep\n# Requires: GNU grep\n" 2 "duplicate";
      case "body ignored" "# Requires: grep\necho hi\n# Requires:\n" 0 "✓";
      case "body only" "echo hi\n# Requires: grep\n" 2 "no Requires";
      case "case sensitive" "# requires: grep\n" 2 "no Requires";
      case "header whitespace" "#!/bin/sh\r\n \t\r\n # Description: test\r\n\t# Requires:\tgrep\r\n" 0 "✓";
      case ~path:(b ^ ":" ^ a) "BSD first" "# Requires: GNU grep\n" 1 "?";
      case ~path:(a ^ ":" ^ b) "GNU first" "# Requires: GNU grep\n" 0 "✓";
      stub a "ggrep" "grep (GNU grep) 3.11" 0;
      case ~path:b "no ggrep fallback" "# Requires: GNU grep\n" 1 "?";
      case "bash presence" "# Requires: Bash\n" 0 "✓ Bash";
      case "bash version" "# Requires: Bash >= 5.2\n" 0 "Bash 5.2.37";
      case "bash old" "# Requires: Bash >= 5.10\n" 1 "too old";
      stub a "bash" "GNU bash, version 5.3.0(1)-rc1" 0;
      case "bash prerelease" "# Requires: Bash >= 5.2\n" 1 "safely parsed";
      stub a "grep" "grep (GNU grep) 3.12-rc1" 0;
      case "grep prerelease" "# Requires: GNU grep >= 3.11\n" 1 "safely parsed";
      stub a "grep" "grep (GNU grep) 3.11" 2;
      case "probe failure" "# Requires: GNU grep\n" 1 "probe failed";
      Unix.chmod (Filename.concat a "grep") 0o600;
      case "not executable" "# Requires: grep\n" 1 "command not found";
      Unix.mkdir (Filename.concat a "directory") 0o700;
      case "directory" "# Requires: directory\n" 1 "command not found";
      stub a "grep" "grep (GNU grep) 3.11" 0;
      let marker = Filename.concat root "executed" in
      case "never execute script" ("# Requires: grep\nprintf bad > " ^ marker ^ "\n") 0 "✓";
      assert (not (Sys.file_exists marker));
      let code, _, _ = invoke ~path:a [] in assert (code = 2);
      let code, _, _ = invoke ~path:a [Filename.concat root "absent"] in assert (code = 2);
      let code, out, err = invoke ~path:a ["--help"] in
      assert (code = 0 && contains out "Usage:" && err = "");
      let version s = Option.get (Version.parse s) in
      List.iter (fun (a, b, expected) ->
          let actual = Version.compare (version a) (version b) in
          assert (Int.compare actual 0 = expected))
        ["5.2", "5.10", -1; "3.11", "3.9", 1; "5.2.1", "5.2", 1;
         "5.2", "5.2.0", 0; "0.0", "0", 0; "0005.02", "5.2", 0;
         "999999999999999999999999999999999", "5", 1];
      Printf.printf "Passed %d CLI cases, argument/file checks, and version comparisons.\n" !count)
