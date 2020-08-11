open Curses
open Cmdliner

(*  applies f to each line in stdin.
    if f returns Ok then continue else stop. *)
let rec iter_stdin_lines f =
    let try_read_line () = 
        try Some (read_line ())
        with End_of_file -> None in
    let result = (match try_read_line () with
        | None -> `Ok ()
        | Some line -> f line) in
    match result with
        | `Ok () -> (iter_stdin_lines [@tailcall]) f
        | _ -> result;;

let handle_input cmd_template replace_string verbose input =
    (* replace special pattern in command with input *)
    let cmd = Str.global_replace (Str.regexp replace_string) input cmd_template in
    (* show command then execute it *)
    let exec_cmd () =
        if verbose then ignore (addstr (cmd ^ "\n") && refresh ());
        Sys.command cmd in
    (* string to show on prompt *)
    let prompt_str = Printf.sprintf "%s: Execute command '%s'? (y/n/a) " input cmd in
    let rec handle_keypress () =
        (* get keypress *)
        let ch = Char.chr (getch ()) in
        (* print keypress if it's valid *)
        ignore (match ch with
            | 'a' | 'n' | 'y' -> addstr (Printf.sprintf "%c\n" ch) && refresh ()
            | _ -> false);
        match ch with
            (* abort *)
            | 'a' -> `Error(false, "Aborted")
            (* do nothing and continue to next input *)
            | 'n' -> `Ok ()
            (* apply command on input, continue if command executed successfully *)
            | 'y' -> (match exec_cmd () with
                        | 0 -> `Ok ()
                        | code -> `Error (false, Printf.sprintf "Aborted (%d)" code))
            (* invalid keypress, try again *)
            | _ -> (handle_keypress [@tailcall]) () in
    (* return carriage in case command output caused it to move *)
    Printf.printf "\r";
    (* show prompt and handle keypresses *)
    if addstr prompt_str && refresh () then
        handle_keypress ()
    else
        `Error (false, "Curses error.")

(*  curses will clear the screen on init by default, this disables that.
    not really sure if this is the right way to do this. *)
let exit_ca_mode () = tputs "\x1b[r\x1b[?1049l" 0 (print_char);;

(* command line arguments *)
let verbose = 
    let doc = "Print commands before executing them." in
    Arg.(value & flag & info ["v"] ~doc);;

let replace_string = 
    let doc = "Occurrences of $(i,REPLACE_STR) will be replaced in $(i,CMD) with lines read from stdin." in
    Arg.(value & opt string "{}" & info ["I"] ~docv:"REPLACE_STR" ~doc);;

let cmd_template =
    let doc = "The command to execute for each line of input. Execution is stopped if the command returns an exit code other than 0." in
    Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"CMD" ~doc);;

let flmgr cmd_template replace_string verbose =
    if Unix.isatty Unix.stdout then (
        ignore @@ newterm "xterm" Unix.stdin Unix.stdout;
        if refresh () && exit_ca_mode () && noecho () then
            iter_stdin_lines @@ handle_input cmd_template replace_string verbose
        else
            `Error (false, "Curses error."))
    else
        `Error (false, "Shell is not interactive.");;

let main =
    let doc = "execute command lines from standard input interactively" in
    Term.(ret (const flmgr $ cmd_template $ replace_string $ verbose)),
    Term.info "flmgr" ~doc;;

let () = Term.(exit @@ eval main);;
