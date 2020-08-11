open Curses
open Cmdliner

(*  applies f to each line in stdin.
    if f returns true then continue else stop. *)
let rec iter_stdin_lines (f: string -> bool) =
    let try_read_line () = 
        try Some (read_line ())
        with End_of_file -> None in
    match try_read_line () with
        | None -> ()
        | Some line -> if f line then (iter_stdin_lines [@tailcall]) f;;

let handle_input cmd_template replace_string verbose input =
    (* replace special pattern in command with input *)
    let cmd = Str.global_replace (Str.regexp replace_string) input cmd_template in
    (* show command then execute it *)
    let exec_cmd () =
        if verbose then ignore (addstr (cmd ^ "\n") && refresh ());
        Sys.command cmd == 0 in
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
            | 'a' -> false
            (* do nothing and continue to next input *)
            | 'n' -> true
            (* apply command on input, continue if command executed successfully *)
            | 'y' -> exec_cmd ()
            (* invalid keypress, try again *)
            | _ -> (handle_keypress [@tailcall]) () in
    (* show prompt and handle keypresses *)
    addstr prompt_str && refresh () && handle_keypress ();;

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
        if refresh () &&  exit_ca_mode () && noecho () then
            `Ok (iter_stdin_lines @@ handle_input cmd_template replace_string verbose)
        else
            `Error (false, "Something went wrong with curses."))
    else
        `Error (false, "Shell is not interactive.");;

let main =
    let doc = "execute command lines from standard input interactively" in
    Term.(ret (const flmgr $ cmd_template $ replace_string $ verbose)),
    Term.info "flmgr" ~doc;;

let () = Term.(exit @@ eval main);;
