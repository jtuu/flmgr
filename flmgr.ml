open Curses

(*  applies f to each line in stdin.
    if f returns true then continue else stop. *)
let rec iter_stdin_lines (f: string -> bool) =
    let try_read_line () = 
        try Some (read_line ())
        with End_of_file -> None in
    match try_read_line () with
        | None -> ()
        | Some line -> if f line then (iter_stdin_lines [@tailcall]) f;;

let handle_input cmd_template input =
    (* replace special pattern in command with input *)
    let cmd = Str.global_replace (Str.regexp "{}") input cmd_template in
    (* show command then execute it *)
    let exec_cmd () = ignore (addstr (cmd ^ "\n") && refresh ()); (Sys.command cmd) == 0 in
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

let screen = newterm "xterm" Unix.stdin Unix.stdout;;
refresh ();;
exit_ca_mode ();;
noecho ();;
iter_stdin_lines (handle_input "echo {}");;

(*  TODO: take command template from argument.
    TODO: separate "check" command (e.g. feh) and "work" command (e.g. mv). *)
