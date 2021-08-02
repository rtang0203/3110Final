
open Dwt.Infix

type mode =
| File of string
| Json

type args = {
  port: int;
  parent: int;
  server: int;
  input: mode;
}

(** Main entry point. *)
let rec main () =
  try
  let args = parse_args () in
  Random.init args.port;
  Io.init args.port args.server;
  let for_each _ _ _ = Lwt.return () in
  let () = Lwt.async (fun () -> Io.bind for_each args.port) in
  let eval_loop = eval args.port args.server args.input |> loop in
  Lwt_main.run (Dwt.danger_danger_if_you_call_this_function_you_will_get_a_zero_on_a8 (eval_loop))
  with _ -> ()
  (* Don't worry, that ^ only applies if you call it in another file *)

(** Main loop. *)
and loop = function
| Types.VDwt lwt -> lwt >>= loop
| v -> Dwt.return ()

(** Main loop. *)
and eval self server mode =
  let env = [("SELF", Types.VHandle self); ("SERVER", Types.VHandle server)] in
  match mode with
  | File f -> eval_file env f
  | Json ->
    let f = read_line () |> Serialize.value_of_string in
    let a = read_line () |> Serialize.value_of_string in
    eval_json env f a

(** [eval_file env f] evaluates the file [f], with preprocessing.
 *
 * @param env The evaluation enviroment.
 * @param f The filename.
 * @return The result value of the evaluation.
 *)
and eval_file env f =
  let path = Filename.dirname f in
  let pre = preprocess path (read_file f) in
  eval_string env pre

(** [eval_string env s] evaluates the string [s] with the environment
 *  [env].
 *
 * @param env The evaluation enviroment.
 * @param str A string to evaluate.
 * @return The result value of the evaluation.
 *)
and eval_string env s =
  let buf = Lexing.from_string s in
  try buf
      |> Parser.program Lexer.token
      |> Eval.eval env
  with
  | Parser.Error ->
    failwith begin
      let s = Lexing.lexeme_start_p buf in
      let e = Lexing.lexeme_end_p buf in
      Printf.sprintf
        "Parse error at %i:%i - %i:%i"
        s.pos_lnum
        (s.pos_cnum - s.pos_bol)
        e.pos_lnum
        (e.pos_cnum - e.pos_bol)
    end

(** [read_file f] is a utility function to read the entirety
 *  of the file with name [f].
 *  @param f The filename.
 *  @return The string content of the file. *)
and read_file f =
  let channel = open_in f in
  let length = in_channel_length channel in
  let content = really_input_string channel length in
  close_in channel;
  content

(** [preprocess path raw] preprocesses a raw string.
 *  Currently, this handles all "include" commands.
 *  @param path The base path.
 *  @param raw The string to preprocess.
 *  @return Preprocessed string. *)
and preprocess path raw =
  let include_re = Str.regexp "include[ \t]+\"\\([^ \t\r\n\"]+\\)\"" in
  let extract s =
    let file' = Str.matched_group 1 s in
    let path' = Filename.concat path (Filename.dirname file') in
    file' |> Filename.concat path
          |> read_file
          |> preprocess path'
  in
  Str.global_substitute include_re extract raw

(** [eval_json env f a] evaluates a program in JSON mode (from [spawn]).
 *  @param env The evaluation environment.
 *  @param f A function.
 *  @param a The argument to start the function with.
 *  @return Evaluation result. *)
and eval_json env f a =
  let open Types in
  match f with
  | VFun (p, b, env') -> eval_app (env @ env') p a b
  | VFunRec (p, b, env'_ref) -> eval_app (env @ !env'_ref) p a b
  | _ -> failwith "[PROGRAM ERROR]: can only spawn with function"

(** [eval_app env p v b] binds and evaluates a pattern.
 *  @param env The evaluation environment.
 *  @param p Pattern to bind.
 *  @param v Value.
 *  @param b The expression.
 *  @return The return value. *)
and eval_app env p v b =
  match Eval.bind_pat p v with
  | Some env' -> Eval.eval (env' @ env) b
  | None -> failwith "Let expression doesn't match pattern"

and parse_args () =
  let port = ref 10000 in
  let parent = ref 0 in
  let server = ref 0 in
  let file = ref "" in
  let json = ref false in
  let () = Arg.parse
    [ ("--port", Arg.Set_int port, "Robot's port");
      ("--server", Arg.Set_int server, "Server port");
      ("--parent", Arg.Set_int parent, "Parent port");
      ("--file", Arg.Set_string file, "Input RML file");
      ("--json", Arg.Set json, "Expect JSON via stdin") ]
    ignore
    usage
  in
  if !port = 0 then quit () |> ignore;
  if !server <> 0 then Io.connect !server |> ignore;
  let input = match !file, !json with
  | file, false when file <> "" -> File file
  | "", true -> Json
  | _ -> quit ()
  in
  { port = !port;
    parent = !parent;
    server = !server;
    input }

and quit () =
  print_endline usage;
  exit 1

and usage =
  Printf.sprintf
    "%s\n%s\n"
    "Usage: ./rml_interpreter.exe --port <PORT> --parent <PORT> --server <PORT> --file <FILE>"
    "     | ./rml_interpreter.exe --port <PORT> --parent <PORT> --server <PORT> --json <EXP> <ARG>"

let () = main ()
