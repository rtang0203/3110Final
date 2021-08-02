open Lwt.Infix

type rx = Lwt_io.input Lwt_io.channel
type tx = Lwt_io.output Lwt_io.channel

module Bots = Map.Make (struct type t = int let compare = Pervasives.compare end)

(** Connected servers. *)
let bots: (rx * tx) Bots.t ref = ref Bots.empty

(** TODO: hardcode? *)
let server: int ref = ref 0
let self: int ref = ref 0

let init self_port server_port =
  server := server_port;
  self := self_port

(** Bind to [port] and accept incoming connections.
 *
  * [initialize port] is run after the listening port is bound.
  * [setup port] is run once initially when each connection is accepted.
  * [for_each port ic oc] is run on each accepted connection.
  * [tear_down port] is run once when a connection is closed.
  *)
let rec bind
  ?initialize:(initialize = no_op)
  ?set_up:(set_up = no_op)
  ?tear_down:(tear_down = no_op)
  for_each
  port
: unit Lwt.t =
  let file, sock = open_port port in
  let () = Lwt_unix.setsockopt file Lwt_unix.SO_REUSEADDR true in
  Lwt_unix.bind file sock
  >|= (fun () -> Lwt_unix.listen file 100)
  >>= (fun () -> begin initialize port >>= fun () -> loop file set_up for_each tear_down end)

(** [no_op] is a trivial dummy function for [initialize], [set_up], and [tear_down]. *)
and no_op _ = Lwt.return ()

(** [loop file] spawns new handlers for each incoming connection on file descriptor [file].
  *
  * See [bind] for description of [set_up], [for_each], and [tear_down].
  *)
and loop file set_up for_each tear_down =
  Lwt_unix.accept file >>= function (file', _) ->
  let ic, oc = open_file file' in
  let port = Lwt_main.run (Lwt_io.read_line ic) |> int_of_string in
  bots := Bots.add port (ic, oc) !bots;
  set_up port >>= fun () ->
  Lwt.async begin fun () ->
    Lwt.catch (fun () -> for_each port ic oc)
              (fun _ -> disconnect port; tear_down port)
  end;
  loop file set_up for_each tear_down

(** [connect port] finds an existing connection to the server listening on [port],
  * or mutably establishes one if no such connection exists. Returns a promise for the
  * [(ic, oc)] channels for communicating with the server.
  *
  * Note that [connect] immediately sends a registration message with this server's
  * own listening port. This is required to have a single canonical port associated
  * with each server, instead of one for passive and one for active connections.
  *)
and connect port =
  match Bots.find_opt port !bots with
  | Some (ic, oc) ->
    Lwt.return (ic, oc)
  | None ->
    let file, sock = open_port port in
    Lwt_unix.connect file sock >>= fun () ->
    let ic, oc = open_file file in
    bots := Bots.add port (ic, oc) !bots;
    Lwt_io.write_line oc (string_of_int !self) >|= fun () ->
    (ic, oc)

(** [disconnect port] mutably removes the channels associated with the server listening
  * on [port], if they exist. *)
and disconnect port =
  bots := Bots.remove port !bots

(** [open_port port] is the [(file, sock)] file descriptor and socket address associated with [port]. *)
and open_port port =
  let file = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let sock = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  file, sock

(** [open_file file] is a [(ic, oc)] tuple of channels for communicating across file descriptor [file]. *)
and open_file file =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input file in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output file in
  ic, oc

(** [spawn_child interpreter server parent child f a] starts child [interpreter]
  * running closure [f] applied to argument [a] and listening on port [child],
  * which connects to parent interpreter listening on port [parent] and
  * server interpreter listening on port [server]. *)
and spawn_child ~interpreter ~server ~parent ~child ~f ~a =
  let (in_descr, out_descr) = Unix.pipe () in
  let oc = Unix.out_channel_of_descr out_descr in
  let _ = Unix.create_process
    interpreter
    [| "rml_interpreter.exe";
      "--server";
      string_of_int server;
      "--port"; 
      string_of_int child;
      "--parent";
        string_of_int parent;
      "--json"; |]
    in_descr
    Unix.stdout
    Unix.stderr
  in
  Printf.fprintf oc "%s\n" f;
  Printf.fprintf oc "%s\n" a;
  flush oc;
  close_out oc

(** [spawn_parent interpreter server port file] starts [interpreter] running
  * [file] listening on [port], which connects to server interpreter
  * listening on port [server]. *)
and spawn_parent ~interpreter ~server ~port ~file =
  Unix.create_process
    interpreter
    [| "rml_interpreter.exe";
      "--server";
      string_of_int server;
      "--port"; 
      string_of_int port;
      "--parent"; 
      "0";
      "--file";
      file; |]
    Unix.stdin
    Unix.stdout
    Unix.stderr
  |> ignore
