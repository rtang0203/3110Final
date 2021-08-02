


open Lwt.Infix
open Sys

let server = 11271
let port = ref 11271
let start_state = ref (Obj.magic ())
let state = ref (Obj.magic ())
let interpreter = ref ""

let quit _ =
  let open Unix in
  let open Game_types.Game in
  let time = time () |> localtime in
  let file =
    Printf.sprintf
      "replay-%02i-%02i-%02i-%02i-%02i.json"
      time.tm_mon
      time.tm_mday
      time.tm_hour
      time.tm_min
      time.tm_sec
  in
  Export.export_to_file file !start_state !state.log;
  exit 0

let _ =
  Lwt_unix.on_signal Sys.sigint quit

let next_port () =
  port := !port + 1;
  !port

let rec main blue red =
  Io.bind ~initialize:(initialize blue red) ~set_up ~tear_down for_each server

and initialize blue red = fun _ ->
  let blue_port = next_port () in
  let red_port = next_port () in
  state := State.spawn !state blue_port Game_types.Team.Blue;
  state := State.spawn !state red_port Game_types.Team.Red;
  Io.spawn_parent
    ~interpreter:!interpreter
    ~server
    ~port:blue_port
    ~file:blue;
  Io.spawn_parent
    ~interpreter:!interpreter
    ~server
    ~port:red_port
    ~file:red;
  Lwt.return ()

and set_up port : unit Lwt.t =
  Lwt_io.printlf "Bot %i connected." port

and tear_down bot : unit Lwt.t =
  Lwt_io.printlf "Bot %i disconnected." bot

and for_each bot ic oc =
  begin match !state.winner with
  | Some team -> winner team; quit ()
  | None ->
    Lwt_io.read_line ic >>= fun message ->
    Lwt_io.printlf
      "%s\nTotal Moves: %i\nBlue Team Score: %i\nRed Team Score: %i\n\n"
      (State.to_debug_string !state)
      (State.get_total_moves !state)
      (State.get_score !state ~team:Game_types.Team.Blue)
      (State.get_score !state ~team:Game_types.Team.Red)
    >>= fun () -> respond bot message
    >>= function
    | None -> for_each bot ic oc
    | Some v ->
      v |> Serialize.string_of_value
        |> Lwt_io.write_line oc
        >>= fun () -> for_each bot ic oc
  end

and winner team =
  Printf.printf
    "%s\n\
    -------------------\n\
    %s won!\n\
    -------------------\n\
    Statistics:\n\
    -------------------\n\
    Total Moves: %i\n\
    Blue Team Score: %i\n\
    Blue Team Moves: %i\n\
    Red Team Score: %i\n\
    Red Team Moves: %i\n"
    (State.to_debug_string !state)
    (Game_types.Team.to_string team)
    (State.get_total_moves !state) 
    (State.get_score !state Game_types.Team.Blue)
    (State.get_moves !state Game_types.Team.Blue)
    (State.get_score !state Game_types.Team.Red)
    (State.get_moves !state Game_types.Team.Red)

and respond bot message =
  let open Types in
  match Serialize.value_of_string message with
  | VPair (VStr "move", VStr dir) -> respond_move bot dir
  | VPair (VStr "take", VStr dir) -> respond_take bot dir
  | VPair (VStr "look", VUnit) -> respond_look bot
  | VPair (VStr "info", VUnit) -> respond_info ()
  | VPair (VStr "inv", VUnit) -> respond_inv bot
  | VPair (VStr "spawn", VPair (f, a)) -> respond_spawn bot f a
  | _ -> Lwt.fail_with ("Invalid message " ^ message)

and respond_move bot dir =
  match Game_types.Dir.of_string dir with
  | None -> Lwt.return None
  | Some dir ->
    let state', cost = State.move !state bot dir in
    state := state';
    wait cost >|= fun () -> None

and respond_take bot dir =
  match Game_types.Dir.of_string dir with
  | None -> Lwt.return None
  | Some dir ->
    state := State.take !state bot dir;
    Lwt.return None

and wait cost =
  Lwt_unix.sleep (float_of_int cost *. 0.1)

and respond_look bot =
  let open Types in
  let around = State.look !state bot in
  let values = List.map (fun (p, t) -> VPair (value_of_pos p, value_of_tile t bot)) around in
  Lwt.return (Some (vlist_of_list values))

and vlist_of_list = function
  | [] -> VNil
  | h :: t -> VCons (h,vlist_of_list t)

and respond_info () =
  let open Types in
  let (w, h) = State.get_dim !state in
  let vr = State.get_vision !state in
  let rc = State.get_max_gold !state in
  let mb = State.get_max_bots !state in
  Lwt.return begin Some
    (VPair (VPair (VInt w, VInt h),
           (VPair (VInt vr, VPair (VInt rc, VInt mb)))))
  end

and respond_inv bot =
 let inv = State.get_inv !state bot in
 Lwt.return (Some (Types.VInt inv))

and value_of_pos p =
  let open Game_types.Pos in
  let open Types in
  VPair (VInt p.x, VInt p.y)

and value_of_tile t bot =
  let open Game_types.Tile in
  match t with
  | Gold g -> value_of_pair ("Gold", [g])
  | Base { team; bots = _ } -> value_of_pair ("Base", [value_of_team bot team])
  | Path { cost; bots = _ } -> value_of_pair ("Path", [cost])
  | Worm p_dest -> value_of_pair ("Wormhole", [p_dest.x; p_dest.y])
  | Wall -> value_of_pair ("Wall", [])

and value_of_pair (s, lst) =
  Types.(VPair (VStr s, lst |> List.map (fun n -> VInt n) |> vlist_of_list))

(* Team is 0 if not your's. If team is your's, team is 1. *)
and value_of_team bot team =
  let my_team = State.get_team !state bot in
  if team = my_team then 1 else 0

and respond_spawn bot f a =
  let team = State.get_team !state bot in
  let bots = State.get_count !state team in
  let max = State.get_max_bots !state in
  if bots = max then Lwt.return (Some Types.VUnit) else
  let port = next_port () in
  let hand = Types.VHandle port in
  state := State.spawn !state port team;
  Io.spawn_child
    ~interpreter:!interpreter
    ~server
    ~parent:bot
    ~child:port
    ~f:(Serialize.string_of_value f)
    ~a:(Serialize.string_of_value a);
  Lwt.return (Some hand)

let () =
  interpreter := Sys.argv.(1);
  start_state := State.make (Sys.argv.(2));
  state := !start_state;
  Lwt_main.run (main Sys.argv.(3) Sys.argv.(4))
