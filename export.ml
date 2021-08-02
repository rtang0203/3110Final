open Game_types

(** [export_to_file filename state moves] writes the JSON representation of
  * [state] after sequence of [moves] to [filename]. *)
let rec export_to_file filename state moves =
  let open Game_types.Game in
  let map_data = json_of_grid state.grid in
  let meta_data =
    [ ("width", `Int state.width);
      ("height", `Int state.height);
      ("max_gold", `Int state.max_gold);
      ("max_bots", `Int state.max_bots);
      ("teams", json_of_scores state.score);
      ("moves", `List (List.rev moves));
      ("vision", `Int state.vision);
      ("end_goal", `Int state.end_goal);
      ("max_moves", `Int state.max_moves);    ]
  in
  let json = `Assoc (meta_data @ map_data) in
  Yojson.Basic.to_file filename json

(** [json_of_grid map] converts [map] into a JSON list of tiles. *)
and json_of_grid map =
  let open Yojson.Basic in
  map |> PosMap.bindings
      |> List.map snd
      |> List.map json_of_tile
      |> fun l -> [("map", `List l)]

(** [json_of_tile] is the JSON representation of a tile. *)
and json_of_tile tile =
  let open Yojson.Basic in
  let open Tile in
  begin match tile with
    | Path p ->
      `Assoc [("type", `String "P"); ("cost", `Int p.cost)]
    | Gold g ->
      `Assoc [("type", `String "G"); ("amount", `Int g)]
    | Base b ->
      `Assoc [("type", `String "B");
              ("team", `String (Team.to_string b.team))]
    | Wall ->
      `Assoc [("type", `String "W");]
    | Worm w ->
      `Assoc [("type", `String "Worm");
              ("out", `List [`Int w.x; `Int w.y])]
  end

(** [json_of_scores scores] is the JSON representation of scoreboard [scores]. *)
and json_of_scores scores =
  `List (TeamMap.fold (fun k d acc -> `String (Team.to_string k) :: acc) scores [])

(** [json_of_spawn bot team] is the JSON representation of a spawn move. *)
and json_of_spawn bot team =
  `Assoc [ ("handle", `Int bot);
           ("team", `String (Team.to_string team));
           ("command", `String "Spawn") ]

(** [json_of_move bot team dir] is the JSON representation of a movement move. *)
and json_of_move bot team dir =
  `Assoc [ ("handle", `Int bot);
           ("team", `String (Team.to_string team));
           ("command", `String "Move");
           ("direction", `String (Dir.to_string dir)) ]

(** [json_of_take bot team dir] is the JSON representation of a take move. *)
and json_of_take bot team dir amt =
  `Assoc [ ("handle", `Int bot);
           ("team", `String (Team.to_string team));
           ("command", `String "Take");
           ("direction", `String (Dir.to_string dir));
           ("amount", `Int amt); ]
