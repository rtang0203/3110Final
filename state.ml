open Game_types
open Game_types.Game
open Yojson.Basic

(** [default_cycle_cost] is the normal number of cycles
  * required to traverse non-Path terrain. *)
let default_cycle_cost = 5

(** [make f] makes the state with the map from file [f] *)
let make filename =
  let (width, height, max_gold, grid, vision, max_bots, end_goal, max_moves) =
    Import.import_from_file filename in
  let score =
    TeamMap.empty |> TeamMap.add Team.Blue 0 |> TeamMap.add Team.Red 0
  in
  let moves = score in
  { score;
    bots = BotMap.empty;
    width;
    height;
    max_gold;
    vision;
    max_bots;
    grid;
    log = [];
    end_goal;
    winner = None;
    max_moves;
    moves; }

(** [get_team s bot] is the team associated with [bot] in state [s]. *)
let get_team s ~bot =
  s.bots |> BotMap.find bot |> fun bot -> bot.team

(** [get_inv s bot] is the gold inventory associated with [bot] in state [s]. *)
let get_inv s ~bot =
  s.bots |> BotMap.find bot |> fun bot -> bot.inv

(** [get_pos s bot] is the position associated with [bot] in state [s]. *)
let get_pos s ~bot =
  s.bots |> BotMap.find bot |> fun bot -> bot.pos

(** [get_count s team] is the number of bots on [team] in state [s]. *)
let get_count s ~team =
  BotMap.filter (fun _ bot -> bot.team = team) s.bots |> BotMap.cardinal

(** [get_score s team] is the score of [team] in state [s]. *)
let get_score s ~team =
  TeamMap.find team s.score

(** [get_moves s team] is the number of MOVE commands that
  * [team] has issued in state [s]. *)
let get_moves s ~team =
  TeamMap.find team s.moves

(** [get_total_moves s] is the total number of MOVE commands issued in state [s]. *)
let get_total_moves s =
  TeamMap.fold (fun _ m acc -> m + acc) s.moves 0

let get_max_gold s = s.max_gold

let get_max_bots s = s.max_bots

let get_dim s = (s.width, s.height)

let get_vision s = s.vision

(** [get_base s team] is the position of the base of [team] in state [s]. *)
let get_base s ~team =
  let select = function
  | (_, Tile.Base { team = team'; bots = _ }) when team = team' -> true
  | _ -> false
  in
  s.grid
    |> PosMap.bindings
    |> List.find select
    |> fst

(** [set_score s team score] is a new state with the score of [team] set to [score]. *)
let set_score s ~team ~score =
  if (score >= s.end_goal) then
    { s with score = TeamMap.add team score s.score; winner = Some team; }
  else
    { s with score = TeamMap.add team score s.score }

(** [set_moves s team moves] is a new state with the moves of [team] set to [moves]. *)
let set_moves s ~team ~moves =
  let moves' = TeamMap.add team moves s.moves in
  let total = TeamMap.fold (fun _ m acc -> m + acc) moves' 0 in
  if total > s.max_moves then
    let highest, _ =
      TeamMap.fold
        (fun b g (b', g') -> if g > g' then (b, g) else (b', g'))
        s.score
        (Team.Blue, -1)
    in
    { s with moves = moves'; winner = Some highest }
  else
    { s with moves = moves'; }

(** [set_inv s bot inv] is a new state with the inventory of [bot] set to [inv]. *)
let set_inv s ~bot ~inv =
  let bot' = BotMap.find bot s.bots in
  { s with bots = BotMap.add bot { bot' with inv } s.bots }

(** [set_pos s bot pos] is a new state with the position of [bot] set to [pos]. *)
let set_pos s ~bot ~pos =
  let bot' = BotMap.find bot s.bots in
  { s with bots = BotMap.add bot { bot' with pos } s.bots }

(** [set_tile s p tile] is state [s] with [tile] at position [p]. *)
let set_tile s ~pos ~tile =
  { s with grid = PosMap.add pos tile s.grid }

(** [add_bot s bot team] is the new state after adding new [bot] to [team] in state [s]. *)
let add_bot s ~bot ~pos ~team =
  { s with bots = BotMap.add bot { inv = 0; pos; team } s.bots }

(** [log_move s json] is the new state after adding move [json] to state [s]'s log. *)
let log_move s ~json =
  { s with log = json :: s.log }

(** [move s bot dir] is the new state after moving [bot] in [dir] in state [s]. *)
let rec move s ~bot ~dir =
  if not (s.winner = None) then (s, default_cycle_cost)
  else
    let pos = get_pos s ~bot in
    let pos' = Pos.shift pos dir in
    if not (valid_pos s pos') then
      (s, default_cycle_cost)
    else
      move_rec s ~bot ~dir ~pos ~pos'

(** [move_rec s bot dir p p'] recursively moves [bot] with initial direction [dir]
  * from point [p] to [p']. *)
and move_rec s ~bot ~dir ~pos ~pos' =
  let t = get_team s bot in
  match PosMap.find pos' s.grid with
  | Tile.Base { team; bots = _ } when team = t ->
    s |> remove_bot_tile ~bot ~pos
      |> add_bot_tile ~bot ~pos:pos'
      |> set_score ~team ~score:(get_score s t + get_inv s bot)
      |> set_inv ~bot ~inv:0
      |> set_pos ~bot ~pos:pos'
      |> set_moves ~team:t ~moves:(get_moves s t + 1)
      |> log_move ~json:(Export.json_of_move bot t dir)
      |> fun s' -> (s', default_cycle_cost)
  | Tile.Base _ ->
    s |> remove_bot_tile ~bot ~pos
      |> add_bot_tile ~bot ~pos:pos'
      |> set_pos ~bot ~pos:pos'
      |> set_moves ~team:t ~moves:(get_moves s t + 1)
      |> log_move ~json:(Export.json_of_move bot t dir)
      |> fun s' -> (s', default_cycle_cost)
  | Tile.Path { cost; bots = _ } ->
    s |> remove_bot_tile ~bot ~pos
      |> add_bot_tile ~bot ~pos:pos'
      |> set_pos ~bot ~pos:pos'
      |> set_moves ~team:t ~moves:(get_moves s t + 1)
      |> log_move ~json:(Export.json_of_move bot t dir)
      |> fun s' -> (s', cost)
  | Tile.Gold _
  | Tile.Wall ->
    (s, default_cycle_cost)
  | Tile.Worm p' ->
    move_rec s ~bot ~dir ~pos ~pos'

(** [add_bot_tile s bot p] adds [bot] to passable tile at point [p]. *)
and add_bot_tile s ~bot ~pos =
  let team = get_team s bot in
  map_passable s ~pos ~f:(fun bots -> team :: bots)

(** [remove_bot_tile s bot p] removes [bot] from passable tile at point [p]. *)
and remove_bot_tile s ~bot ~pos =
  let team = get_team s bot in
  map_passable s ~pos ~f:(fun bots -> remove_first bots team)

(** [map_passable s p f] is the new state after applying [f] to
  * the bot list of passable tile [p] in state [s]. *)
and map_passable s ~pos ~f =
  match PosMap.find pos s.grid with
  | Tile.Path { cost; bots } -> set_tile s ~pos ~tile:(Tile.Path { cost; bots = f bots })
  | Tile.Base { team; bots } -> set_tile s ~pos ~tile:(Tile.Base { team; bots = f bots })
  | _ -> failwith "[INTERNAL ERROR]: impassable terrain"

(** [spawn s bot team] is the new state after spawning [bot] at the base of [team]. *)
and spawn s bot team =
  if not (s.winner = None) then s else
  let pos = get_base s ~team in
  s |> add_bot ~bot ~pos ~team
    |> add_bot_tile ~bot ~pos
    |> log_move ~json:(Export.json_of_spawn bot team)

(** [take s bot dir] is the new state after removing gold in [dir] relative to [bot].
  * Attempts to remove gold up to maximum bot capacity. Replaces gold tiles with
  * default path tiles once they reach 0 gold. Does nothing if the target tile is not gold. *)
and take s bot dir =
  if not (s.winner = None) then s else
  let pos = get_pos s ~bot in
  let pos' = Pos.shift pos dir in
  if not (valid_pos s pos') then s else
  let inv = get_inv s ~bot in
  let team = get_team s ~bot in
  match PosMap.find pos' s.grid with
  | Tile.Gold g when g + inv <= s.max_gold ->
    s |> set_tile ~pos:pos' ~tile:(Tile.Path { cost = 10; bots = [] })
      |> set_inv ~bot ~inv:(inv + g)
      |> log_move ~json:(Export.json_of_take bot team dir g)
  | Tile.Gold g ->
    s |> set_tile ~pos:pos' ~tile:(Tile.Gold (g + inv - s.max_gold))
      |> set_inv ~bot ~inv:(s.max_gold)
      |> log_move ~json:(Export.json_of_take bot team dir (s.max_gold - inv))
  | _ -> s

(** [look s bot] is an association list [(pos, tile); ...] of tiles
  * within the vision radius of [bot] in state [s].
  * The first element of the list is always the tile the bot is on. *)
and look s bot =
  let p = get_pos s bot in
  Pos.around p s.vision
  |> reorder_around p
  |> List.filter (fun p' -> valid_pos s p')
  |> List.map (fun p' -> (p', PosMap.find p' s.grid))

(* Reorders tile list such that tile that current bot is on is always first. *)
and reorder_around p lst =
  p :: (List.filter ((<>) p) lst)

(** [valid_pos s p] is [true] if [p] is within bounds of state [s], or [false] otherwise. *)
and valid_pos s p =
  let open Pos in
  p.x >= 0 && p.x < s.width &&
  p.y >= 0 && p.y < s.height

(** [remove_first l item] is [l] with the first instance of [item] removed.
  * Has no effect if [item] does not appear in [l]. *)
and remove_first l item =
  let rec remove_first' l acc =
    match l with
    | [] -> List.rev acc
    | h :: t -> if h = item then (List.rev acc) @ t else remove_first' t (h :: acc)
  in
  remove_first' l []

(** [to_debug_string s] is an ASCII version of state [s] suitable for basic debugging. *)
and to_debug_string s =
  Pos.rectangle s.width s.height
  |> List.map (List.map (fun p -> PosMap.find p s.grid))
  |> List.map (List.map Tile.to_string)
  |> List.map (String.concat "")
  |> String.concat "\n"
