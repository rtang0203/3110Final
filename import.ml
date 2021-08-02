open Game_types

(** [import_from_file file] is the OCaml representation
  * of a map created from the online level editor.
  *)
let rec import_from_file filename =
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_file filename in
  let width = json |> member "width" |> to_int in
  let height = json |> member "height" |> to_int in
  let max_gold = json |> member "max_gold" |> to_int in
  let vision = json |> member "vision" |> to_int in
  let max_bots = json |> member "max_bots" |> to_int in
  let end_goal = json |> member "end_goal" |> to_int in
  let max_moves = json |> member "max_moves" |> to_int in
  let grid = generate_grid (json |> member "map" |> to_list) width height in
  width, height, max_gold, grid, vision, max_bots, end_goal, max_moves

(** [generate_grid json w h] is the 2D grid corresponding to a list of
  * tiles stored in [json], with width [w] and height [h].
  *)
and generate_grid json w h =
  let open Pos in
  let open Tile in
  Pos.rectangle w h
  |> List.flatten
  |> List.fold_left 
     begin fun g p ->
       let t = tile_of_json (List.nth json (p.x * h + p.y)) in
       PosMap.add p t g
     end
     PosMap.empty

(** [tile_of_json json] is the tile corresponding to [json]. *)
and tile_of_json json =
  let open Tile in
  let open Pos in
  let open Yojson.Basic.Util in
  let typ = json |> member "type" |> Yojson.Basic.Util.to_string in
  begin match typ with
    | "P" ->
      let cost = json |> member "cost" |> to_int in
      Path {cost = cost; bots = []}
    | "G" ->
      let amt = json |> member "amount" |> to_int in
      Gold amt
    | "B" ->
      let team = json |> member "team" |> to_string in
      if (team = "Blue") then Base {team = Team.Blue; bots = []}
      else Base {team = Team.Red; bots = []}
    | "W" -> Wall
    | "Worm" ->
      let out = json |> member "out" |> to_list in
      let outx = List.nth out 0 |> to_int in
      let outy = List.nth out 1 |> to_int in
      Worm {x = outx; y = outy}
    | _ -> failwith "unexpected tile type"
  end
