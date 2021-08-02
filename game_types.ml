module Team = struct

  (** Represents a robot team. *)
  type t =
  | Blue
  | Red
  [@@deriving yojson]

  let to_string t =
    match t with
    | Blue -> "Blue"
    | Red -> "Red"

  let compare = compare
end

module TeamMap = Map.Make (Team)

module Bot = struct

  (** Represents a unique robot ID. *)
  type t = int
  [@@deriving yojson]

  let compare = compare
end

module BotMap = Map.Make (Bot)

module Dir = struct

  (** Represents a relative direction. *)
  type t =
  | N
  | S
  | E
  | W
  | NE
  | NW
  | SE
  | SW

  (* [of_string s] is the direction corresponding to string [s].
   * @raises [Failure "[INTERNAL ERROR]: invalid direction"] if [s] is not valid.
   *)
  let of_string = function
  | "N" -> Some N
  | "S" -> Some S
  | "E" -> Some E
  | "W" -> Some W
  | "NE" -> Some NE
  | "NW" -> Some NW
  | "SE" -> Some SE
  | "SW" -> Some SW
  | _ -> failwith "[INTERNAL ERROR]: invalid direction"

  (* [to_string d] is a server-compatible string representation of direction [d]. *)
  let to_string = function
  | N -> "N"
  | S -> "S"
  | E -> "E"
  | W -> "W"
  | NE -> "NE"
  | NW -> "NW"
  | SE -> "SE"
  | SW -> "SW"
end

module Pos = struct

  (** Represents a 2D coordinate. *)
  type t = {
    x: int;
    y: int;
  } [@@deriving yojson]

  (** [compare a b] is [-1] if point [a] is lexicographically before
   * point [b], [0] if they are equal, and [1] if point [a] is after.
   *)
  let compare a b =
    let xs = compare a.x b.x in
    if xs = 0 then compare a.y b.y else xs

  (** [around p r] is a list of 2D points within distance [r] of point [p].
    * Does not include point [p] itself.
    *)
  let rec around p r =
    range (-r) (r + 1)
    |> List.map (fun dy -> List.map (fun dx -> { x = p.x + dx; y = p.y + dy }) (range (-r) (r + 1)))
    |> List.flatten
    |> List.filter (fun p' -> p <> p')

  (** [distance a b] is the number of moves required to travel from point [a]
    * to point [b], allowing for diagonal movement.
    *)
  and distance a b =
    max (abs (a.x - b.x)) (abs (a.y - b.y))

  (** [rectangle width height] is the list of points [(0, 0); ...; (width - 1, height -1)]. *)
  and rectangle width height =
    range 0 height |> List.map (fun y -> List.map (fun x -> { x; y }) (range 0 width))

  (** [range i j] is the list of integers [i; ...; j - 1]. *)
  and range i j =
    let rec range' i j acc =
      if i >= j then acc else range' (i + 1) j (i :: acc)
    in
    List.rev (range' i j [])

  (** [shift p d] is the point [p'] after taking a step in direction [d] from point [p]. *)
  let shift p = function
  | Dir.N -> { p with y = p.y - 1 }
  | Dir.S -> { p with y = p.y + 1 }
  | Dir.E -> { p with x = p.x + 1 }
  | Dir.W -> { p with x = p.x - 1 }
  | Dir.NE -> { x = p.x + 1; y = p.y - 1 }
  | Dir.NW -> { x = p.x - 1; y = p.y - 1 }
  | Dir.SE -> { x = p.x + 1; y = p.y + 1 }
  | Dir.SW -> { x = p.x - 1; y = p.y + 1 }
end

module PosMap = Map.Make (Pos)

module Tile = struct

  (** Represents terrain on the game grid. *)
  type t =
  | Gold of int
  | Base of
    { team: Team.t;
      bots: Team.t list; }
  | Path of
    { cost: int;
      bots: Team.t list; }
  | Wall
  | Worm of Pos.t
  [@@deriving yojson]

  (** [to_string t] is a single-character representation of tile [t],
    * suitable for basic ASCII visualization and debugging.
    *)
  let to_string = function
  | Wall -> "#"
  | Path { cost = _; bots = [] } -> " "
  | Base { team = Team.Blue; bots = [] } -> "B"
  | Base { team = Team.Red; bots = [] } -> "R"
  | Path { cost = _; bots = Team.Blue :: _ }
  | Base { team = _; bots = Team.Blue :: _ } -> "b"
  | Path { cost = _; bots = Team.Red :: _ }
  | Base { team = _; bots = Team.Red :: _ } -> "r"
  | Gold g ->
    assert (g < 10 && g > 0);
    string_of_int g
  | Worm _ -> "W"

  (** [of_char c] is a tile [t] corresponding to the character,
    * suitable for creating basic maps.
    *)
  let of_char = function
  | '#' -> Wall
  | ' ' -> Path { cost = 10; bots = [] }
  | 'B' -> Base { team = Team.Blue; bots = [] }
  | 'R' -> Base { team = Team.Red; bots = [] }
  | '1'..'9' as g -> Gold (int_of_char g - 48)
  | _ -> failwith "[MAP ERROR]: invalid tile"
end

module Game = struct
  (** Represents the overall game state. *)
  type t = {
    (* Bot information*)
    bots: bot BotMap.t;

    (* Team scoreboard *)
    score: int TeamMap.t;

    (* Team move counter *)
    moves: int TeamMap.t;

    (* Width of the game map *)
    width: int;

    (* Height of the game map *)
    height: int;

    (* Vision radius of the game map *)
    vision: int;

    (* Carrying capacity of individual robots *)
    max_gold: int;

    (* Spawn limit of the game map *)
    max_bots: int;

    (* Tile arrangement *)
    grid: Tile.t PosMap.t;

    (* Replay viewer data *)
    log: Yojson.Basic.json list;

    (* Minimum amount of gold to win *)
    end_goal: int;

    (* Current winner *)
    winner: Team.t option;

    (* Maximum moves allowed on game map before timeout *)
    max_moves: int;
  }

  and bot = {
    (* Inventory amount *)
    inv: int;

    (* Current position on grid *)
    pos: Pos.t;

    (* Team ownership *)
    team: Team.t;
  }
end
