open! Core

module Player_kind : sig
  type t = P1 | P2 [@@deriving sexp, compare, equal]
  val opposite : t -> t
end

module Cell_position : sig
  type t = { row : int; column : int } [@@deriving sexp, compare]
  include Comparable.S with type t := t
end

module Move : sig
  include module type of Cell_position with type t = Cell_position.t
end

module Cell_type : sig
  type t = Hit | Miss | Ship [@@deriving sexp, compare, equal]
end

module Ship : sig
  type t = { id : string; cells : Cell_position.t list } [@@deriving sexp, compare, equal]
end

module Board : sig
  type t =
    { rows : int
    ; cols : int
    ; ships : Ship.t list
    ; shots : Cell_type.t Cell_position.Map.t
    }
  [@@deriving sexp, compare, equal]

  val cell_has_ship : t -> Cell_position.t -> bool
  val is_legal_cell_position : t -> Cell_position.t -> bool
  val all_ships_sunk : t -> bool
  val already_shot : t -> Cell_position.t -> bool
end

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
  [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

module Game_mode : sig
  type t =
    | PvP
    | PvE_easy
    | PvE_hard
  [@@deriving sexp, compare, equal]
end

module Phase : sig
  type placement_status = {
    p1_ready: bool;
    p2_ready: bool;
  }

  type t =
    | Placement of placement_status
    | In_progress
    | Game_over
  [@@deriving sexp, compare, equal]
end

module Game_state : sig
  module Create_error : sig
    type t =
      | Board_too_big_or_small
      | Illegal_ship_cell
      | Overlapping_ships
    [@@deriving sexp, compare, equal, hash]
  end

  module Move_error : sig
    type t =
      | Game_is_over
      | Already_shot
      | Illegal_cell_position
    [@@deriving sexp, compare, equal]
  end

  module Ship_rules : sig
    val validate_fleet
      :  rows:int
      -> cols:int
      -> Ship.t list
      -> Create_error.t list
  end

  type t =
    { p1_board : Board.t
    ; p2_board : Board.t
    ; decision : Decision.t
    ; last_move : Move.t option
    ; phase : Phase.t
    ; mode : Game_mode.t
    }
  [@@deriving sexp, compare, equal]

  val make_board
    :  rows:int
    -> cols:int
    -> ships:Ship.t list
    -> Board.t

  val create
    :  rows:int
    -> cols:int
    -> p1_ships:Ship.t list
    -> p2_ships:Ship.t list
    -> (t, Create_error.t list) Result.t

  val create_empty : rows:int -> cols:int -> mode:Game_mode.t -> t

  val create_random
    :  rows:int
    -> cols:int
    -> seed:int
    -> (t, Create_error.t list) Result.t

  val random_fleet
    :  rows:int
    -> cols:int
    -> player:Player_kind.t
    -> seed:int
    -> Ship.t list

  val place_player_fleet : t -> player:Player_kind.t -> ships:Ship.t list -> t

  val randomize_player_fleet : t -> player:Player_kind.t -> seed:int -> t

  val cells_from_start
    :  start:Cell_position.t
    -> len:int
    -> horizontal:bool
    -> Cell_position.t list

  val can_place_on_board
    :  rows:int
    -> cols:int
    -> existing:Ship.t list
    -> Cell_position.t list
    -> bool

  val fleet_spec : (string * int) list

  val make_move : t -> Move.t -> (t, Move_error.t) Result.t

  val get_all_moves : t -> Move.t list

  val ai_move : t -> Move.t option
end