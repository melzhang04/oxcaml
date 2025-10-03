open! Core

type player_kind =
  | P1
  | P2

type cell_position =
  { row : int
  ; column : int
  }

type cell_type =
  | Hit
  | Miss
  | Ship

type shot_result =
  | Hit
  | Miss

type decision =
  | In_progress of { whose_turn : player_kind }
  | Winner of player_kind

type ship =
  { id : string
  ; cells : cell_position list
  }

type board =
  { rows : int
  ; cols : int
  ; ships : ship list
  ; shots : (cell_position * shot_result) list
  }

type game_state =
  { p1_board : board
  ; p2_board : board
  ; decision : decision
  }

type move = cell_position

val initial_state : game_state
val before_terminal_state : game_state
val move_to_terminal_state : move
val terminal_state : game_state
