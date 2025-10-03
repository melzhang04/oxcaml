open! Core
open! Hw2_battleship_logic

val pick_random_move : Game_state.t -> seed:int -> Move.t option
val play_random : Game_state.t -> seed:int -> Game_state.t

val greedy_move : Game_state.t -> Move.t option
val choose_move_timed : Game_state.t -> time_ms:int -> Move.t option

val play_with_policy :
  Game_state.t ->
  (Game_state.t -> Move.t option) ->
  Game_state.t

val simulate_matches :
  games:int ->
  seed:int ->
  time_ms:int ->
  unit
