open! Core
open Battleship_logic_library
open Hw2_battleship_logic

let ok_exn r = Result.ok r |> Option.value_exn

let cell row column : Cell_position.t = { row; column }
let move row column : Move.t = ({ row; column } : Move.t)
let make_ship id cells : Ship.t = { id; cells }

let p1_ships =
  [ make_ship "destroyer_1"        [ cell 5 5; cell 6 5 ]
  ; make_ship "submarine_1"        [ cell 0 0; cell 0 1; cell 0 2 ]
  ; make_ship "cruiser_1"          [ cell 4 7; cell 5 7; cell 6 7 ]
  ; make_ship "battleship_1"       [ cell 9 0; cell 9 1; cell 9 2; cell 9 3 ]
  ; make_ship "aircraft_carrier_1" [ cell 2 2; cell 2 3; cell 2 4; cell 2 5; cell 2 6 ]
  ]

let p2_ships =
  [ make_ship "destroyer_2"        [ cell 9 8; cell 9 9 ]
  ; make_ship "submarine_2"        [ cell 6 0; cell 7 0; cell 8 0 ]
  ; make_ship "cruiser_2"          [ cell 3 3; cell 3 4; cell 3 5 ]
  ; make_ship "battleship_2"       [ cell 0 9; cell 1 9; cell 2 9; cell 3 9 ]
  ; make_ship "aircraft_carrier_2" [ cell 5 2; cell 5 3; cell 5 4; cell 5 5; cell 5 6 ]
  ]

let init_state () =
  Game_state.create ~rows:10 ~cols:10 ~p1_ships ~p2_ships |> ok_exn

let row_separator cols = String.init (2 * cols - 1) ~f:(fun _ -> '-')

let char_of_shot = function
  | Some Cell_type.Hit  -> "H"
  | Some Cell_type.Miss -> "M"
  | Some Cell_type.Ship -> "S"
  | None                -> " "

let print_board (board : Board.t) =
  let sep = row_separator board.cols in
  for r = 0 to board.rows - 1 do
    List.init board.cols ~f:(fun c ->
      let pos = { Cell_position.row = r; column = c } in
      char_of_shot (Map.find board.shots pos))
    |> String.concat ~sep:"|"
    |> print_endline;
    if r < board.rows - 1 then print_endline sep
  done

let pretty_print_board (state : Game_state.t) =
  let board =
    match state.decision with
    | Decision.In_progress { whose_turn = Player_kind.P1 } -> state.p2_board
    | Decision.In_progress { whose_turn = Player_kind.P2 } -> state.p1_board
    | Decision.Winner _ -> state.p2_board
  in
  print_board board;
  print_s [%sexp (state.decision : Decision.t)]
;;

let print_both_boards (state : Game_state.t) =
    print_endline "P2 board (shots by P1):";
    print_board state.p2_board;
    print_endline "P1 board (shots by P2):";
    print_board state.p1_board;
    print_s [%sexp (state.decision : Decision.t)]
;;

let%test "create 10x10 valid boards for P1 and P2" =
  let state = init_state () in
  let expected_state : Game_state.t =
    { p1_board =
        { rows = 10; cols = 10; ships = p1_ships; shots = Map.empty (module Cell_position) }
    ; p2_board =
        { rows = 10; cols = 10; ships = p2_ships; shots = Map.empty (module Cell_position) }
    ; decision = Decision.In_progress { whose_turn = Player_kind.P1 }
    ; last_move = None
    }
  in
  Game_state.equal state expected_state
;;

let create_and_print ?(p1 = p1_ships) ?(p2 = p2_ships) ~rows ~cols () =
  let result = Game_state.create ~rows ~cols ~p1_ships:p1 ~p2_ships:p2 in
  print_s [%sexp (result : (Game_state.t, Game_state.Create_error.t list) Result.t)]


let%expect_test "Game_state.create: wrong size => Board_too_big_or_small" =
  create_and_print ~rows:10 ~cols:11 ~p1:p1_ships ~p2:p2_ships ();
  [%expect {| (Error (Board_too_big_or_small)) |}]
;;

let%expect_test "Game_state.create: overlapping ships => Overlapping_ships" =
  let p1_bad_overlap =
    [ make_ship "destroyer_1" [ cell 0 0; cell 0 1 ]
    ; make_ship "submarine_1" [ cell 0 1; cell 0 2; cell 0 3 ]  (* overlaps at 0,1 *)
    ; make_ship "cruiser_1"   [ cell 4 7; cell 5 7; cell 6 7 ]
    ; make_ship "battleship_1"[ cell 9 0; cell 9 1; cell 9 2; cell 9 3 ]
    ; make_ship "aircraft_carrier_1" [ cell 2 2; cell 2 3; cell 2 4; cell 2 5; cell 2 6 ]
    ]
  in
  create_and_print ~rows:10 ~cols:10 ~p1:p1_bad_overlap ~p2:p2_ships ();
  [%expect {| (Error (Overlapping_ships)) |}]
;;

let%expect_test "Game_state.create: out-of-bounds cell => Illegal_ship_cell" =
  let p1_oob =
    [ make_ship "destroyer_1" [ cell 10 0; cell 10 1 ]  (* row 10 OOB *)
    ; make_ship "submarine_1" [ cell 0 0; cell 0 1; cell 0 2 ]
    ; make_ship "cruiser_1"   [ cell 4 7; cell 5 7; cell 6 7 ]
    ; make_ship "battleship_1"[ cell 9 0; cell 9 1; cell 9 2; cell 9 3 ]
    ; make_ship "aircraft_carrier_1" [ cell 2 2; cell 2 3; cell 2 4; cell 2 5; cell 2 6 ]
    ]
  in
  create_and_print ~rows:10 ~cols:10 ~p1:p1_oob ~p2:p2_ships ();
  [%expect {|
    (Error
     (Illegal_ship_cell Illegal_ship_cell Illegal_ship_cell Illegal_ship_cell
      Illegal_ship_cell)) |}]
;;

let%expect_test "Game_state.create: diagonal/discontiguous => Illegal_ship_cell" =
  let p1_diag =
    [ make_ship "destroyer_1" [ cell 0 0; cell 1 1 ]  (* diagonal *)
    ; make_ship "submarine_1" [ cell 0 3; cell 0 4; cell 0 5 ]
    ; make_ship "cruiser_1"   [ cell 4 7; cell 5 7; cell 6 7 ]
    ; make_ship "battleship_1"[ cell 9 0; cell 9 1; cell 9 2; cell 9 3 ]
    ; make_ship "aircraft_carrier_1" [ cell 2 2; cell 2 3; cell 2 4; cell 2 5; cell 2 6 ]
    ]
  in
  create_and_print ~rows:10 ~cols:10 ~p1:p1_diag ~p2:p2_ships ();
  [%expect {|
    (Error
     (Illegal_ship_cell Illegal_ship_cell Illegal_ship_cell Illegal_ship_cell
      Illegal_ship_cell)) |}]
;;

let%expect_test "Game_state.create: wrong length => Illegal_ship_cell" =
  let p1_wrong_len =
    [ make_ship "destroyer_1" [ cell 0 0 ]  (* should be 2 *)
    ; make_ship "submarine_1" [ cell 0 2; cell 0 3; cell 0 4 ]
    ; make_ship "cruiser_1"   [ cell 4 7; cell 5 7; cell 6 7 ]
    ; make_ship "battleship_1"[ cell 9 0; cell 9 1; cell 9 2; cell 9 3 ]
    ; make_ship "aircraft_carrier_1" [ cell 2 2; cell 2 3; cell 2 4; cell 2 5; cell 2 6 ]
    ]
  in
  create_and_print ~rows:10 ~cols:10 ~p1:p1_wrong_len ~p2:p2_ships ();
  [%expect {|
    (Error
     (Illegal_ship_cell Illegal_ship_cell Illegal_ship_cell Illegal_ship_cell
      Illegal_ship_cell)) |}]
;;

let%expect_test "pretty print opponent board (initial: all blanks)" =
  let s0 = init_state () in
  pretty_print_board s0;
  [%expect {|
 | | | | | | | | | 
-------------------
 | | | | | | | | | 
-------------------
 | | | | | | | | | 
-------------------
 | | | | | | | | | 
-------------------
 | | | | | | | | | 
-------------------
 | | | | | | | | | 
-------------------
 | | | | | | | | | 
-------------------
 | | | | | | | | | 
-------------------
 | | | | | | | | | 
-------------------
 | | | | | | | | | 
(In_progress (whose_turn P1))
|}]
;;

let%expect_test "Game_state.make_move: illegal cell and already-shot" =
  let s0 = init_state () in
  let r1 = Game_state.make_move s0 (move 10 0) in
  print_s [%sexp (r1 : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect {| (Error Illegal_cell_position) |}];

  (* Valid first shot (P1), then P2 plays, then P1 repeats same spot -> Already_shot *)
  let s1 = Game_state.make_move s0 (move 0 9) |> ok_exn in
  let s2 = Game_state.make_move s1 (move 1 1) |> ok_exn in
  let r3 = Game_state.make_move s2 (move 0 9) in
  print_s [%sexp (r3 : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect {| (Error Already_shot) |}]
;;

(* Winner test: P1 sinks all P2 ships with alternation preserved *)
let all_cells (rows:int) (cols:int) =
  List.concat_map (List.range 0 rows) ~f:(fun r ->
    List.map (List.range 0 cols) ~f:(fun c -> cell r c))
;;

let cells_set lst = Cell_position.Set.of_list lst
let p1_ship_cells = cells_set (List.concat_map p1_ships ~f:(fun s -> s.cells))
let p2_ship_cells = List.concat_map p2_ships ~f:(fun s -> s.cells)

let p1_miss_cells =
  all_cells 10 10
  |> List.filter ~f:(fun p -> not (Set.mem p1_ship_cells p))

let%expect_test "Game_state.make_move: P1 sinks all P2 ships => Winner P1" =
  let rec sink_all (state : Game_state.t)
                   (remaining_hits : Cell_position.t list)
                   (remaining_misses : Cell_position.t list) =
    match remaining_hits with
    | [] -> state
    | target :: tl ->
      let s1 = Game_state.make_move state (move target.row target.column) |> ok_exn in
      match s1.decision with
      | Decision.Winner _ -> s1
      | Decision.In_progress _ ->
        match remaining_misses with
        | [] -> s1
        | miss :: miss_tl ->
          let s2 = Game_state.make_move s1 (move miss.row miss.column) |> ok_exn in
          sink_all s2 tl miss_tl
  in
  let final = sink_all (init_state ()) p2_ship_cells p1_miss_cells in
  print_s [%sexp (final.decision : Decision.t)];
  [%expect {| (Winner P1) |}];

  let r = Game_state.make_move final (move 0 0) in
  print_s [%sexp (r : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect {| (Error Game_is_over) |}]
;;

let random_walk (initial_state : Game_state.t) ~random_seed =
  let rec random_walk (state : Game_state.t) =
    let all_moves = Game_state.get_all_moves state in
    let next_states =
      List.filter_map all_moves ~f:(fun move ->
        Game_state.make_move state move |> Result.ok)
    in
    let random_state = List.random_element next_states |> Option.value_exn in
    match Decision.is_game_over random_state.decision with
    | true -> random_state
    | false -> random_walk random_state
  in
  (* Set random seed. *)
  Core.Random.init random_seed;
  random_walk initial_state
;;

let%test "random walk reaches terminal state (seed=1)" =
  Decision.is_game_over (random_walk (init_state ()) ~random_seed:1).decision
;;
let%test "random walk reaches terminal state (seed=3)" =
  Decision.is_game_over (random_walk (init_state ()) ~random_seed:3).decision
;;
let%test "random walk reaches terminal state (seed=1234)" =
  Decision.is_game_over (random_walk (init_state ()) ~random_seed:1234).decision
;;

let%expect_test "Battleship random walk (seed=7) — full boards" =
  let final = random_walk (init_state ()) ~random_seed:7 in
  print_both_boards final;
  [%expect {|
P2 board (shots by P1):
M|M|M| |M|M|M|M|M|H
-------------------
M|M|M|M|M|M|M|M|M|H
-------------------
M|M|M|M|M| |M|M|M|H
-------------------
 |M|M|H|H|H|M|M| |H
-------------------
M|M|M| |M|M|M|M|M|M
-------------------
M|M|H|H|H|H|H|M|M|M
-------------------
H|M|M|M|M|M|M|M|M|M
-------------------
H|M|M|M|M|M|M|M|M|M
-------------------
H|M|M|M|M|M|M|M|M|M
-------------------
M|M|M|M|M|M|M|M|H|H
P1 board (shots by P2):
H|H|H|M|M|M|M|M|M|M
-------------------
M|M|M|M|M|M|M|M| |M
-------------------
M|M| |H|H| |H|M|M|M
-------------------
M|M|M|M|M|M|M|M| |M
-------------------
M|M|M|M|M|M|M|H|M|M
-------------------
M|M|M|M|M|H|M|H|M|M
-------------------
M|M|M|M|M|H|M|H|M|M
-------------------
M|M|M|M|M|M|M|M|M|M
-------------------
 |M|M|M|M| |M|M|M|M
-------------------
H|H|H|H|M|M|M|M|M|M
(Winner P1)
|}]
;;
