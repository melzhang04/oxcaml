open! Core
open Battleship_logic_library
open Hw2_battleship_logic
module AB = Battleship_logic_library.Hw4_alpha_beta_search

let ok_exn r = Result.ok r |> Option.value_exn

let init_random () =
  Game_state.create_random ~rows:10 ~cols:10 ~seed:7 |> ok_exn

let row_separator cols = String.init (2 * cols - 1) ~f:(fun _ -> '-')

let cell_str = function
  | Some Cell_type.Hit -> "H"
  | Some Cell_type.Miss -> "M"
  | Some Cell_type.Ship -> "S"
  | None -> " "

let print_board (b : Board.t) =
  let sep = row_separator b.cols in
  for r = 0 to b.rows - 1 do
    List.init b.cols ~f:(fun c ->
      let pos = { Cell_position.row = r; column = c } in
      cell_str (Map.find b.shots pos))
    |> String.concat ~sep:"|"
    |> print_endline;
    if r < b.rows - 1 then print_endline sep
  done

let print_turn (state : Game_state.t) =
  match state.decision with
  | Decision.In_progress { whose_turn = Player_kind.P1 } ->
      print_endline "P2 board (shots by P1):";
      print_board state.p2_board
  | Decision.In_progress { whose_turn = Player_kind.P2 } ->
      print_endline "P1 board (shots by P2):";
      print_board state.p1_board
  | Decision.Winner _ ->
      print_endline "Final boards:";
      print_endline "P2 board (shots by P1):"; print_board state.p2_board;
      print_endline "P1 board (shots by P2):"; print_board state.p1_board

let%test "timed AI picks a legal move on a fresh random game" =
  let s = init_random () in
  match AB.choose_move_timed s ~time_ms:10 with
  | None -> false
  | Some mv ->
    (match s.decision with
     | Decision.In_progress { whose_turn = Player_kind.P1 } ->
       Board.is_legal_cell_position s.p2_board mv
       && not (Board.already_shot s.p2_board mv)
     | Decision.In_progress { whose_turn = Player_kind.P2 } ->
       Board.is_legal_cell_position s.p1_board mv
       && not (Board.already_shot s.p1_board mv)
     | Decision.Winner _ -> false)

let%test "greedy AI picks a legal move" =
  let s = init_random () in
  match AB.greedy_move s with
  | None -> false
  | Some mv ->
    (match s.decision with
     | Decision.In_progress { whose_turn = Player_kind.P1 } ->
       Board.is_legal_cell_position s.p2_board mv
       && not (Board.already_shot s.p2_board mv)
     | Decision.In_progress { whose_turn = Player_kind.P2 } ->
       Board.is_legal_cell_position s.p1_board mv
       && not (Board.already_shot s.p1_board mv)
     | Decision.Winner _ -> false)

let%test "random AI picks a legal move" =
  let s = init_random () in
  match AB.pick_random_move s ~seed:123 with
  | None -> false
  | Some mv ->
    (match s.decision with
     | Decision.In_progress { whose_turn = Player_kind.P1 } ->
       Board.is_legal_cell_position s.p2_board mv
       && not (Board.already_shot s.p2_board mv)
     | Decision.In_progress { whose_turn = Player_kind.P2 } ->
       Board.is_legal_cell_position s.p1_board mv
       && not (Board.already_shot s.p1_board mv)
     | Decision.Winner _ -> false)

let%expect_test "timed vs random: print first 10 turns and last turns" =
  let rec loop state turn =
    if Decision.is_game_over state.Game_state.decision then (
      print_endline (Printf.sprintf "\nFinal turn %d:" turn);
      print_s [%sexp (state.decision : Decision.t)];
      print_turn state;
      state
    ) else (
      let state' =
        match state.decision with
        | Decision.In_progress { whose_turn = Player_kind.P1 } ->
          let mv = AB.choose_move_timed state ~time_ms:10 |> Option.value_exn in
          Game_state.make_move state mv |> ok_exn
        | Decision.In_progress { whose_turn = Player_kind.P2 } ->
          AB.play_random state ~seed:turn
        | Decision.Winner _ -> state
      in
      if turn <= 10 then (
        print_endline (Printf.sprintf "\nAfter turn %d:" turn);
        print_s [%sexp (state'.decision : Decision.t)];
        print_turn state'
      );
      loop state' (turn + 1))
  in
  let s = init_random () in
  let _ = loop s 1 in
  ();
  [%expect {|
    After turn 1:
    (In_progress (whose_turn P2))
    P1 board (shots by P2):
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

    After turn 2:
    (In_progress (whose_turn P1))
    P2 board (shots by P1):
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | |M| | | | |
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

    After turn 3:
    (In_progress (whose_turn P2))
    P1 board (shots by P2):
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
     | | | | | | | | |M
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |

    After turn 4:
    (In_progress (whose_turn P1))
    P2 board (shots by P1):
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | |M| | | | |
    -------------------
     | | | | |M| | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |

    After turn 5:
    (In_progress (whose_turn P2))
    P1 board (shots by P2):
     | | | | | | | | |
    -------------------
     | |M| | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |M
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |

    After turn 6:
    (In_progress (whose_turn P1))
    P2 board (shots by P1):
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | |H| | | | | |
    -------------------
     | | | |M| | | | |
    -------------------
     | | | | |M| | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |

    After turn 7:
    (In_progress (whose_turn P2))
    P1 board (shots by P2):
     | | | | | | | | |
    -------------------
     | |M| | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |M
    -------------------
     | | | | | | | | |
    -------------------
     | | | | |M| | | |
    -------------------
     | | | | | | | | |

    After turn 8:
    (In_progress (whose_turn P1))
    P2 board (shots by P1):
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | |H| | | | | |
    -------------------
     | | |H| | | | | |
    -------------------
     | | | |M| | | | |
    -------------------
     | | | | |M| | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |

    After turn 9:
    (In_progress (whose_turn P2))
    P1 board (shots by P2):
     | | | | | | | | |
    -------------------
     | |M| | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | |M| | |M
    -------------------
     | | | | | | | | |
    -------------------
     | | | | |M| | | |
    -------------------
     | | | | | | | | |

    After turn 10:
    (In_progress (whose_turn P1))
    P2 board (shots by P1):
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | |H| | | | | |
    -------------------
     | | |H| | | | | |
    -------------------
     | | |H|M| | | | |
    -------------------
     | | | | |M| | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |
    -------------------
     | | | | | | | | |

    Final turn 192:
    (Winner P1)
    Final boards:
    P2 board (shots by P1):
    M|M|M| |M|M|M|M|M|M
    -------------------
    H|H|M|M| |M|M|M|M|M
    -------------------
    M|M|M|H|M|M|H|M|M|
    -------------------
    M|M|M|H|M|M|H|M|M|M
    -------------------
    M|M|M|H|M|M|H|M|M|M
    -------------------
    M|M|M|H|M|M|M|M|M|M
    -------------------
    M|M|M|H|M|H|M|M|M|M
    -------------------
    M|M| |M|M|H|M|M|M|H
    -------------------
    M|M|M|M|M|H|M|M|M|H
    -------------------
    M|M|M|M|M|H|M|M|M|H
    P1 board (shots by P2):
    M|M|M|M|M|M|H|H|H|M
    -------------------
    M|M|M| |M|M|M|M|M|M
    -------------------
    M|M|M|M|M|M|M|M|M|M
    -------------------
    M|M|M|M|M|M|M|M|M|M
    -------------------
     |M|H|M|M|M|M|H|H|H
    -------------------
    M|M|H|H|M|M|M|H|M|M
    -------------------
    M|M|H|H|M|M|M|H|M|M
    -------------------
    M| |M|H|M|M|M| |M|M
    -------------------
    M|M|M|H|M|M|M|H|M|M
    -------------------
    M|M|M| |M|M|M|M|M|M |}]

let%expect_test "timed AI vs random over 50 games (2s per move)" =
  AB.simulate_matches ~games:50 ~seed:42 ~time_ms:50;
  [%expect {|
    Timed AI (P1) wins: 45 / 50
    Random AI (P2) wins: 5 / 50
    Unfinished: 0 |}]