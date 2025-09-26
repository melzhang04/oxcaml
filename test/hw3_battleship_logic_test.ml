open! Core
open Battleship_logic_library
open Hw2_battleship_logic

let ok_exn r = Result.ok r |> Option.value_exn

let cp row column : Cell_position.t = { row; column }
let mv row column : Move.t = ({ row; column } : Move.t)

let ship id cells : Ship.t = { id; cells }

let p1_ships =
  [ ship 1 [ cp 0 0; cp 0 1; cp 0 2 ]
  ; ship 2 [ cp 5 5 ]
  ]

let p2_ships =
  [ ship 3 [ cp 9 8; cp 9 9 ]
  ; ship 4 [ cp 3 3 ]
  ]

let make_state () =
  Game_state.create ~rows:10 ~cols:10 ~p1_ships:p1_ships ~p2_ships:p2_ships
  |> ok_exn

let%test "create OK with 10x10 valid boards" =
  let rows, cols = 10, 10 in
  let state = make_state () in
  let expected : Game_state.t =
    { p1_board = { Board.rows; cols; ships = p1_ships; shots = Map.empty (module Cell_position) }
    ; p2_board = { Board.rows; cols; ships = p2_ships; shots = Map.empty (module Cell_position) }
    ; columns = cols
    ; decision = Decision.In_progress { whose_turn = Player_kind.P1 }
    ; last_move = None
    }
  in
  Game_state.equal state expected
;;

let pretty_print_board (state : Game_state.t) =
  let board =
    match state.decision with
    | Decision.In_progress { whose_turn = Player_kind.P1 } -> state.p2_board
    | Decision.In_progress { whose_turn = Player_kind.P2 } -> state.p1_board
    | Decision.Winner _ -> state.p2_board
  in
  for row = 0 to board.rows - 1 do
    for col = 0 to board.cols - 1 do
      let cell = { Cell_position.row; column = col } in
      match Map.find board.shots cell with
      | Some Cell_type.Hit -> print_string "H "
      | Some Cell_type.Miss -> print_string "M "
      | _ -> print_string ". "
    done;
    print_endline ""
  done
;;


let%expect_test "pretty print P2 board after a miss then a hit" =
  let s0 = make_state () in
  let s1 = Game_state.make_move s0 (mv 0 9) |> ok_exn in
  let s2 = Game_state.make_move s1 (mv 0 0) |> ok_exn in
  let s3 = Game_state.make_move s2 (mv 3 3) |> ok_exn in
  let s4 = Game_state.make_move s3 (mv 1 1) |> ok_exn in
  pretty_print_board s4;
  [%expect {|
    . . . . . . . . . M
    . . . . . . . . . .
    . . . . . . . . . .
    . . . H . . . . . .
    . . . . . . . . . .
    . . . . . . . . . .
    . . . . . . . . . .
    . . . . . . . . . .
    . . . . . . . . . .
    . . . . . . . . . .
  |}]
;;

let create_and_print ~rows ~cols ~p1 ~p2 =
  print_s
    [%sexp
      (Game_state.create ~rows ~cols ~p1_ships:p1 ~p2_ships:p2
        : (Game_state.t, Game_state.Create_error.t list) Result.t)]
;;

let%expect_test "create fails: bad sizes" =
  create_and_print ~rows:0 ~cols:10 ~p1:[] ~p2:[];
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~rows:10 ~cols:(-1) ~p1:[] ~p2:[];
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~rows:21 ~cols:21 ~p1:[] ~p2:[];
  [%expect {| (Error (Board_too_big_or_small)) |}]
;;

let%expect_test "create fails: illegal ship cell" =
  let p1_bad = [ ship 1 [ cp 10 0 ] ] in
  create_and_print ~rows:10 ~cols:10 ~p1:p1_bad ~p2:[];
  [%expect {| (Error (Illegal_ship_cell)) |}]
;;

let%expect_test "create fails: overlapping ships" =
  let p2_overlap = [ ship 1 [ cp 1 1 ]; ship 2 [ cp 1 1; cp 2 2 ] ] in
  create_and_print ~rows:10 ~cols:10 ~p1:[] ~p2:p2_overlap;
  [%expect {| (Error (Overlapping_ships)) |}]
;;

let make_move_and_print s p =
  print_s
    [%sexp
      (Game_state.make_move s p : (Game_state.t, Game_state.Move_error.t) Result.t)]
;;

let%expect_test "make_move: illegal cell position (out of bounds)" =
  let s = make_state () in
  make_move_and_print s (mv 10 0);
  [%expect {| (Error Illegal_cell_position) |}]
;;

let%expect_test "make_move: space already shot" =
  let s = make_state () in
  let s1 = Game_state.make_move s (mv 0 0) |> ok_exn in
  make_move_and_print s1 (mv 0 0);
  [%expect {| (Error Space_already_shot) |}]
;;

let%expect_test "P1 shoots a MISS then turn switches to P2" =
  let s = make_state () in
  let r = Game_state.make_move s (mv 0 9) in
  print_s [%sexp (r : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect
    {|
    (Ok
     ((p1_board
       ((rows 10) (cols 10)
        (ships
         (((id 1) (cells ((((row 0) (column 0)) ((row 0) (column 1))
                           ((row 0) (column 2)))))
          ((id 2) (cells ((((row 5) (column 5))))))))
        (shots ())))
      (p2_board
       ((rows 10) (cols 10)
        (ships
         (((id 3) (cells ((((row 9) (column 8)) ((row 9) (column 9)))))
          ((id 4) (cells ((((row 3) (column 3))))))))
        (shots ((((row 0) (column 9)) Miss)))))
      (columns 10) (decision (In_progress (whose_turn P2)))
      (last_move (((row 0) (column 9))))))
    |}]
;;

let%expect_test "P1 hits (3,3) on P2 and game continues (not yet all sunk)" =
  let s = make_state () in
  let r = Game_state.make_move s (mv 3 3) in
  print_s [%sexp (r : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect
    {|
    (Ok
     ((p1_board
       ((rows 10) (cols 10)
        (ships
         (((id 1) (cells ((((row 0) (column 0)) ((row 0) (column 1))
                           ((row 0) (column 2)))))
          ((id 2) (cells ((((row 5) (column 5))))))))
        (shots ())))
      (p2_board
       ((rows 10) (cols 10)
        (ships
         (((id 3) (cells ((((row 9) (column 8)) ((row 9) (column 9)))))
          ((id 4) (cells ((((row 3) (column 3))))))))
        (shots ((((row 3) (column 3)) Hit)))))
      (columns 10) (decision (In_progress (whose_turn P2)))
      (last_move (((row 3) (column 3))))))
    |}]
;;

let%expect_test "P1 sinks all P2 ships and wins" =
  let s = make_state () in
  let s1 = Game_state.make_move s (mv 3 3) |> ok_exn in
  let s2 = Game_state.make_move s1 (mv 1 1) |> ok_exn in
  let s3 = Game_state.make_move s2 (mv 9 8) |> ok_exn in
  let res = Game_state.make_move s3 (mv 9 9) in
  print_s [%sexp (res : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect
    {|
    (Ok
     ((p1_board
       ((rows 10) (cols 10)
        (ships
         (((id 1) (cells ((((row 0) (column 0)) ((row 0) (column 1))
                           ((row 0) (column 2)))))
          ((id 2) (cells ((((row 5) (column 5))))))))
        (shots ())))
      (p2_board
       ((rows 10) (cols 10)
        (ships
         (((id 3) (cells ((((row 9) (column 8)) ((row 9) (column 9)))))
          ((id 4) (cells ((((row 3) (column 3))))))))
        (shots ((((row 3) (column 3)) Hit) (((row 9) (column 8)) Hit)
                (((row 9) (column 9)) Hit)))))
      (columns 10) (decision (Winner P1)) (last_move (((row 9) (column 9))))))
    |}]
;;

let%expect_test "get_all_moves initially lists 100 moves (10x10)" =
  let s = make_state () in
  let moves = Game_state.get_all_moves s in
  print_s [%message (List.length moves : int)];
  [%expect {| (List.length 100) |}]
;;

let%expect_test "get_all_moves shrinks after shots" =
  let s = make_state () in
  let s1 = Game_state.make_move s (mv 0 0) |> ok_exn in
  let moves_p2_turn = Game_state.get_all_moves s1 in
  print_s [%message "after P1 shot (now P2 turn)" (List.length moves_p2_turn : int)];
  [%expect {| ("after P1 shot (now P2 turn)" (List.length 100)) |}];
  let s2 = Game_state.make_move s1 (mv 1 1) |> ok_exn in
  let moves_p1_turn_again = Game_state.get_all_moves s2 in
  print_s [%message "after P2 shot (back to P1 turn)" (List.length moves_p1_turn_again : int)];
  [%expect {| ("after P2 shot (back to P1 turn)" (List.length 99)) |}]
;;

let%test "Decision.is_game_over true iff Winner" =
  Decision.is_game_over (Decision.Winner Player_kind.P2)
  && not (Decision.is_game_over (Decision.In_progress { whose_turn = Player_kind.P1 }))
;;
