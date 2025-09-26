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
  { id : int
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

(*=
Initial State (10x10, empty board, no ships placed yet)
   0 1 2 3 4 5 6 7 8 9
0 | | | | | | | | | | |
1 | | | | | | | | | | |
2 | | | | | | | | | | |
3 | | | | | | | | | | |
4 | | | | | | | | | | |
5 | | | | | | | | | | |
6 | | | | | | | | | | |
7 | | | | | | | | | | |
8 | | | | | | | | | | |
9 | | | | | | | | | | |
*)

(*=
P1 (10x10, ships placed )
   0 1 2 3 4 5 6 7 8 9
0 | | | | | | | | |S| |
1 | |S|S|S|S|S| | |S| |
2 | | | | | | | | |S| |
3 | | | | | | | | |S| |
4 | | | | | | | | | | |
5 |S|S|S| | | | | | | |
6 | | | | |S| | | | | |
7 | | | | |S| | | | | |
8 | | | | |S| | | | | |
9 | | | | | | | |S|S| |
*)

(*=
P2 (10x10, ships placed)
   0 1 2 3 4 5 6 7 8 9
0 |S|S|S|S|S| | | | | |
1 | | | | | | | | | |S|
2 | | | |S| | | | | |S|
3 | | | |S| | | | | |S|
4 | | | |S| | | | | | |
5 | | | |S| | | | | | |
6 | | | | | |S|S|S| | |
7 | | | | | | | | | | |
8 | | |S|S| | | | | | |
9 | | | | | | | | | | |
*)

let initial_state : game_state =
  let p1_ships =
    [ { id = 1
      ; cells =
          [ { row = 1; column = 1 }
          ; { row = 1; column = 2 }
          ; { row = 1; column = 3 }
          ; { row = 1; column = 4 }
          ; { row = 1; column = 5 }
          ]
      }
    ; { id = 2
      ; cells =
          [ { row = 0; column = 8 }
          ; { row = 1; column = 8 }
          ; { row = 2; column = 8 }
          ; { row = 3; column = 8 }
          ]
      }
    ; { id = 3
      ; cells =
          [ { row = 5; column = 0 }; { row = 5; column = 1 }; { row = 5; column = 2 } ]
      }
    ; { id = 4
      ; cells =
          [ { row = 6; column = 4 }; { row = 7; column = 4 }; { row = 8; column = 4 } ]
      }
    ; { id = 5; cells = [ { row = 9; column = 7 }; { row = 9; column = 8 } ] }
    ]
  in
  let p2_ships =
    [ { id = 101
      ; cells =
          [ { row = 0; column = 0 }
          ; { row = 0; column = 1 }
          ; { row = 0; column = 2 }
          ; { row = 0; column = 3 }
          ; { row = 0; column = 4 }
          ]
      }
    ; { id = 102
      ; cells =
          [ { row = 2; column = 3 }
          ; { row = 3; column = 3 }
          ; { row = 4; column = 3 }
          ; { row = 5; column = 3 }
          ]
      }
    ; { id = 103
      ; cells =
          [ { row = 6; column = 5 }; { row = 6; column = 6 }; { row = 6; column = 7 } ]
      }
    ; { id = 104
      ; cells =
          [ { row = 1; column = 9 }; { row = 2; column = 9 }; { row = 3; column = 9 } ]
      }
    ; { id = 105; cells = [ { row = 8; column = 2 }; { row = 8; column = 3 } ] }
    ]
  in
  { p1_board = { rows = 10; cols = 10; ships = p1_ships; shots = [] }
  ; p2_board = { rows = 10; cols = 10; ships = p2_ships; shots = [] }
  ; decision = In_progress { whose_turn = P1 }
  }
;;

(*=
Before Terminal State

(*=
P1 Board
   0 1 2 3 4 5 6 7 8 9
0 |M| | | | | | | |H| |
1 | |H|H|H|S|S| | |H| |
2 | | | | | | | | |H| |
3 | |M| | | |M|M| |H| |
4 | | | |M| | | | | | |
5 |S|H|H| | | | |M| | |
6 | | | | |S| | | |M| |
7 | |M| | |S| | | |M| |
8 | |M| | |S| | | | | |
9 | | |M| | | | |H|H| |
*)

P2 Board
   0 1 2 3 4 5 6 7 8 9
0 |H|H|H|H|H| | | | | |
1 |M| | | | | | | | |H|
2 | | | |H| | | | | |H|
3 | |M| |H| | |M| | |H|
4 | |M| |H| | | | | | |
5 | | | |S| | | | | |M|
6 | | | | | |H|H|H| | |
7 | | | | | | | | | | |
8 |M| |H|H| | | | | | |
9 | | | | | | | | |M| |
*)
let before_terminal_state : game_state =
  let p1_board = initial_state.p1_board in
  let p2_board = initial_state.p2_board in
  { p1_board =
      { p1_board with
        shots =
          [ { row = 0; column = 0 }, Miss
          ; { row = 0; column = 8 }, Hit
          ; { row = 1; column = 1 }, Hit
          ; { row = 1; column = 2 }, Hit
          ; { row = 1; column = 3 }, Hit
          ; { row = 1; column = 8 }, Hit
          ; { row = 2; column = 8 }, Hit
          ; { row = 3; column = 1 }, Miss
          ; { row = 3; column = 5 }, Miss
          ; { row = 3; column = 6 }, Miss
          ; { row = 3; column = 8 }, Hit
          ; { row = 4; column = 3 }, Miss
          ; { row = 5; column = 1 }, Hit
          ; { row = 5; column = 2 }, Hit
          ; { row = 5; column = 7 }, Miss
          ; { row = 6; column = 8 }, Miss
          ; { row = 7; column = 1 }, Miss
          ; { row = 7; column = 8 }, Miss
          ; { row = 8; column = 1 }, Miss
          ; { row = 9; column = 2 }, Miss
          ; { row = 9; column = 7 }, Hit
          ; { row = 9; column = 8 }, Hit
          ]
      }
  ; p2_board =
      { p2_board with
        shots =
          [ { row = 0; column = 0 }, Hit
          ; { row = 0; column = 1 }, Hit
          ; { row = 0; column = 2 }, Hit
          ; { row = 0; column = 3 }, Hit
          ; { row = 0; column = 4 }, Hit
          ; { row = 1; column = 0 }, Miss
          ; { row = 1; column = 9 }, Hit
          ; { row = 2; column = 3 }, Hit
          ; { row = 2; column = 9 }, Hit
          ; { row = 3; column = 1 }, Miss
          ; { row = 3; column = 3 }, Hit
          ; { row = 3; column = 6 }, Miss
          ; { row = 3; column = 9 }, Hit
          ; { row = 4; column = 1 }, Miss
          ; { row = 4; column = 3 }, Hit
          ; { row = 5; column = 9 }, Miss
          ; { row = 6; column = 5 }, Hit
          ; { row = 6; column = 6 }, Hit
          ; { row = 6; column = 7 }, Hit
          ; { row = 8; column = 0 }, Miss
          ; { row = 8; column = 2 }, Hit
          ; { row = 8; column = 3 }, Hit
          ; { row = 9; column = 9 }, Miss
          ]
      }
  ; decision = In_progress { whose_turn = P1 }
  }
;;

(*=
Move to Terminal State
   0 1 2 3 4 5 6 7 8 9
5 | | | |H| | | | | | |
*)
let move_to_terminal_state : move = { row = 5; column = 3 }

(*=
Terminal State
P2 Board
   0 1 2 3 4 5 6 7 8 9
0 |H|H|H|H|H| | | | | |
1 |M| | | | | | | | |H|
2 | | | |H| | | | | |H|
3 | |M| |H| | |M| | |H|
4 | |M| |H| | | | | | |
5 | | | |H| | | | | |M|
6 | | | | | |H|H|H| | |
7 | | | | | | | | | | |
8 |M| |H|H| | | | | | |
9 | | | | | | | | |M| |
*)
let terminal_state : game_state =
  let p1_board = before_terminal_state.p1_board in
  let p2_board =
    let shots' = ({ row = 5; column = 3 }, Hit) :: before_terminal_state.p2_board.shots in
    { before_terminal_state.p2_board with shots = shots' }
  in
  { p1_board; p2_board; decision = Winner P1 }
;;
