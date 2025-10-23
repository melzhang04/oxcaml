open! Core
open Battleship_logic_library
open Hw2_battleship_logic
open Virtual_dom
open! Bonsai.Let_syntax

(* === Ship Node === *)
let ship_node (ship : Ship.t) =
  let file =
    match String.lowercase ship.Ship.id with
    | id when String.is_substring id ~substring:"carrier" -> "Aircraft_Carrier.svg"
    | id when String.is_substring id ~substring:"battleship" -> "Battleship.svg"
    | id when String.is_substring id ~substring:"cruiser" -> "Cruiser.svg"
    | id when String.is_substring id ~substring:"submarine" -> "Submarine.svg"
    | id when String.is_substring id ~substring:"destroyer" -> "Destroyer.svg"
    | _ -> "Cruiser.svg"
  in
  let first = List.hd_exn ship.cells in
  let last = List.last_exn ship.cells in
  let horizontal = first.Cell_position.row = last.row in
  let len = List.length ship.cells in
  let rotate = if horizontal then "0deg" else "90deg" in
  let (w, h) = if horizontal then (len, 1) else (1, len) in
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.class_ "ship"
      ; Vdom.Attr.create "style"
          (Printf.sprintf "--x:%d; --y:%d; --w:%d; --h:%d; --rotate:%s;"
             first.column first.row w h rotate)
      ]
    [ Vdom.Node.img
        ~attrs:[ Vdom.Attr.src ("hw5_html_css/" ^ file); Vdom.Attr.alt ship.Ship.id ] ()
    ]

(* === Marker Node === *)
let marker_node ~hit ~x ~y =
  let cls = if hit then "marker hit" else "marker miss" in
  let style =
    Printf.sprintf
      "left:calc(%d * 100%% / var(--n)); top:calc(%d * 100%% / var(--n));" x y
  in
  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ cls; Vdom.Attr.create "style" style ] []

(* === Single Cell === *)
let cell_node ~x ~y ~clickable ~on_click =
  let click_attr =
    if clickable then
      Vdom.Attr.on_click (fun _ -> on_click { Cell_position.row = y; column = x })
    else Vdom.Attr.empty
  in
  let style =
    Printf.sprintf
      "left:calc(%d * 100%% / var(--n)); \
       top:calc(%d * 100%% / var(--n)); \
       width:calc(100%% / var(--n)); \
       height:calc(100%% / var(--n)); \
       box-sizing:border-box; \
       border:1px solid rgba(255,255,255,0.15); \
       background:rgba(0,0,0,0.05);"
      x y
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "cell"; Vdom.Attr.create "style" style; click_attr ]
    []

(* === Playfield === *)
let render_playfield (board : Board.t) ~clickable ~on_click ~is_game_over =
  let cells =
    List.concat_map (List.init board.rows ~f:Fn.id) ~f:(fun y ->
      List.map (List.init board.cols ~f:Fn.id) ~f:(fun x ->
        cell_node ~x ~y ~clickable ~on_click))
  in

  (* Show ships only if fully sunk OR after game ends *)
  let ships =
    if is_game_over then
      List.map board.ships ~f:ship_node
    else
      List.filter board.ships ~f:(fun ship ->
        List.for_all ship.cells ~f:(fun cell ->
          match Map.find board.shots cell with
          | Some Cell_type.Hit -> true
          | _ -> false))
      |> List.map ~f:ship_node
  in

  let markers =
    Map.to_alist board.shots
    |> List.map ~f:(fun (pos, cell) ->
      let hit = Cell_type.equal cell Cell_type.Hit in
      marker_node ~hit ~x:pos.column ~y:pos.row)
  in

  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "playfield" ]
    (cells @ ships @ markers)
;;

(* === Labels === *)
let labels_top =
  let letters = [|"Y";"B";"C";"D";"E";"F";"G";"H";"I";"J"|] in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "labels-top" ]
    (Array.to_list (Array.map letters ~f:(fun s -> Vdom.Node.span [ Vdom.Node.text s ])))

let labels_left =
  let nums = List.init 10 ~f:(fun i -> Int.to_string (i + 1)) in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "labels-left" ]
    (List.map nums ~f:(fun n -> Vdom.Node.span [ Vdom.Node.text n ]))

(* === Board Wrapper === *)
let render_board (board : Board.t) ~clickable ~on_click ~is_game_over =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game" ]
    [ labels_top
    ; labels_left
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "board" ]
        [ render_playfield board ~clickable ~on_click ~is_game_over ]
    ]

(* === Main App === *)
let battleship_app =
  let init_state =
    let p1 =
      Game_state.random_fleet ~rows:10 ~cols:10 ~player:Player_kind.P1 ~seed:(Core.Random.int_incl 0 100000)
    in
    let p2 =
      Game_state.random_fleet ~rows:10 ~cols:10 ~player:Player_kind.P2 ~seed:(Core.Random.int_incl 0 100000)
    in
    Game_state.create ~rows:10 ~cols:10 ~p1_ships:p1 ~p2_ships:p2
    |> Result.ok |> Option.value_exn
  in

  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:init_state (module Game_state)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state in

  let handle_click pos =
    match Game_state.make_move game_state pos with
    | Ok new_state -> set_game_state new_state
    | Error _ -> Vdom.Effect.Ignore
  in

  let is_game_over =
    match game_state.decision with
    | Decision.Winner _ -> true
    | _ -> false
  in

  (* === Header === *)
  let header =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "header" ]
      [ Vdom.Node.h1
          ~attrs:[ Vdom.Attr.class_ "bungee-tint-regular" ]
          [ Vdom.Node.text "BATTLESHIP" ] ]
  in

  (* === Turn Indicator === *)
  let turn_text =
    match game_state.decision with
    | Decision.In_progress { whose_turn } ->
        (match whose_turn with
         | Player_kind.P1 -> "PLAYER 1’S TURN"
         | Player_kind.P2 -> "PLAYER 2’S TURN")
    | Decision.Winner winner ->
        (match winner with
         | Player_kind.P1 -> "PLAYER 1 WINS!"
         | Player_kind.P2 -> "PLAYER 2 WINS!")
  in
  let turn_indicator =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "turn-indicator" ]
      [ Vdom.Node.text turn_text ]
  in

  (* === Clickability by turn === *)
  let my_clickable, opp_clickable =
    match game_state.decision with
    | Decision.In_progress { whose_turn = Player_kind.P1 } -> (false, true)
    | Decision.In_progress { whose_turn = Player_kind.P2 } -> (true, false)
    | Decision.Winner _ -> (false, false)
  in

  (* === Player Boards === *)
  let my_board =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.create "style" "display:flex; flex-direction:column; align-items:center;" ]
      [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "board-title" ] [ Vdom.Node.text "PLAYER 1'S BOARD" ]
      ; render_board game_state.p1_board
          ~clickable:my_clickable ~on_click:handle_click ~is_game_over
      ]
  in

  let opp_board =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.create "style" "display:flex; flex-direction:column; align-items:center;" ]
      [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "board-title" ] [ Vdom.Node.text "PLAYER 2'S BOARD" ]
      ; render_board game_state.p2_board
          ~clickable:opp_clickable ~on_click:handle_click ~is_game_over
      ]
  in

  (* === Layout === *)
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "battleship-container" ]
    [ header
    ; turn_indicator
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "board-wrapper" ]
        [ my_board; opp_board ]
    ]
;;

let () = Bonsai_web.Start.start battleship_app