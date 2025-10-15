open! Core
open Battleship_logic_library
open Hw2_battleship_logic
open Virtual_dom
open! Bonsai.Let_syntax

let viewbox = Vdom.Attr.create "viewBox" "0 0 100 100"

let hit_mark =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ viewbox ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      "<line x1='25' y1='25' x2='75' y2='75' stroke='red' stroke-width='8' stroke-linecap='round' />\
       <line x1='25' y1='75' x2='75' y2='25' stroke='red' stroke-width='8' stroke-linecap='round' />"
    ()
;;

let miss_mark =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ viewbox ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      "<circle cx='50' cy='50' r='10' stroke='navy' stroke-width='6' fill='none' />"
    ()
;;

let render_cell (board : Board.t) (pos : Cell_position.t) ~(clickable : bool)
    ~(on_click : unit Vdom.Effect.t)
  =
  let cell_state = Map.find board.shots pos in
  let content =
    match cell_state with
    | Some Cell_type.Hit -> hit_mark
    | Some Cell_type.Miss -> miss_mark
    | Some Cell_type.Ship -> hit_mark
    | None ->
      if (not clickable) && Board.cell_has_ship board pos
      then Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "ship-cell" ] []
      else Vdom.Node.none
  in
  let base_attrs =
    [ Vdom.Attr.class_ "cell"
    ; Vdom.Attr.style
        Css_gen.(
          position `Absolute
          @> top
               (`Percent
                  (Percent.of_percentage
                     (Float.of_int pos.row *. (100. /. Float.of_int board.rows))))
          @> left
               (`Percent
                  (Percent.of_percentage
                     (Float.of_int pos.column *. (100. /. Float.of_int board.cols))))
          @> width
               (`Percent
                  (Percent.of_percentage (100. /. Float.of_int board.cols)))
          @> height
               (`Percent
                  (Percent.of_percentage (100. /. Float.of_int board.rows)))
          @> border ~width:(`Px 1) ~style:`Solid ~color:(`Name "#1f3649") ()
          @> background_color
               (if (not clickable) && not (Map.mem board.shots pos)
                then (`Name "#6aa7c0")
                else (`Name "#a8c4d4"))
        )
    ]
  in
  let clickable_attr =
    if clickable then Vdom.Attr.on_click (fun _ -> on_click) else Vdom.Attr.empty
  in
  Vdom.Node.div ~attrs:(base_attrs @ [ clickable_attr ]) [ content ]
;;

let render_board (board : Board.t) ~clickable ~on_click =
  let cells =
    List.concat_map (List.init board.rows ~f:Fn.id) ~f:(fun r ->
      List.map (List.init board.cols ~f:Fn.id) ~f:(fun c ->
        let pos = { Cell_position.row = r; column = c } in
        render_cell board pos ~clickable ~on_click:(on_click pos)))
  in
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.class_ "board-grid"
      ; Vdom.Attr.style
          Css_gen.(
            position `Relative
            @> width (`Percent (Percent.of_percentage 100.))
            @> height (`Percent (Percent.of_percentage 100.))
            @> border_radius (`Px 8)
            @> overflow `Hidden)
      ]
    cells
;;

let battleship_app =
  let init_state =
    Game_state.create_random ~rows:10 ~cols:10 ~seed:42 |> Result.ok |> Option.value_exn
  in
  let%sub game_state_var, set_game_state = Bonsai.state ~default_model:init_state (module Game_state) in
  let%arr game_state = game_state_var and set_game_state = set_game_state in
  let is_game_over = Decision.is_game_over game_state.Game_state.decision in

  let handle_click pos =
    if is_game_over
    then Vdom.Effect.Ignore
    else (
      match Game_state.make_move game_state pos with
      | Ok new_state -> set_game_state new_state
      | Error _ -> Vdom.Effect.Ignore)
  in

  let title =
    match game_state.decision with
    | In_progress { whose_turn } ->
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "status" ]
        [ Vdom.Node.textf
            "Turn: %s"
            (match whose_turn with P1 -> "Player 1" | P2 -> "Player 2") ]
    | Winner p ->
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "status winner" ]
        [ Vdom.Node.textf
            "Winner: %s"
            (match p with P1 -> "Player 1" | P2 -> "Player 2") ]
  in

  let p1_view =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "board-container" ]
      [ Vdom.Node.h3 [ Vdom.Node.text "My Board" ]
      ; render_board game_state.p1_board ~clickable:false
          ~on_click:(fun _ -> Vdom.Effect.Ignore)
      ]
  in

  let p2_view =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "board-container" ]
      [ Vdom.Node.h3 [ Vdom.Node.text "Opponent’s Board" ]
      ; render_board game_state.p2_board ~clickable:true ~on_click:handle_click
      ]
  in

  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "battleship-app" ]
    [ title
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "boards-wrapper" ]
        [ p1_view; p2_view ]
    ]
;;

let () = Bonsai_web.Start.start battleship_app