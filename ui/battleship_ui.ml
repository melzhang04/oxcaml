open! Core
open Battleship_logic_library
open Hw2_battleship_logic
open Game_state
open Virtual_dom
open! Bonsai.Let_syntax

module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module F = Firebase_bind.Firebase

(* Ship SVGs *)
let ship_svg_file (id : string) =
  let id = String.lowercase id in
  if String.is_substring id ~substring:"carrier" then "Aircraft_Carrier.svg"
  else if String.is_substring id ~substring:"battleship" then "Battleship.svg"
  else if String.is_substring id ~substring:"cruiser" then "Cruiser.svg"
  else if String.is_substring id ~substring:"submarine" then "Submarine.svg"
  else if String.is_substring id ~substring:"destroyer" then "Destroyer.svg"
  else "Cruiser.svg"

let same_ship_kind (a : string) (b : string) =
  let a = String.lowercase a in
  let b = String.lowercase b in
  String.is_substring a ~substring:b || String.is_substring b ~substring:a

let ship_node (ship : Ship.t) =
  let file = ship_svg_file ship.Ship.id in
  let first = List.hd_exn ship.cells in
  let last = List.last_exn ship.cells in
  let horizontal = first.Cell_position.row = last.row in
  let len = List.length ship.cells in
  let rotate = if horizontal then "0deg" else "90deg" in
  let w, h = if horizontal then len, 1 else 1, len in
  Vdom.Node.div
    ~attrs:
      [
        Vdom.Attr.class_ "ship";
        Vdom.Attr.create "style"
          (Printf.sprintf "--x:%d; --y:%d; --w:%d; --h:%d; --rotate:%s;"
             first.column first.row w h rotate);
      ]
    [
      Vdom.Node.img
        ~attrs:
          [
            Vdom.Attr.src ("hw5_html_css/" ^ file);
            Vdom.Attr.alt ship.Ship.id;
          ]
        ();
    ]

(* Markers *)
let marker_node ~hit ~x ~y =
  let cls = if hit then "marker hit" else "marker miss" in
  let style =
    Printf.sprintf
      "left:calc(%d * 100%% / var(--n)); top:calc(%d * 100%% / var(--n));" x y
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ cls; Vdom.Attr.create "style" style ]
    []

(* Cells *)
let cell_node ~x ~y ~clickable ~on_click =
  let click_attr =
    if clickable
    then
      Vdom.Attr.on_click (fun _ ->
          on_click { Cell_position.row = y; column = x })
    else Vdom.Attr.empty
  in
  let style =
    Printf.sprintf
      "left:calc(%d * 100%% / var(--n)); \
       top:calc(%d * 100%% / var(--n)); \
       width:calc(100%% / var(--n)); \
       height:calc(100%% / var(--n)); \
       border:1px solid rgba(255,255,255,0.15); \
       background:rgba(0,0,0,0.05);"
      x y
  in
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.class_ "cell"; Vdom.Attr.create "style" style; click_attr ]
    []

(* Playfield *)
let render_playfield (board : Board.t) ~clickable ~on_click ~show_ships =
  let cells =
    List.concat_map (List.init board.rows ~f:Fn.id) ~f:(fun y ->
        List.map (List.init board.cols ~f:Fn.id) ~f:(fun x ->
            cell_node ~x ~y ~clickable ~on_click))
  in

  let ships =
    if show_ships
    then List.map board.ships ~f:ship_node
    else
      board.ships
      |> List.filter ~f:(fun ship ->
             List.for_all ship.cells ~f:(fun c ->
                 Map.mem board.shots c
                 && Cell_type.equal (Map.find_exn board.shots c) Cell_type.Hit))
      |> List.map ~f:ship_node
  in

  let markers =
    Map.to_alist board.shots
    |> List.map ~f:(fun (pos, celltype) ->
           let hit = Cell_type.equal celltype Cell_type.Hit in
           marker_node ~hit ~x:pos.column ~y:pos.row)
  in

  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "playfield" ]
    (cells @ ships @ markers)

(* Labels *)
let labels_top =
  let letters =
    [| "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J" |]
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "labels-top" ]
    (Array.to_list
       (Array.map letters ~f:(fun s ->
            Vdom.Node.span [ Vdom.Node.text s ])))

let labels_left =
  let nums = List.init 10 ~f:(fun i -> Int.to_string (i + 1)) in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "labels-left" ]
    (List.map nums ~f:(fun s -> Vdom.Node.span [ Vdom.Node.text s ]))

let render_board (board : Board.t) ~clickable ~on_click ~show_ships =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game" ]
    [
      labels_top;
      labels_left;
      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "board" ]
        [ render_playfield board ~clickable ~on_click ~show_ships ];
    ]

(* Setup Screen *)
let setup_screen ~on_start =
  let button txt mode =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> on_start mode) ]
      [ Vdom.Node.text txt ]
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "setup-screen" ]
    [
      Vdom.Node.h1 [ Vdom.Node.text "BATTLESHIP" ];
      Vdom.Node.h3 [ Vdom.Node.text "Select Game Mode" ];
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "mode-buttons" ]
        [
          button "Player vs Player" Game_mode.PvP;
          button "Vs AI (Easy)" Game_mode.PvE_easy;
          button "Vs AI (Hard)" Game_mode.PvE_hard;
        ];
    ]

(* Waiting Screens *)
let waiting_for_opponent_screen msg =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "setup-screen" ]
    [
      Vdom.Node.h1 [ Vdom.Node.text msg ];
      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "spinner" ] [];
    ]

let waiting_screen =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "setup-screen" ]
    [
      Vdom.Node.h1 [ Vdom.Node.text "BATTLESHIP" ];
      Vdom.Node.h3 [ Vdom.Node.text "Searching for opponent..." ];
      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "spinner" ] [];
    ]

(* Placement Screen *)
let placement_screen ~state ~set_game_state ~on_confirm ~local_player =
  let%sub selected_ship, set_selected_ship = Bonsai.state_opt (module String) in
  let%sub horizontal, set_horizontal =
    Bonsai.state (module Bool) ~default_model:true
  in

  let%arr selected_ship = selected_ship
  and set_selected_ship = set_selected_ship
  and horizontal = horizontal
  and set_horizontal = set_horizontal
  and state = state
  and set_game_state = set_game_state
  and on_confirm = on_confirm
  and local_player = local_player
  in

  let player =
    match state.phase with
    | Phase.Placement _ -> local_player
    | _ -> Player_kind.P1
  in

  let board =
    match player with
    | Player_kind.P1 -> state.p1_board
    | Player_kind.P2 -> state.p2_board
  in

  let handle_click pos =
    match selected_ship with
    | None -> Vdom.Effect.Ignore
    | Some id ->
        let len =
          List.Assoc.find_exn Game_state.fleet_spec ~equal:String.equal id
        in
        let cells =
          Game_state.cells_from_start ~start:pos ~len ~horizontal
        in
        if
          Game_state.can_place_on_board ~rows:board.rows ~cols:board.cols
            ~existing:board.ships cells
        then
          let new_ship = { Ship.id = id; cells } in
          let updated_board = { board with ships = new_ship :: board.ships } in
          let new_state =
            match player with
            | Player_kind.P1 -> { state with p1_board = updated_board }
            | Player_kind.P2 -> { state with p2_board = updated_board }
          in
          Vdom.Effect.Many
            [ set_selected_ship None; set_game_state new_state ]
        else Vdom.Effect.Ignore
  in

  let rotate_button =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ -> set_horizontal (not horizontal)) ]
      [
        Vdom.Node.text
          (if horizontal then "Rotate: Horizontal"
           else "Rotate: Vertical");
      ]
  in

  let random_button =
    Vdom.Node.button
      ~attrs:
        [
          Vdom.Attr.on_click
            (fun _ ->
              let seed = Random.int_incl 0 100_000 in
              let randomized_state =
                Game_state.randomize_player_fleet state ~player ~seed
              in
              let updated =
                match player with
                | Player_kind.P1 ->
                    { state with p1_board = randomized_state.p1_board }
                | Player_kind.P2 ->
                    { state with p2_board = randomized_state.p2_board }
              in
              Vdom.Effect.Many
                [ set_selected_ship None; set_game_state updated ]);
        ]
      [ Vdom.Node.text "Randomize Fleet" ]
  in

  let clear_button =
    Vdom.Node.button
      ~attrs:
        [
          Vdom.Attr.on_click
            (fun _ ->
              let empty_board = { board with ships = [] } in
              let new_state =
                match player with
                | Player_kind.P1 -> { state with p1_board = empty_board }
                | Player_kind.P2 -> { state with p2_board = empty_board }
              in
              Vdom.Effect.Many
                [ set_selected_ship None; set_game_state new_state ]);
        ]
      [ Vdom.Node.text "Clear Board" ]
  in

  let placement_row =
    let all_ships_placed =
      List.length board.ships = List.length Game_state.fleet_spec
    in

    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "placement-flex" ]
      [
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "ship-sidebar-wrapper" ]
          [
            Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "ship-sidebar" ]
              (List.map Game_state.fleet_spec ~f:(fun (id, _) ->
                   let placed =
                     List.exists board.ships ~f:(fun s ->
                         same_ship_kind s.id id)
                   in
                   let file = ship_svg_file id in
                   let cls =
                     if placed then "placed-ship"
                     else if
                       Option.equal String.equal (Some id) selected_ship
                     then "selected-ship"
                     else "ship-choice"
                   in
                   Vdom.Node.button
                     ~attrs:
                       [
                         Vdom.Attr.class_ cls;
                         (if placed
                          then Vdom.Attr.create "disabled" "disabled"
                          else
                            Vdom.Attr.on_click (fun _ ->
                                set_selected_ship (Some id)));
                       ]
                     [
                       Vdom.Node.img
                         ~attrs:
                           [
                             Vdom.Attr.src ("hw5_html_css/" ^ file);
                             Vdom.Attr.alt id;
                             Vdom.Attr.class_ "ship-icon";
                           ]
                         ();
                       Vdom.Node.span
                         ~attrs:[ Vdom.Attr.class_ "ship-label" ]
                         [ Vdom.Node.text id ];
                     ]));
          ];
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "board-wrapper-outer" ]
          [
            render_board board
              ~clickable:(not all_ships_placed)
              ~on_click:handle_click ~show_ships:true;
          ];
      ]
  in

  let confirm_button =
    let all_ships_placed =
      List.length board.ships = List.length Game_state.fleet_spec
    in
    Vdom.Node.button
      ~attrs:
        [
          Vdom.Attr.on_click (fun _ -> on_confirm ());
          if not all_ships_placed
          then Vdom.Attr.create "disabled" "disabled"
          else Vdom.Attr.empty;
        ]
      [ Vdom.Node.text "Confirm Placement" ]
  in

  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "placement-screen" ]
    [
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "placement-title" ]
        [
          Vdom.Node.text
            (match player with
            | Player_kind.P1 -> "Place Your Ships"
            | Player_kind.P2 -> "Place Your Ships");
        ];
      placement_row;
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "placement-buttons" ]
        [ rotate_button; random_button; clear_button; confirm_button ];
    ]

let pick_ai_move (mode : Game_mode.t) (state : Game_state.t)
    ~(time_ms : int) : Move.t option =
  match mode with
  | Game_mode.PvE_easy ->
      Hw4_alpha_beta_search.pick_random_move state ~seed:(Random.bits ())
  | Game_mode.PvE_hard ->
      Hw4_alpha_beta_search.choose_move_timed state ~time_ms
  | Game_mode.PvP -> None

(* Game Screen *)
let game_screen ~state ~set_game_state ~on_reset ~local_player ~game_id_opt =
  let apply_ai ns =
    match ns.mode, ns.decision with
    | (Game_mode.PvE_easy | Game_mode.PvE_hard as mode),
      Decision.In_progress { whose_turn } ->
        if Player_kind.equal whose_turn Player_kind.P2 then
          begin
            match pick_ai_move mode ns ~time_ms:20 with
            | None -> Vdom.Effect.Ignore
            | Some mv ->
                let open Js_of_ocaml in
                let callback () =
                  match Game_state.make_move ns mv with
                  | Ok ns' ->
                      let effect = set_game_state ns' in
                      Vdom.Effect.Expert.handle_non_dom_event_exn effect
                  | Error _ -> ()
                in
                let _ = Dom_html.setTimeout callback 1000. in
                Vdom.Effect.Ignore
          end
        else Vdom.Effect.Ignore
    | _ -> Vdom.Effect.Ignore
  in

let handle_click pos =
  match state.mode with
  | Game_mode.PvP ->
      (match game_id_opt with
       | None -> Vdom.Effect.Ignore
       | Some game_id ->
           (match Game_state.make_move state pos with
            | Ok new_state ->
                let player_str =
                  match local_player with
                  | Player_kind.P1 -> "P1"
                  | Player_kind.P2 -> "P2"
                in
                F.send_move ~game_id
                  ~row:pos.Cell_position.row
                  ~col:pos.Cell_position.column
                  ~player:player_str;
                set_game_state new_state
            | Error _ -> Vdom.Effect.Ignore))
  | _ ->
      (match Game_state.make_move state pos with
       | Ok ns -> Vdom.Effect.Many [ set_game_state ns; apply_ai ns ]
       | Error _ -> Vdom.Effect.Ignore)
  in

  let is_game_over =
    match state.decision with
    | Decision.Winner _ -> true
    | _ -> false
  in

  let own_board, opp_board =
    match local_player with
    | Player_kind.P1 -> (state.p1_board, state.p2_board)
    | Player_kind.P2 -> (state.p2_board, state.p1_board)
  in

  let turn_text =
    match state.decision with
    | Decision.In_progress { whose_turn } ->
        if Player_kind.equal whose_turn local_player then "YOUR TURN"
        else "OPPONENT'S TURN"
    | Decision.Winner winner ->
        if Player_kind.equal winner local_player then "YOU WIN!"
        else "YOU LOSE!"
  in

  let can_shoot =
    match state.decision with
    | Decision.In_progress { whose_turn } ->
        Player_kind.equal whose_turn local_player
    | _ -> false
  in

  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "battleship-container" ]
    [
      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "header" ]
        [ Vdom.Node.h1 [ Vdom.Node.text "BATTLESHIP" ] ];
      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "turn-indicator" ]
        [ Vdom.Node.text turn_text ];
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "board-wrapper" ]
        [
          Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "board-container" ]
            [
              Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "board-title" ]
                [ Vdom.Node.text "Your Fleet" ];
              render_board own_board ~clickable:false ~on_click:handle_click
                ~show_ships:true;
            ];
          Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "board-container" ]
            [
              Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "board-title" ]
                [ Vdom.Node.text "Target Grid" ];
              render_board opp_board
                ~clickable:(can_shoot && not is_game_over)
                ~on_click:handle_click ~show_ships:is_game_over;
            ];
        ];
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "reset-container" ]
        [
          Vdom.Node.button
            ~attrs:
              [
                Vdom.Attr.class_ "reset-button";
                Vdom.Attr.on_click (fun _ -> on_reset ());
              ]
            [ Vdom.Node.text "Reset Game" ];
        ];
    ]

(* Root App *)
let battleship_app =
  let%sub signed_in, set_signed_in =
    Bonsai.state (module Bool) ~default_model:false
  in
  let%sub user_id, set_user_id = Bonsai.state_opt (module String) in
  let%sub setup_mode, set_setup_mode = Bonsai.state_opt (module Game_mode) in
  let%sub game_state, set_game_state = Bonsai.state_opt (module Game_state) in
  (* base_state = state at the moment both players are ready & fleets loaded *)
  let%sub base_state, set_base_state =
    Bonsai.state_opt (module Game_state)
  in

  let%sub match_info, set_match_info = Bonsai.state_opt (module String) in
  let%sub waiting_for_match, set_waiting_for_match =
    Bonsai.state (module Bool) ~default_model:false
  in
  let%sub ready_status_subscribed, set_ready_status_subscribed =
    Bonsai.state (module Bool) ~default_model:false
  in
  let%sub move_subscribed, set_move_subscribed =
    Bonsai.state (module Bool) ~default_model:false
  in

  let local_player_val =
    Bonsai.Value.map match_info ~f:(function
      | None -> Player_kind.P1
      | Some s ->
          if String.is_suffix s ~suffix:"P2" then Player_kind.P2
          else Player_kind.P1)
  in

  let game_id_val =
    Bonsai.Value.map match_info ~f:(function
      | None -> None
      | Some s ->
          match String.split s ~on:'|' with
          | gid :: _ -> Some gid
          | _ -> None)
  in

  let set_game_state_non_opt =
    Bonsai.Value.map set_game_state ~f:(fun f st -> f (Some st))
  in

  let on_confirm =
    Bonsai.Value.map5 game_state set_game_state setup_mode local_player_val game_id_val
      ~f:(fun gs set mode local_player game_id_opt ->
        match gs with
        | None -> (fun () -> Vdom.Effect.Ignore)
        | Some st ->
            fun () ->
              let updated =
                match st.phase, mode with
                | Phase.Placement placement, Some Game_mode.PvP ->
                    let is_p1_local =
                      Player_kind.equal local_player Player_kind.P1
                    in
                    (match game_id_opt with
                     | Some game_id ->
                         let ships =
                           if is_p1_local then st.p1_board.ships
                           else st.p2_board.ships
                         in
                         let ships_str =
                           ships |> [%sexp_of: Ship.t list] |> Sexp.to_string
                         in
                         let pstr = if is_p1_local then "P1" else "P2" in
                         F.set_player_ships ~game_id ~player:pstr ~ships_json:ships_str;
                         F.mark_player_ready ~game_id ~player:pstr
                     | None -> ());
                    let placement' =
                      if is_p1_local then
                        { placement with Phase.p1_ready = true }
                      else
                        { placement with Phase.p2_ready = true }
                    in
                    { st with phase = Phase.Placement placement' }

                | Phase.Placement placement,
                  Some (Game_mode.PvE_easy | Game_mode.PvE_hard) ->
                    if placement.Phase.p1_ready then st
                    else
                      let seed = Random.int_incl 0 100_000 in
                      let st' =
                        Game_state.randomize_player_fleet st
                          ~player:Player_kind.P2 ~seed
                      in
                      { st' with phase = Phase.In_progress }

                | _ -> st
              in
              set (Some updated))
  in

  let%sub placement_view =
    match%sub game_state with
    | None -> Bonsai.const (Vdom.Node.text "")
    | Some st ->
        placement_screen
          ~state:st
          ~set_game_state:set_game_state_non_opt
          ~on_confirm
          ~local_player:local_player_val
  in

  let%sub auth_bar =
    let%arr signed_in = signed_in
    and user_id = user_id
    and set_signed_in = set_signed_in
    and set_user_id = set_user_id in

    let label =
      match signed_in, user_id with
      | false, _ -> "Sign In (Guest)"
      | true, None -> "Signed in..."
      | true, Some uid -> "User: " ^ String.prefix uid 6
    in

    let on_sign_in _ =
      F.sign_in_guest ();
      let uid_opt = F.get_current_uid () in
      let eff =
        match uid_opt with
        | Some uid -> set_user_id (Some uid)
        | None -> Vdom.Effect.Ignore
      in
      Vdom.Effect.Many [ eff; set_signed_in true ]
    in

    let on_sign_out _ =
      F.sign_out ();
      Vdom.Effect.Many [ set_user_id None; set_signed_in false ]
    in

    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "auth-bar" ]
      (if signed_in then
         [
           Vdom.Node.button
             ~attrs:[ Vdom.Attr.create "disabled" "disabled" ]
             [ Vdom.Node.text label ];
           Vdom.Node.button
             ~attrs:[ Vdom.Attr.on_click on_sign_out ]
             [ Vdom.Node.text "Sign Out" ];
         ]
       else
         [
           Vdom.Node.button
             ~attrs:[ Vdom.Attr.on_click on_sign_in ]
             [ Vdom.Node.text label ];
         ])
  in

  let%arr setup_mode = setup_mode
  and set_setup_mode = set_setup_mode
  and game_state = game_state
  and set_game_state = set_game_state
  and _base_state = base_state
  and set_base_state = set_base_state
  and placement_view = placement_view
  and auth_bar = auth_bar
  and match_info = match_info
  and set_match_info = set_match_info
  and _waiting_for_match = waiting_for_match
  and set_waiting_for_match = set_waiting_for_match
  and local_player = local_player_val
  and game_id_opt = game_id_val
  and ready_status_subscribed = ready_status_subscribed
  and set_ready_status_subscribed = set_ready_status_subscribed
  and move_subscribed = move_subscribed
  and set_move_subscribed = set_move_subscribed
  in
  let () =
    match match_info with
    | Some match_str ->
        let game_id =
          match String.split match_str ~on:'|' with
          | gid :: _ -> gid
          | _ -> ""
        in
        if not ready_status_subscribed then begin
          F.subscribe_ready_status ~game_id ~on_ready:(fun status ->
              if String.equal status "BOTH_READY" then
                F.get_ships ~game_id ~on_result:(fun joined ->
                    let open Js_of_ocaml in
                    let callback () =
                      match game_state with
                      | Some st ->
                          let p1_json, p2_json =
                            match String.lsplit2 joined ~on:';' with
                            | Some (a, b) -> (a, b)
                            | None -> (joined, "[]")
                          in
                          let p1_ships =
                            Sexp.of_string p1_json |> [%of_sexp: Ship.t list]
                          in
                          let p2_ships =
                            Sexp.of_string p2_json |> [%of_sexp: Ship.t list]
                          in
                          let base_st =
                            { st with
                              p1_board = { st.p1_board with ships = p1_ships }
                            ; p2_board = { st.p2_board with ships = p2_ships }
                            ; phase = Phase.In_progress
                            }
                          in
                          (* Set up move subscription NOW with the base_st we just created *)
                          if not move_subscribed then begin
                            F.subscribe_game game_id (fun moves_joined ->
                                let moves =
                                  moves_joined
                                  |> String.split ~on:';'
                                  |> List.filter ~f:(fun s -> not (String.is_empty s))
                                in
                                let final_state =
                                  List.fold moves ~init:base_st ~f:(fun st move_str ->
                                      match String.split move_str ~on:',' with
                                      | [ row; col; _shooter_str ] ->
                                          let mv =
                                            { Cell_position.row = Int.of_string row
                                            ; column = Int.of_string col
                                            }
                                          in
                                          (match Game_state.make_move st mv with
                                          | Ok st' -> st'
                                          | Error _ -> st)
                                      | _ -> st)
                                in
                                let eff = set_game_state (Some final_state) in
                                Vdom.Effect.Expert.handle_non_dom_event_exn eff);
                            let eff = set_move_subscribed true in
                            Vdom.Effect.Expert.handle_non_dom_event_exn eff
                          end;
                          let eff =
                            Vdom.Effect.Many
                              [ set_base_state (Some base_st)
                              ; set_game_state (Some base_st)
                              ]
                          in
                          Vdom.Effect.Expert.handle_non_dom_event_exn eff
                      | None -> ()
                    in
                    let _ = Dom_html.setTimeout callback 0. in
                    ()));
          let eff = set_ready_status_subscribed true in
          Vdom.Effect.Expert.handle_non_dom_event_exn eff
        end
    | None -> ()
  in

  let main_view =
    match setup_mode with
    | None ->
        setup_screen ~on_start:(fun mode ->
            let init = Game_state.create_empty ~rows:10 ~cols:10 ~mode in
            match mode with
            | Game_mode.PvP ->
                F.request_quick_match (fun match_str ->
                    let eff =
                      Vdom.Effect.Many
                        [ set_match_info (Some match_str)
                        ; set_waiting_for_match false
                        ]
                    in
                    Vdom.Effect.Expert.handle_non_dom_event_exn eff);
                Vdom.Effect.Many
                  [ set_setup_mode (Some mode)
                  ; set_game_state (Some init)
                  ; set_base_state None
                  ; set_waiting_for_match true
                  ]
            | _ ->
                Vdom.Effect.Many
                  [ set_setup_mode (Some mode)
                  ; set_game_state (Some init)
                  ; set_base_state None
                  ])

    | Some Game_mode.PvP ->
        (match match_info with
         | None -> waiting_screen
         | Some _ ->
             match game_state with
             | None -> Vdom.Node.text "Loading..."
             | Some st ->
                 (match st.phase with
                  | Phase.Placement status ->
                      let is_p1_local =
                        Player_kind.equal local_player Player_kind.P1
                      in
                      let local_ready =
                        if is_p1_local then status.p1_ready else status.p2_ready
                      in
                      if local_ready then
                        waiting_for_opponent_screen
                          "Waiting for opponent to place ships..."
                      else placement_view
                  | Phase.In_progress | Phase.Game_over ->
                      game_screen
                        ~state:st
                        ~set_game_state:(fun ns ->
                          set_game_state (Some ns))
                        ~on_reset:(fun () ->
                          Vdom.Effect.Many
                            [ set_game_state None
                            ; set_base_state None
                            ; set_setup_mode None
                            ; set_match_info None
                            ; set_waiting_for_match false
                            ; set_ready_status_subscribed false
                            ; set_move_subscribed false
                            ])
                        ~local_player
                        ~game_id_opt))

    | Some _non_pvp_mode ->
        (match game_state with
         | None -> Vdom.Node.text "Loading..."
         | Some st ->
             match st.phase with
             | Phase.Placement plc ->
                 if plc.p1_ready then waiting_screen else placement_view
             | Phase.In_progress | Phase.Game_over ->
                 game_screen
                   ~state:st
                   ~set_game_state:(fun ns -> set_game_state (Some ns))
                   ~on_reset:(fun () ->
                     Vdom.Effect.Many
                       [ set_game_state None
                       ; set_base_state None
                       ; set_setup_mode None ])
                   ~local_player
                   ~game_id_opt)
  in

  Vdom.Node.div [ auth_bar; main_view ]

let () = Bonsai_web.Start.start battleship_app
