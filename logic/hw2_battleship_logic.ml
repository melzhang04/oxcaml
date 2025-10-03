open! Core

module Player_kind = struct
  type t =
    | P1
    | P2
  [@@deriving sexp, compare, equal]

  let opposite (t : t) : t =
    match t with
    | P1 -> P2
    | P2 -> P1
  ;;
end

module Cell_position = struct
  module T = struct
    type t =
      { row : int
      ; column : int
      }
    [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
end

module Move = Cell_position

module Cell_type = struct
  type t =
    | Hit
    | Miss
    | Ship
  [@@deriving sexp, compare, equal]
end

module Ship = struct
  type t =
    { id : string
    ; cells : Cell_position.t list
    }
  [@@deriving sexp, compare, equal]
end

module Board = struct
  type t =
    { rows : int
    ; cols : int
    ; ships : Ship.t list
    ; shots : Cell_type.t Cell_position.Map.t
    }
  [@@deriving sexp, compare, equal]

  (* helpers *)
  let ship_cells_set (ships : Ship.t list) : Cell_position.Set.t =
    List.concat_map ships ~f:(fun s -> s.Ship.cells)
    |> Cell_position.Set.of_list
  ;;

  let cell_has_ship (t : t) (pos : Cell_position.t) : bool =
    let occupied = ship_cells_set t.ships in
    Set.mem occupied pos
  ;;

  (* exposed functions *)
  let is_legal_cell_position (t : t) ({ row; column } : Cell_position.t) : bool =
    0 <= row && row < t.rows && 0 <= column && column < t.cols
  ;;

  let already_shot (t : t) (pos : Cell_position.t) : bool =
    Map.mem t.shots pos
  ;;

  let all_ships_sunk (t : t) : bool =
    let occupied = ship_cells_set t.ships in
    Set.for_all occupied ~f:(fun pos ->
      match Map.find t.shots pos with
      | Some Cell_type.Hit -> true
      | _ -> false)
  ;;
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
  [@@deriving sexp, compare, equal]

  let is_game_over = function
    | Winner _ -> true
    | In_progress _ -> false
  ;;
end

module Game_state = struct
  type t =
    { p1_board : Board.t
    ; p2_board : Board.t
    ; decision : Decision.t
    ; last_move : Move.t option
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t =
      | Board_too_big_or_small
      | Illegal_ship_cell
      | Overlapping_ships
    [@@deriving sexp, compare, equal]
  end

  module Move_error = struct
    type t =
      | Game_is_over
      | Already_shot
      | Illegal_cell_position
    [@@deriving sexp, compare, equal]
  end

  module Ship_rules = struct
    open Ship

    let required_fleet : int String.Map.t =
      String.Map.of_alist_exn
        [ "destroyer", 2
        ; "submarine", 3
        ; "cruiser", 3
        ; "battleship", 4
        ; "aircraft_carrier", 5
        ]

    (* FIX: parse kind using rsplit2 (last underscore) so "aircraft_carrier_1" -> "aircraft_carrier" *)
    let ship_kind (id : string) : string option =
      match String.rsplit2 id ~on:'_' with
      | Some (kind, _) -> Some kind
      | None -> None

    let ship_within_bounds ~rows ~cols (pos : Cell_position.t) : bool =
      0 <= pos.row && pos.row < rows && 0 <= pos.column && pos.column < cols

    let valid_ship_position (cells : Cell_position.t list) =
      match cells with
      | [] | [_] -> true
      | _ ->
        let rows = List.map cells ~f:(fun cell -> cell.row) in
        let cols = List.map cells ~f:(fun cell -> cell.column) in
        let same_row = List.for_all rows ~f:(fun r -> r = List.hd_exn rows) in
        let same_col = List.for_all cols ~f:(fun c -> c = List.hd_exn cols) in
        if not (same_row || same_col) then false
        else if same_row then
          let sorted_cols = List.sort ~compare:Int.compare cols in
          List.for_alli sorted_cols ~f:(fun i c ->
            i = 0 || c = List.nth_exn sorted_cols (i - 1) + 1
          )
        else
          let sorted_rows = List.sort ~compare:Int.compare rows in
          List.for_alli sorted_rows ~f:(fun i r ->
            i = 0 || r = List.nth_exn sorted_rows (i - 1) + 1
          )

    let no_overlaps (ships : t list) =
      let module S = Set.Make (struct
        type t = Cell_position.t [@@deriving compare, sexp]
      end)
      in
      let rec go ss seen =
        match ss with
        | [] -> true
        | ship :: t1 ->
          let cells = ship.cells in
          let has_dupe = List.exists cells ~f:(fun p -> Set.mem seen p) in
          if has_dupe then false
          else
            let seen' = List.fold cells ~init:seen ~f:(fun acc p -> Set.add acc p) in
            go t1 seen'
      in
      go ships S.empty

    let validate_fleet ~rows ~cols (ships : t list) : Create_error.t list =
      let errors = ref [] in
      let kind_counts = String.Table.create () in
      let per_ship_ok =
        List.for_all ships ~f:(fun s ->
          let cells = s.cells in
          let ok_non_empty = not (List.is_empty cells) in
          let ok_bounds = List.for_all cells ~f:(ship_within_bounds ~rows ~cols) in
          let ok_positions = valid_ship_position cells in
          let ok_kind_len =
            match ship_kind s.id with
            | None -> false
            | Some k ->
              (match Map.find required_fleet k with
               | None -> false
               | Some required_len ->
                 let len = List.length cells in
                 Hashtbl.set kind_counts ~key:k
                   ~data:(1 + Option.value (Hashtbl.find kind_counts k) ~default:0);
                 len = required_len)
          in
          if ok_non_empty && ok_bounds && ok_positions && ok_kind_len then true
          else (errors := Create_error.Illegal_ship_cell :: !errors; false))
      in
      ignore per_ship_ok;
      if not (no_overlaps ships) then
        errors := Create_error.Overlapping_ships :: !errors;
      Map.iter_keys required_fleet ~f:(fun kind ->
        match Hashtbl.find kind_counts kind with
        | Some 1 -> ()
        | _ -> errors := Create_error.Illegal_ship_cell :: !errors);
      !errors
  end

  let validate_board (b : Board.t) : Create_error.t list =
    Ship_rules.validate_fleet ~rows:b.rows ~cols:b.cols b.ships
  ;;

  let create ~rows ~cols ~p1_ships ~p2_ships : (t, Create_error.t list) Result.t =
    let size_ok = rows = 10 && cols = 10 in
    if not size_ok then Error [ Create_error.Board_too_big_or_small ] else
    let p1_board =
      { Board.rows; Board.cols; ships = p1_ships; shots = Map.empty (module Cell_position) }
    in
    let p2_board =
      { Board.rows; Board.cols; ships = p2_ships; shots = Map.empty (module Cell_position) }
    in
    let errs = validate_board p1_board @ validate_board p2_board in
    if List.is_empty errs then
      Ok { p1_board
         ; p2_board
         ; decision = In_progress { whose_turn = P1 }
         ; last_move = None
         }
    else
      Error errs
  ;;

  (* Apply a single shot *)
  let apply_shot (board : Board.t) (pos : Cell_position.t)
    : (Board.t * bool, Move_error.t) Result.t =
    if not (Board.is_legal_cell_position board pos)
    then Error Move_error.Illegal_cell_position
    else if Board.already_shot board pos
    then Error Move_error.Already_shot
    else
      let hit = Board.cell_has_ship board pos in
      let cell_type = if hit then Cell_type.Hit else Cell_type.Miss in
      let shots' = Map.set board.shots ~key:pos ~data:cell_type in
      Ok ({ board with shots = shots' }, hit)
  ;;

  (* make_move *)
  let make_move (t : t) (pos : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Winner _ -> Error Move_error.Game_is_over
    | In_progress { whose_turn } ->
      let shooting_at_p2 = Player_kind.equal whose_turn Player_kind.P1 in
      let target_board = if shooting_at_p2 then t.p2_board else t.p1_board in
      match apply_shot target_board pos with
      | Error e -> Error e
      | Ok (target_board', _hit) ->
        let p1_board', p2_board' =
          if shooting_at_p2 then t.p1_board, target_board' else target_board', t.p2_board
        in
        let opponent_after = if shooting_at_p2 then p2_board' else p1_board' in
        let decision' =
          if Board.all_ships_sunk opponent_after
          then Decision.Winner whose_turn
          else Decision.In_progress { whose_turn = Player_kind.opposite whose_turn }
        in
        Ok { p1_board = p1_board'
           ; p2_board = p2_board'
           ; decision = decision'
           ; last_move = Some pos
           }
  ;;

  (* get_all_moves *)
  let get_all_moves (t : t) : Move.t list =
    match t.decision with
    | Decision.Winner _ -> []
    | Decision.In_progress { whose_turn } ->
      let target =
        if Player_kind.equal whose_turn Player_kind.P1
        then t.p2_board else t.p1_board
      in
      let positions =
        List.concat_map (List.init target.rows ~f:Fn.id) ~f:(fun r ->
          List.map (List.init target.cols ~f:Fn.id) ~f:(fun c ->
            { Cell_position.row = r; column = c }))
      in
      List.filter positions ~f:(fun pos -> not (Board.already_shot target pos))
  ;;
end