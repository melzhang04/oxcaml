open! Core

module Player_kind = struct
  type t = P1 | P2 [@@deriving sexp, compare, equal]
  let opposite = function P1 -> P2 | P2 -> P1
end

module Cell_position = struct
  module T = struct
    type t = { row:int; column:int } [@@deriving sexp, compare, hash]
  end
  include T
  include Comparable.Make(T)
end

module Move = Cell_position

module Cell_type = struct
  type t = Hit | Miss | Ship [@@deriving sexp, compare, equal]
end

module Ship = struct
  type t = { id:string; cells:Cell_position.t list } [@@deriving sexp, compare, equal]
end

module Pos = struct
  let in_bounds ~rows ~cols { Cell_position.row; column } =
    0 <= row && row < rows && 0 <= column && column < cols
  let all_positions ~rows ~cols =
    List.cartesian_product (List.init rows ~f:Fn.id) (List.init cols ~f:Fn.id)
end

module Board = struct
  type t = { rows:int; cols:int; ships:Ship.t list; shots:Cell_type.t Cell_position.Map.t }
  [@@deriving sexp, compare, equal]
  let ship_cells_set ships =
    List.concat_map ships ~f:(fun s -> s.Ship.cells) |> Cell_position.Set.of_list
  let cell_has_ship t pos = Set.mem (ship_cells_set t.ships) pos
  let is_legal_cell_position t p = Pos.in_bounds ~rows:t.rows ~cols:t.cols p
  let already_shot t pos = Map.mem t.shots pos
  let all_ships_sunk t =
    let occ = ship_cells_set t.ships in
    Set.for_all occ ~f:(fun pos -> match Map.find t.shots pos with Some Cell_type.Hit -> true | _ -> false)
end

module Decision = struct
  type t = In_progress of { whose_turn:Player_kind.t } | Winner of Player_kind.t
  [@@deriving sexp, compare, equal]
  let is_game_over = function Winner _ -> true | In_progress _ -> false
end

module Game_mode = struct
  type t = PvP | PvE_easy | PvE_hard [@@deriving sexp, compare, equal]
end

module Phase = struct
  type t = Placement of Player_kind.t | In_progress | Game_over
  [@@deriving sexp, compare, equal]
end

module Game_state = struct
  type t =
    { p1_board : Board.t
    ; p2_board : Board.t
    ; decision : Decision.t
    ; last_move : Move.t option
    ; phase : Phase.t
    ; mode : Game_mode.t
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t = Board_too_big_or_small | Illegal_ship_cell | Overlapping_ships
    [@@deriving sexp, compare, equal, hash]
  end

  module Move_error = struct
    type t = Game_is_over | Already_shot | Illegal_cell_position
    [@@deriving sexp, compare, equal]
  end

  module Ship_rules = struct
    open Ship
    let required_fleet =
      String.Map.of_alist_exn
        [ "destroyer",2; "submarine",3; "cruiser",3; "battleship",4; "aircraft_carrier",5 ]
    let ship_kind id =
      match String.rsplit2 id ~on:'_' with Some(k,_) -> Some k | None -> None
    let ship_within_bounds ~rows ~cols = Pos.in_bounds ~rows ~cols
    let valid_ship_position cells =
      match cells with
      | [] -> false
      | [_] -> true
      | _ ->
        let rows = List.map cells ~f:(fun c -> c.Cell_position.row) in
        let cols = List.map cells ~f:(fun c -> c.Cell_position.column) in
        let same_row = List.for_all rows ~f:(fun r -> r = List.hd_exn rows) in
        let same_col = List.for_all cols ~f:(fun c -> c = List.hd_exn cols) in
        let consecutive ints =
          let mn = List.min_elt ints ~compare:Int.compare |> Option.value_exn in
          let mx = List.max_elt ints ~compare:Int.compare |> Option.value_exn in
          mx - mn + 1 = List.length ints &&
          List.length (List.dedup_and_sort ~compare:Int.compare ints) = List.length ints
        in
        (same_row && consecutive cols) || (same_col && consecutive rows)
    let no_overlaps ships =
      let all = List.concat_map ships ~f:(fun s -> s.cells) in
      Set.length (Cell_position.Set.of_list all) = List.length all
    let validate_fleet ~rows ~cols ships =
      let errors = Hash_set.create (module Create_error) in
      let kind_counts = String.Table.create () in
      let per_ship_ok =
        List.for_all ships ~f:(fun s ->
          let cells = s.cells in
          let ok_non_empty = not(List.is_empty cells) in
          let ok_bounds = List.for_all cells ~f:(ship_within_bounds ~rows ~cols) in
          let ok_positions = valid_ship_position cells in
          let ok_kind_len =
            match ship_kind s.id with
            | None -> false
            | Some k ->
              (match Map.find required_fleet k with
               | None -> false
               | Some req ->
                 let len = List.length cells in
                 Hashtbl.set kind_counts ~key:k
                   ~data:(1 + Option.value (Hashtbl.find kind_counts k) ~default:0);
                 len = req)
          in
          if ok_non_empty && ok_bounds && ok_positions && ok_kind_len then true
          else (Hash_set.add errors Create_error.Illegal_ship_cell; false))
      in
      ignore per_ship_ok;
      if not(no_overlaps ships) then Hash_set.add errors Create_error.Overlapping_ships;
      Map.iter_keys required_fleet ~f:(fun k ->
        match Hashtbl.find kind_counts k with
        | Some 1 -> ()
        | _ -> Hash_set.add errors Create_error.Illegal_ship_cell);
      Hash_set.to_list errors
  end

  let make_board ~rows ~cols ~ships =
    { Board.rows; cols; ships; shots = Map.empty (module Cell_position) }

  let cells_from_start ~start ~len ~horizontal =
    List.init len ~f:(fun i ->
      if horizontal
      then { Cell_position.row = start.Cell_position.row; column = start.Cell_position.column + i }
      else { Cell_position.row = start.Cell_position.row + i; column = start.Cell_position.column })

  let can_place_on_board ~rows ~cols ~existing cells =
    List.for_all cells ~f:(Pos.in_bounds ~rows ~cols)
    && let occ =
         List.concat_map existing ~f:(fun s -> s.Ship.cells)
         |> Cell_position.Set.of_list
       in
       List.for_all cells ~f:(fun p -> not (Set.mem occ p))

  let fleet_spec =
    [ "destroyer",2; "submarine",3; "cruiser",3; "battleship",4; "aircraft_carrier",5 ]

  let rec random_fleet ~rows ~cols ~player ~seed =
    Core.Random.init seed;
    let suffix = match player with Player_kind.P1->"1" | P2->"2" in
    let rec place_all acc = function
      | [] -> acc
      | (kind,len)::tl ->
        let rec try_k k =
          if k=0 then acc else
          let horiz = Core.Random.bool() in
          let max_row = if horiz then rows else rows-len+1 in
          let max_col = if horiz then cols-len+1 else cols in
          let start = {Cell_position.row=Core.Random.int max_row; column=Core.Random.int max_col} in
          let cells = cells_from_start ~start ~len ~horizontal:horiz in
          if can_place_on_board ~rows ~cols ~existing:acc cells
          then place_all ({Ship.id=kind^"_"^suffix; cells}::acc) tl
          else try_k(k-1)
        in try_k 500
    in
    let ships = place_all [] fleet_spec in
    match Ship_rules.validate_fleet ~rows ~cols ships with
    | [] -> ships
    | _ -> random_fleet ~rows ~cols ~player ~seed:(seed+1)

  let create ~rows ~cols ~p1_ships ~p2_ships =
    if rows <> 10 || cols <> 10
    then Error [Create_error.Board_too_big_or_small]
    else
      let p1_board = make_board ~rows ~cols ~ships:p1_ships in
      let p2_board = make_board ~rows ~cols ~ships:p2_ships in
      let errs = Ship_rules.validate_fleet ~rows ~cols p1_ships
                 @ Ship_rules.validate_fleet ~rows ~cols p2_ships in
      if List.is_empty errs then
        Ok { p1_board; p2_board
           ; decision = Decision.In_progress { whose_turn = Player_kind.P1 }
           ; last_move = None
           ; phase = Phase.In_progress
           ; mode = Game_mode.PvP
           }
      else Error errs

  let create_empty ~rows ~cols ~mode =
    let empty_board =
      { Board.rows = rows; cols; ships = []; shots = Map.empty (module Cell_position) }
    in
    { p1_board = empty_board
    ; p2_board = empty_board
    ; decision = Decision.In_progress { whose_turn = Player_kind.P1 }
    ; last_move = None
    ; phase = Phase.Placement Player_kind.P1
    ; mode
    }

  let place_player_fleet t ~player ~ships =
    match Ship_rules.validate_fleet ~rows:t.p1_board.rows ~cols:t.p1_board.cols ships with
    | [] ->
        (match player with
         | Player_kind.P1 ->
             { t with
               p1_board = { t.p1_board with ships }
             ; phase =
                 (match t.phase with
                  | Phase.Placement Player_kind.P1 -> Phase.Placement Player_kind.P2
                  | _ -> t.phase)
             }
         | Player_kind.P2 ->
             { t with
               p2_board = { t.p2_board with ships }
             ; phase = Phase.In_progress })
    | errs ->
        failwithf "Invalid fleet placement: %s"
          (Sexp.to_string ([%sexp_of: Create_error.t list] errs)) ()

  let randomize_player_fleet t ~player ~seed =
    let ships = random_fleet ~rows:t.p1_board.rows ~cols:t.p1_board.cols ~player ~seed in
    place_player_fleet t ~player ~ships

  let create_random ~rows ~cols ~seed =
    let s0 = create_empty ~rows ~cols ~mode:Game_mode.PvP in
    let s1 = randomize_player_fleet s0 ~player:Player_kind.P1 ~seed in
    let s2 = randomize_player_fleet s1 ~player:Player_kind.P2 ~seed:(seed + 1) in
    Ok s2

  let apply_shot board pos =
    if not (Board.is_legal_cell_position board pos)
    then Error Move_error.Illegal_cell_position
    else if Board.already_shot board pos
    then Error Move_error.Already_shot
    else
      let hit = Board.cell_has_ship board pos in
      let cell_type = if hit then Cell_type.Hit else Cell_type.Miss in
      let shots' = Map.set board.shots ~key:pos ~data:cell_type in
      Ok ({ board with shots = shots' }, hit)

  let make_move t pos =
    match t.decision with
    | Decision.Winner _ -> Error Move_error.Game_is_over
    | In_progress { whose_turn } ->
        let shooting_at_p2 = Player_kind.equal whose_turn Player_kind.P1 in
        let target = if shooting_at_p2 then t.p2_board else t.p1_board in
        match apply_shot target pos with
        | Error e -> Error e
        | Ok (target', _) ->
            let p1', p2' =
              if shooting_at_p2 then t.p1_board, target' else target', t.p2_board
            in
            let opp_after = if shooting_at_p2 then p2' else p1' in
            let decision' =
              if Board.all_ships_sunk opp_after
              then Decision.Winner whose_turn
              else In_progress { whose_turn = Player_kind.opposite whose_turn }
            in
            Ok { t with
                 p1_board = p1'
               ; p2_board = p2'
               ; decision = decision'
               ; last_move = Some pos
               ; phase = (if Decision.is_game_over decision' then Phase.Game_over else Phase.In_progress)
               }

  let get_all_moves t =
    match t.decision with
    | Decision.Winner _ -> []
    | In_progress { whose_turn } ->
        let target =
          if Player_kind.equal whose_turn Player_kind.P1 then t.p2_board else t.p1_board
        in
        Pos.all_positions ~rows:target.rows ~cols:target.cols
        |> List.map ~f:(fun (r,c) -> { Cell_position.row=r; column=c })
        |> List.filter ~f:(fun pos -> not (Board.already_shot target pos))

  let ai_move t =
    match t.mode with
    | Game_mode.PvP -> None
    | PvE_easy ->
        let moves = get_all_moves t in
        Option.some_if (not (List.is_empty moves)) (List.random_element_exn moves)
    | PvE_hard ->
        let moves = get_all_moves t in
        Option.some_if (not (List.is_empty moves)) (List.random_element_exn moves)
end