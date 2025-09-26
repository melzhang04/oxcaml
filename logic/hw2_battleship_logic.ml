open! Core

module Player_kind = struct
  type t =
    | P1
    | P2
  [@@deriving sexp, compare, equal]

  (* It's clearer to use type inference and just write:
     [let opposite t =]
  *)
  let opposite (t : t) : t =
    match t with
    | P1 -> P2
    | P2 -> P1
  ;;
end

module Cell_position = struct
  type t =
    { row : int
    ; column : int
    }
  [@@deriving sexp, compare, hash]

  (* Creates a [Cell_position.Map.t]. *)
    include Comparable.Make(struct
    type nonrec t = t [@@deriving sexp, compare]
  end)

  module Hash_set = Hash_set.Make(struct
    type nonrec t = t [@@deriving sexp, compare, hash]
  end)
end

module Cell_type = struct
  type t =
    | Hit
    | Miss
    | Ship
  [@@deriving sexp, compare, equal]
end

module Move = Cell_position

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
  [@@deriving sexp, compare, equal]

  let is_game_over t =
    match t with
    | Winner _ -> true
    | In_progress _ -> false
  ;;
end

module Ship = struct
  type t =
    { id : int
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

  let is_legal_cell_position t ({ row; column } : Cell_position.t) =
    0 <= row && 0 <= column && row < t.rows && column < t.cols
  ;;

  let ship_cells_set (ships : Ship.t list) : Cell_position.Set.t =
    ships |> List.concat_map ~f:(fun s -> s.cells) |> Cell_position.Set.of_list
  ;;

  let all_ships_sunk (t : t) : bool =
    let ship_cells = ship_cells_set t.ships in
    let hit_cells = Map.filter t.shots ~f:(Cell_type.equal Hit) |> Map.key_set in
    Set.is_subset ship_cells ~of_:hit_cells
  ;;

  let already_shot t (pos : Cell_position.t) = Map.mem t.shots pos

  let place_shot (t : t) (pos : Cell_position.t) : t * Cell_type.t =
    let ship_cells = ship_cells_set t.ships in
    let result = if Set.mem ship_cells pos then Cell_type.Hit else Cell_type.Miss in
    let shots = Map.set t.shots ~key:pos ~data:result in
    { t with shots }, result
  ;;
end

module Game_state = struct
  type t =
    { p1_board : Board.t
    ; p2_board : Board.t
    ; columns : int
    ; decision : Decision.t
    ; last_move : Move.t option (* For animation purposes. *)
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
      | Space_already_shot
      | Illegal_cell_position
    [@@deriving sexp, compare, equal]
  end

  let validate_board (b : Board.t) : Create_error.t list =
    let errors = ref [] in
    (* bounds check *)
    let push e = errors := e :: !errors in
    if not (b.rows > 0 && b.cols > 0 && b.rows <= 20 && b.cols <= 20)
    then push Create_error.Board_too_big_or_small;
    (* ship cell legality *)
    let all_cells = List.concat_map b.ships ~f:(fun s -> s.cells) in
    if List.exists all_cells ~f:(Fn.non (Board.is_legal_cell_position b))
    then push Create_error.Illegal_ship_cell;
    (* overlap *)
    let set = Cell_position.Hash_set.create () in
    if List.exists all_cells ~f:(fun pos ->
         if Hash_set.mem set pos
         then true
         else (
           Hash_set.add set pos;
           false))
    then push Overlapping_ships;
    List.rev !errors
  ;;

  let create
      ~(rows : int)
      ~(cols : int)
      ~(p1_ships : Ship.t list)
      ~(p2_ships : Ship.t list)
      : (t, Create_error.t list) Result.t
    =
    let p1_board =
      { Board.rows; cols; ships = p1_ships; shots = Map.empty (module Cell_position) }
    in
    let p2_board =
      { Board.rows; cols; ships = p2_ships; shots = Map.empty (module Cell_position) }
    in
    let errs = validate_board p1_board @ validate_board p2_board in
    if List.is_empty errs
    then
      Ok
        { p1_board
        ; p2_board
        ; columns = cols
        ; decision = In_progress { whose_turn = P1 }
        ; last_move = None
        }
    else Error errs
  ;;

  let get_all_moves (t : t) : Move.t list =
    let target_board =
      match t.decision with
      | Decision.In_progress { whose_turn = P1 } -> t.p2_board
      | Decision.In_progress { whose_turn = P2 } -> t.p1_board
      | Decision.Winner _ -> t.p2_board
    in
    List.cartesian_product
      (List.range 0 target_board.rows)
      (List.range 0 target_board.cols)
    |> List.map ~f:(fun (row, column) -> { Cell_position.row; column })
    |> List.filter ~f:(fun pos -> not (Board.already_shot target_board pos))
  ;;

  let make_move (t : t) (pos : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Decision.Winner _ -> Error Move_error.Game_is_over
    | Decision.In_progress { whose_turn } ->
      let shooter, target =
        match whose_turn with
        | P1 -> `P1, t.p2_board
        | P2 -> `P2, t.p1_board
      in
      if not (Board.is_legal_cell_position target pos)
      then Error Move_error.Illegal_cell_position
      else if Board.already_shot target pos
      then Error Move_error.Space_already_shot
      else (
        let target', _result = Board.place_shot target pos in
        let p1_board, p2_board =
          match shooter with
          | `P1 -> t.p1_board, target'
          | `P2 -> target', t.p2_board
        in
        let decision =
          if Board.all_ships_sunk target'
          then Decision.Winner whose_turn
          else Decision.In_progress { whose_turn = Player_kind.opposite whose_turn }
        in
         Ok { p1_board; p2_board; columns = t.columns; decision; last_move = Some pos })
  ;;
end
