open! Core
open Hw2_battleship_logic

let pick_random_move (state : Game_state.t) ~seed =
  Core.Random.init seed;
  match Game_state.get_all_moves state with
  | [] -> None
  | moves -> List.random_element moves

let play_random (state : Game_state.t) ~seed =
  match pick_random_move state ~seed with
  | None -> state
  | Some mv -> Option.value (Result.ok (Game_state.make_move state mv)) ~default:state

let greedy_move (state : Game_state.t) : Move.t option =
  match state.decision with
  | Decision.Winner _ -> None
  | Decision.In_progress { whose_turn } ->
    let target =
      if Player_kind.equal whose_turn Player_kind.P1 then state.p2_board else state.p1_board
    in
    let shots = target.Board.shots in
    let is_hit p = match Map.find shots p with Some Cell_type.Hit -> true | _ -> false in
    let already p = Map.mem shots p in
    let in_bounds p = Board.is_legal_cell_position target p in
    let neighbors { Cell_position.row; column } =
      [ { Cell_position.row = row - 1; column }
      ; { Cell_position.row = row + 1; column }
      ; { Cell_position.row = row; column = column - 1 }
      ; { Cell_position.row = row; column = column + 1 }
      ]
    in
    let hits =
      Map.keys shots |> List.filter ~f:is_hit
    in
    let candidates_from_hits =
      hits
      |> List.concat_map ~f:neighbors
      |> List.filter ~f:in_bounds
      |> List.filter ~f:(fun p -> not (already p))
    in
    match candidates_from_hits with
    | p :: _ -> Some p
    | [] ->
      let all =
        List.concat_map (List.init target.rows ~f:Fn.id) ~f:(fun r ->
          List.map (List.init target.cols ~f:Fn.id)
            ~f:(fun c -> { Cell_position.row = r; column = c }))
      in
      let checker =
        all
        |> List.filter ~f:(fun { Cell_position.row; column } -> ((row + column) land 1) = 0)
        |> List.filter ~f:(fun p -> not (already p))
      in
      (match checker with
       | p :: _ -> Some p
       | [] -> List.find all ~f:(fun p -> not (already p)))

let fleet_lengths = [ 2; 3; 3; 4; 5 ]

let cell_ok_for_ship (shots : Cell_type.t Cell_position.Map.t) (p : Cell_position.t) =
  match Map.find shots p with
  | Some Cell_type.Miss -> false
  | _ -> true

let score_placement
    ~(shots : Cell_type.t Cell_position.Map.t)
    (cells : Cell_position.t list)
  =
  let hits_in =
    List.count cells ~f:(fun p ->
      match Map.find shots p with Some Cell_type.Hit -> true | _ -> false)
  in
  if List.for_all cells ~f:(cell_ok_for_ship shots)
  then 1 + (hits_in * 5)
  else 0

let probability_move (state : Game_state.t) ~(seed:int) : Move.t option =
  match state.decision with
  | Decision.Winner _ -> None
  | Decision.In_progress { whose_turn } ->
    let target =
      if Player_kind.equal whose_turn Player_kind.P1 then state.p2_board else state.p1_board
    in
    let shots = target.Board.shots in
    let scores = Array.make_matrix ~dimx:target.rows ~dimy:target.cols 0 in
    let bump { Cell_position.row; column } v =
      scores.(row).(column) <- scores.(row).(column) + v
    in
    let in_bounds p = Board.is_legal_cell_position target p in
    let line_from start len ~horizontal =
      List.init len ~f:(fun i ->
        if horizontal
        then { Cell_position.row = start.Cell_position.row; column = start.Cell_position.column + i }
        else { Cell_position.row = start.Cell_position.row + i; column = start.Cell_position.column })
    in
    List.iter fleet_lengths ~f:(fun len ->
      for r = 0 to target.rows - 1 do
        for c = 0 to target.cols - 1 do
          let start = { Cell_position.row = r; column = c } in
          let cells_h = line_from start len ~horizontal:true in
          if List.for_all cells_h ~f:in_bounds then
            let w = score_placement ~shots cells_h in
            if w > 0 then List.iter cells_h ~f:(fun p -> bump p w);
          let cells_v = line_from start len ~horizontal:false in
          if List.for_all cells_v ~f:in_bounds then
            let w = score_placement ~shots cells_v in
            if w > 0 then List.iter cells_v ~f:(fun p -> bump p w);
        done
      done);
    let unknowns =
      List.concat_map (List.init target.rows ~f:Fn.id) ~f:(fun r ->
        List.filter_map (List.init target.cols ~f:Fn.id) ~f:(fun c ->
          let p = { Cell_position.row = r; column = c } in
          if Map.mem shots p then None else Some p))
    in
    if List.is_empty unknowns then None
    else
      let max_score =
        List.fold unknowns ~init:Int.min_value
          ~f:(fun acc p -> Int.max acc scores.(p.row).(p.column))
      in
      let bests = List.filter unknowns ~f:(fun p -> scores.(p.row).(p.column) = max_score) in
      let bests =
        match List.filter bests ~f:(fun p -> ((p.row + p.column) land 1) = 0) with
        | [] -> bests
        | ps -> ps
      in
      Core.Random.init seed;
      List.random_element bests

let choose_move_timed (state : Game_state.t) ~time_ms:_ : Move.t option =
  match probability_move state ~seed:12345 with
  | Some m -> Some m
  | None ->
    (match greedy_move state with
     | Some m -> Some m
     | None -> pick_random_move state ~seed:67890)

let play_with_policy (state : Game_state.t) (policy : Game_state.t -> Move.t option) =
  match policy state with
  | None -> state
  | Some mv -> Option.value (Result.ok (Game_state.make_move state mv)) ~default:state

let simulate_one ~seed ~time_ms =
  let rec loop st turn =
    if Decision.is_game_over st.Game_state.decision then st
    else
      let st' =
        if turn land 1 = 1
        then play_with_policy st (fun s -> choose_move_timed s ~time_ms)
        else play_with_policy st (fun s -> pick_random_move s ~seed:(seed + turn))
      in
      loop st' (turn + 1)
  in
  let init =
    Game_state.create_random ~rows:10 ~cols:10 ~seed
    |> Result.ok |> Option.value_exn
  in
  loop init 1

let simulate_matches ~games ~seed ~time_ms =
  let wins_ai = ref 0
  and wins_rand = ref 0
  and draws = ref 0 in
  for i = 0 to games - 1 do
    let final = simulate_one ~seed:(seed + 997*i) ~time_ms in
    (match final.decision with
     | Decision.Winner Player_kind.P1 -> incr wins_ai
     | Decision.Winner Player_kind.P2 -> incr wins_rand
     | Decision.In_progress _ -> incr draws)
  done;
  printf "Timed AI (P1) wins: %d / %d\n" !wins_ai games;
  printf "Random AI (P2) wins: %d / %d\n" !wins_rand games;
  printf "Unfinished: %d\n%!" !draws