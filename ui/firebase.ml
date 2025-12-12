open Js_of_ocaml

class type firebase_api = object
  method signInGuest : unit -> unit Js.meth
  method signOutUser : unit -> unit Js.meth
  method getCurrentUid : unit -> Js.js_string Js.t Js.opt Js.meth

  method requestQuickMatch :
    (Js.js_string Js.t -> unit) Js.callback -> unit Js.meth

  method cancelMatchmaking : unit -> unit Js.meth

  method subscribeGame :
    Js.js_string Js.t ->
    (Js.js_string Js.t -> unit) Js.callback ->
    (unit -> unit) Js.meth

  method sendMove :
    Js.js_string Js.t -> int -> int -> Js.js_string Js.t -> unit Js.meth

  method setPlayerShips :
    Js.js_string Js.t -> Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth

  method markPlayerReady :
    Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth

  method subscribeReadyStatus :
    Js.js_string Js.t ->
    (Js.js_string Js.t -> unit) Js.callback ->
    (unit -> unit) Js.meth

  method getShips :
    Js.js_string Js.t ->
    (Js.js_string Js.t -> unit) Js.callback ->
    unit Js.meth

  method resetGame :
    Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth

  method setupForfeitOnDisconnect :
    Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth

  method clearForfeitOnDisconnect : unit -> unit Js.meth

  method monitorOpponentHeartbeat :
    Js.js_string Js.t ->
    Js.js_string Js.t ->
    (Js.js_string Js.t -> unit) Js.callback ->
    (unit -> unit) Js.meth
end

let firebase : firebase_api Js.t =
  Js.Unsafe.js_expr "window.firebaseBindings"

let sign_in_guest () : unit =
  firebase##signInGuest ()

let sign_out () : unit =
  firebase##signOutUser ()

let get_current_uid () : string option =
  match firebase##getCurrentUid () |> Js.Opt.to_option with
  | None -> None
  | Some s -> Some (Js.to_string s)

let request_quick_match (on_matched : string -> unit) : unit =
  let cb =
    Js.wrap_callback (fun s ->
        let str = Js.to_string s in
        on_matched str)
  in
  firebase##requestQuickMatch cb

let cancel_matchmaking () : unit =
  firebase##cancelMatchmaking ()

let subscribe_game (game_id : string) (on_update : string -> unit) : (unit -> unit) =
  let cb =
    Js.wrap_callback (fun s ->
        let str = Js.to_string s in
        on_update str)
  in
  let unsub_js = firebase##subscribeGame (Js.string game_id) cb in
  fun () -> unsub_js ()

let subscribe_ready_status ~(game_id : string) ~(on_ready : string -> unit) : (unit -> unit) =
  let cb =
    Js.wrap_callback (fun s ->
        let str = Js.to_string s in
        on_ready str)
  in
  let unsub_js = firebase##subscribeReadyStatus (Js.string game_id) cb in
  fun () -> unsub_js ()

let send_move ~(game_id : string) ~(row : int) ~(col : int)
    ~(player : string) : unit =
  firebase##sendMove
    (Js.string game_id)
    row
    col
    (Js.string player)

let set_player_ships ~(game_id : string) ~(player : string) ~(ships_json : string) : unit =
  firebase##setPlayerShips
    (Js.string game_id)
    (Js.string player)
    (Js.string ships_json)

let mark_player_ready ~(game_id : string) ~(player : string) : unit =
  firebase##markPlayerReady
    (Js.string game_id)
    (Js.string player)

let get_ships ~(game_id : string) ~(on_result : string -> unit) : unit =
  let cb =
    Js.wrap_callback (fun s ->
        let str = Js.to_string s in
        on_result str)
  in
  firebase##getShips (Js.string game_id) cb

let reset_game ~(game_id : string) ~(player : string) : unit =
  firebase##resetGame (Js.string game_id) (Js.string player)

let setup_forfeit_on_disconnect ~(game_id : string) ~(player : string) : unit =
  firebase##setupForfeitOnDisconnect (Js.string game_id) (Js.string player)

let clear_forfeit_on_disconnect () : unit =
  firebase##clearForfeitOnDisconnect ()

let monitor_opponent_heartbeat ~(game_id : string) ~(opponent_player : string) ~(on_disconnect : string -> unit) : (unit -> unit) =
  let cb =
    Js.wrap_callback (fun s ->
        let str = Js.to_string s in
        on_disconnect str)
  in
  let unsub_js = firebase##monitorOpponentHeartbeat (Js.string game_id) (Js.string opponent_player) cb in
  fun () -> unsub_js ()