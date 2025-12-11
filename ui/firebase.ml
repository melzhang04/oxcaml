open Js_of_ocaml

class type firebase_api = object
  method signInGuest : unit -> unit Js.meth
  method signOutUser : unit -> unit Js.meth
  method getCurrentUid : unit -> Js.js_string Js.t Js.opt Js.meth

  method requestQuickMatch :
    (Js.js_string Js.t -> unit) Js.callback -> unit Js.meth

  method subscribeGame :
    Js.js_string Js.t ->
    (Js.js_string Js.t -> unit) Js.callback ->
    unit Js.meth

  method sendMove :
    Js.js_string Js.t -> int -> int -> Js.js_string Js.t -> unit Js.meth
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

let subscribe_game (game_id : string) (on_update : string -> unit) : unit =
  let cb =
    Js.wrap_callback (fun s ->
        let str = Js.to_string s in
        on_update str)
  in
  firebase##subscribeGame (Js.string game_id) cb

let send_move ~(game_id : string) ~(row : int) ~(col : int)
    ~(player : string) : unit =
  firebase##sendMove
    (Js.string game_id)
    row
    col
    (Js.string player)