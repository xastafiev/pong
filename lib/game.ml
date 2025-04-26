let width = 1900
let height = 1080
let player_width = 30
let player_height = height / 5
let ball_radius = 20.0

type status = Start | Playing | End | Continue
type state = { score : int ref array; status : status ref }
type vec2 = { x : int ref; y : int ref }
type player = { position : vec2 }
type ball = { position : vec2; velocity : vec2 }

let game_state : state = { score = [| ref 0; ref 0 |]; status = ref Start }

let p0 : player =
  { position = { x = ref 0; y = ref ((height / 2) - (player_height / 2)) } }

let p1 : player =
  {
    position =
      {
        x = ref (width - player_width);
        y = ref ((height / 2) - (player_height / 2));
      };
  }

let ball : ball =
  {
    position =
      { x = ref 100; y = ref ((height / 2) - Float.to_int ball_radius) };
    velocity = { x = ref 20; y = ref 0 };
  }

let score_chars : char array = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7' |]
let win_score : int = 7

let draw_player (p : player) =
  let open Raylib in
  draw_rectangle !(p.position.x) !(p.position.y) player_width player_height
    Color.white

let draw_score (s : state) =
  let open Raylib in
  draw_text
    (Char.escaped score_chars.(!(s.score.(0))))
    (Int.shift_right width 1 - 100)
    0 64 Color.white;
  draw_text
    (Char.escaped score_chars.(!(s.score.(1))))
    (Int.shift_right width 1 + 100)
    0 64 Color.white

let process_users_input (p0 : player) (p1 : player) =
  let open Raylib in
  if is_key_down Key.W then p0.position.y := !(p0.position.y) - 20;
  if is_key_down Key.S then p0.position.y := !(p0.position.y) + 20;

  if is_key_down Key.Up then p1.position.y := !(p1.position.y) - 20;
  if is_key_down Key.Down then p1.position.y := !(p1.position.y) + 20

let resolve_player_offscreen_position (p : player) =
  if !(p.position.y) + player_height > height then
    p.position.y := height - player_height
  else if !(p.position.y) < 0 then p.position.y := 0

let draw_ball (b : ball) =
  let open Raylib in
  draw_circle !(b.position.x) !(b.position.y) ball_radius Color.white

let resolve_ball_collision (b : ball) =
  let vx = !(b.velocity.x) in
  let bx = !(b.position.x) in
  let by = !(b.position.y) in

  let p0x = !(p0.position.x) in
  let p0y = !(p0.position.y) in

  let p1x = !(p1.position.x) in
  let p1y = !(p1.position.y) in

  assert (vx <> 0);

  let p0_collided = bx - Float.to_int ball_radius <= p0x in
  let p1_collided = bx >= p1x in
  let x_collided = p0_collided || p1_collided in

  let y_collided = ref false in
  if p0_collided then y_collided := by >= p0y && by <= p0y + player_height
  else if p1_collided then y_collided := by >= p1y && by <= p1y + player_height;

  if x_collided && !y_collided then (
    b.velocity.x := vx * -1;

    let d0x = bx - p0x in
    let d1x = p1x - bx in
    let y_lerp = ref 0.0 in
    let by_float = Int.to_float by in
    let p0y_float_centered =
      Int.to_float p0y +. (Int.to_float player_height /. 2.)
    in
    let p1y_float_centered =
      Int.to_float p1y +. (Int.to_float player_height /. 2.)
    in
    if d0x > d1x then (
      b.position.x := p1x - Float.to_int ball_radius;
      y_lerp :=
        (by_float -. p1y_float_centered) /. (by_float +. p1y_float_centered))
    else (
      b.position.x := p0x + Float.to_int ball_radius;
      y_lerp :=
        (by_float -. p0y_float_centered) /. (by_float +. p0y_float_centered));

    b.velocity.y := Float.to_int (!y_lerp *. 60.))

let update_ball_position (b : ball) =
  b.position.x := !(b.position.x) + !(b.velocity.x);
  b.position.y := !(b.position.y) + !(b.velocity.y)

let resolve_ball_offscreen_position (b : ball) =
  let r = Float.to_int ball_radius in
  if !(b.position.y) >= height - r then (
    b.position.y := height - r;
    b.velocity.y := -1 * !(b.velocity.y))
  else if !(b.position.y) <= 0 then (
    b.position.y := 0;
    b.velocity.y := -1 * !(b.velocity.y))

let setup () =
  Raylib.init_window width height "Game of the year 2025";
  Raylib.set_target_fps 60

let draw_text_center_pivot (text : string) (x : int) (y : int) (size : int) =
  let open Raylib in
  let s = measure_text text size in
  draw_text text (x - (s / 2)) y size Color.white

let process_playing_status () =
  let open Raylib in
  clear_background Color.black;

  draw_player p0;
  draw_player p1;
  draw_score game_state;
  process_users_input p0 p1;

  resolve_player_offscreen_position p0;
  resolve_player_offscreen_position p1;

  draw_ball ball;
  update_ball_position ball;
  resolve_ball_collision ball;

  let ballr_int = Float.to_int ball_radius in
  let bx = !(ball.position.x) in
  if bx + ballr_int >= width then (
    game_state.status := Continue;
    game_state.score.(0) := !(game_state.score.(0)) + 1;
    ball.position.x := width - (ballr_int * 2);
    ball.velocity.x := -20)
  else if bx + ballr_int <= 0 then (
    game_state.status := Continue;
    game_state.score.(1) := !(game_state.score.(1)) + 1;
    ball.position.x := 0 + (ballr_int * 2);
    ball.velocity.x := 20);

  if
    !(game_state.score.(0)) >= win_score || !(game_state.score.(1)) >= win_score
  then game_state.status := End;

  resolve_ball_offscreen_position ball

let process_start_status () =
  draw_text_center_pivot "PRESS ENTER TO START" (width / 2) (height / 2) 70;
  if Raylib.is_key_down Raylib.Key.Enter then game_state.status := Playing

let process_end_status () =
  draw_text_center_pivot "GAME ENDED" (width / 2) 200 70;
  draw_text_center_pivot "PRESS ENTER TO RESTART" (width / 2) (height / 2) 70;

  game_state.score.(0) := 0;
  game_state.score.(1) := 0;

  if Raylib.is_key_down Raylib.Key.Enter then game_state.status := Playing

let process_continue_status () =
  draw_text_center_pivot "PRESS ENTER TO CONTINUE" (width / 2) (height / 2) 70;
  if Raylib.is_key_down Raylib.Key.Enter then game_state.status := Playing

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();

    (match !(game_state.status) with
    | Start -> process_start_status ()
    | Playing -> process_playing_status ()
    | Continue -> process_continue_status ()
    | End -> process_end_status ());

    end_drawing ();
    loop ()
