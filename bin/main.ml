let width = 1280
let height = 720
let player_width = 30
let player_height = 130
let ball_radius = 20.0

type state = { score : int ref array }
type vec2 = { x : int ref; y : int ref }
type player = { position : vec2 }
type ball = { position : vec2; velocity : vec2 }

let game_state : state = { score = [| ref 0; ref 0 |] }

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
      {
        x = ref 100;
        y = ref ((height / 2) - (Float.to_int ball_radius / 2));
      };
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

  let x_collided = ref false in
  if bx - (Float.to_int ball_radius / 2) <= p0x || bx >= p1x then
    x_collided := true;

  let y_collided = ref false in
  if (by >= p0y && by <= p0y + player_height) || (by >= p1y && by <= p1y + player_height) then
    y_collided := true;

  if !x_collided && !y_collided then begin
    b.velocity.x := vx * -1;

    let d0x = bx - p0x in
    let d1x = p1x - bx in
    let y_lerp = ref 0.0 in
    let by_float = Int.to_float by in
    let p0y_float = Int.to_float p0y in
    let p1y_float = Int.to_float p1y in
    if d0x > d1x then begin
      b.position.x := p1x - (Float.to_int ball_radius);
      y_lerp := (by_float -. p1y_float) /. (by_float +. p1y_float)
    end
    else begin
      b.position.x := p0x + (Float.to_int ball_radius);
      y_lerp := (by_float -. p0y_float) /. (by_float +. p0y_float)
    end;

    b.velocity.y := Float.to_int (!y_lerp *. 30.)
  end

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
  Raylib.init_window width height "raylib [core] example - basic window";
  Raylib.set_target_fps 60

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.black;

    draw_player p0;
    draw_player p1;
    draw_score game_state;
    process_users_input p0 p1;

    resolve_player_offscreen_position p0;
    resolve_player_offscreen_position p1;

    draw_ball ball;
    update_ball_position ball;

    let ballr_int = (Float.to_int ball_radius) in
    let bx = !(ball.position.x) in
    if bx + ballr_int / 2 >= width then begin
      game_state.score.(0) := !(game_state.score.(0)) + 1;
      ball.position.x := width - ballr_int;
      ball.velocity.x := -20
    end
    else if bx - ballr_int / 2 <= 0 then begin
      game_state.score.(0) := !(game_state.score.(0)) + 1;
      ball.position.x := width - ballr_int;
      ball.velocity.x := 20
    end;

    resolve_ball_collision ball;
    resolve_ball_offscreen_position ball;

    end_drawing ();
    loop ()

let () = setup () |> loop
