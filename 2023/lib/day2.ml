open! Core
open! Stdio

let parse_value_color cubes =
  match String.split cubes ~on:' ' with
  | _ :: value :: color :: _ -> Int.of_string value, color
  | _ -> raise (Failure "invalid color value pairing")
;;

let find_invalid_cube_pair (cubes : string) =
  let value, color = parse_value_color cubes in
  match value, color with
  | value, "red" when value > 12 -> true
  | value, "green" when value > 13 -> true
  | value, "blue" when value > 14 -> true
  | _, _ -> false
;;

let find_invalid_set (set : string) =
  let invalid_pair =
    String.split set ~on:',' |> List.find ~f:find_invalid_cube_pair
  in
  match invalid_pair with
  | Some _ -> true
  | None -> false
;;

let get_game_and_id line =
  match String.split ~on:':' line with
  | game_id :: game :: _ -> game_id, game
  | _ -> raise (Failure "Invalid line")
;;

let part_1 (input : string list) =
  List.fold input ~init:0 ~f:(fun acc line ->
    let game_id, game = get_game_and_id line in
    let game_id =
      String.split ~on:' ' game_id
      |> List.find_map_exn ~f:(fun a -> Int.of_string_opt a)
    in
    let invalid_set =
      String.split game ~on:';' |> List.find ~f:find_invalid_set
    in
    match invalid_set with
    | Some _ -> acc
    | None -> acc + game_id)
;;

let find_max_pairs acc pair =
  let value, color = parse_value_color pair in
  let current = Hashtbl.find acc color in
  match current with
  | None ->
    Hashtbl.set acc ~key:color ~data:value;
    acc
  | Some curr_val when curr_val < value ->
    Hashtbl.set acc ~key:color ~data:value;
    acc
  | Some _ -> acc
;;

let calculate_set_power (game : string) =
  let pairs =
    String.split_on_chars game ~on:[ ';'; ',' ]
    |> List.fold ~init:(Hashtbl.create (module String)) ~f:find_max_pairs
  in
  Hashtbl.fold pairs ~init:1 ~f:(fun ~key:_ ~data:value acc -> value * acc)
;;

let part_2 (input : string list) =
  List.fold input ~init:0 ~f:(fun acc line ->
    let _, game = get_game_and_id line in
    let power = calculate_set_power game in
    acc + power)
;;

let%test_module "Day 2" =
  (module struct
    let test_input =
      {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}
    ;;

    let%test_unit "part 1" =
      [%test_eq: int] (part_1 (String.split_lines test_input)) 8
    ;;

    let%test_unit "part 2" =
      [%test_eq: int] (part_2 (String.split_lines test_input)) 2286
    ;;
  end)
;;
