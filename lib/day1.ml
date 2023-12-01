open Core
open! Stdio

(* Part 1 *)

let part_1 (input : string list) =
  List.fold input ~init:0 ~f:(fun acc line ->
    let filtered =
      List.filter_map (String.to_list line) ~f:(fun c ->
        Int.of_string_opt (String.of_char c))
    in
    let first = List.hd_exn filtered in
    let last = List.last_exn filtered in
    let number = (first * 10) + last in
    acc + number)
;;

(* Part 2 *)

let num_strings =
  [ "one", 1
  ; "two", 2
  ; "three", 3
  ; "four", 4
  ; "five", 5
  ; "six", 6
  ; "seven", 7
  ; "eight", 8
  ; "nine", 9
  ]
;;

let find_nums line =
  List.filter_mapi (String.to_list line) ~f:(fun pos item ->
    match Int.of_string_opt (String.of_char item) with
    | Some num -> Some (pos, num)
    | None -> None)
;;

let find_spoken_numbers line =
  List.filter_map num_strings ~f:(fun (str, value) ->
    if String.is_substring line ~substring:str
    then (
      let positions = String.substr_index_all ~may_overlap:true line ~pattern:str in
      Some (List.map positions ~f:(fun pos -> pos, value)))
    else None)
  |> Stdlib.List.flatten
;;

let part_2 (input : string list) =
  List.fold input ~init:0 ~f:(fun acc line ->
    let nums = find_spoken_numbers line @ find_nums line in
    let sorted =
      List.sort nums ~compare:(fun (pos1, _) (pos2, _) -> Int.compare pos1 pos2)
    in
    let _, first = List.hd_exn sorted in
    let _, last = List.last_exn sorted in
    let number = (first * 10) + last in
    acc + number)
;;

(* Tests *)

let%test_module "Test Day1" =
  (module struct
    let part_1_test_input = {|1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|}

    let%test_unit "part1" =
      [%test_eq: int] (part_1 (String.split_lines part_1_test_input)) 142
    ;;

    let part_2_test_input =
      {|two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen|}
    ;;

    let%test_unit "part2" =
      [%test_eq: int] (part_2 (String.split_lines part_2_test_input)) 281
    ;;
  end)
;;
