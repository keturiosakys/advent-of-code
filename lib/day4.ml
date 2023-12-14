open Core

let parse_numbers_exn (line : string) : int Hash_set.t * int Hash_set.t =
  match String.split line ~on:':' with
  | _ :: numbers :: _ ->
    (match String.split numbers ~on:'|' with
     | winning :: drawn :: _ ->
       let winning =
         String.split winning ~on:' '
         |> List.filter_map ~f:(fun s -> Int.of_string_opt s)
         |> Hash_set.of_list (module Int)
       in
       let drawn =
         String.split drawn ~on:' '
         |> List.filter_map ~f:(fun s -> Int.of_string_opt s)
         |> Hash_set.of_list (module Int)
       in
       winning, drawn
     | _ -> raise (failwith "Failed to parse on |"))
  | _ -> raise (failwith "Failed to parse on :")
;;

let calculate_wins (winning, drawn) =
  Hash_set.inter winning drawn |> Hash_set.length
;;

let part_1 (input : string list) =
  List.fold input ~init:0 ~f:(fun acc line ->
    let wins = parse_numbers_exn line |> calculate_wins in
    match wins with
    | 0 -> acc
    | 1 -> acc + 1
    | n -> acc + (int_of_float @@ (float_of_int 2 ** float_of_int (n - 1))))
;;

let calculate_copies (cards : (int * int) list) : int =
  let rec calc' acc = function
    | [] -> acc
    | (wins, units) :: rest ->
      let to_copy, rest = List.split_n rest wins in
      let updated =
        List.map to_copy ~f:(fun (w, copies) -> w, copies + units)
      in
      calc' (units :: acc) (updated @ rest)
  in
  calc' [] cards |> List.reduce ~f:( + ) |> Option.value_exn
;;

let part_2 (input : string list) =
  List.map input ~f:(fun line ->
    let wins = parse_numbers_exn line |> calculate_wins in
    wins, 1)
  |> calculate_copies
;;

let%test_module "Day 4" =
  (module struct
    let test_input =
      {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}
    ;;

    let%test_unit "Part 1" =
      [%test_eq: int] (part_1 (String.split_lines test_input)) 13
    ;;

    let%test_unit "Part 2" =
      [%test_eq: int] (part_2 (String.split_lines test_input)) 30
    ;;
  end)
;;
