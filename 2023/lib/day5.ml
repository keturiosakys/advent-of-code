open Core

let split_when_empty_string lst =
  let rec loop acc curr = function
    | [] -> List.rev @@ (List.rev curr :: acc)
    | "" :: rest -> loop (List.rev curr :: acc) [] rest
    | hd :: rest -> loop acc (hd :: curr) rest
  in
  match loop [] [] lst with
  | [] :: rest -> rest
  | rest -> rest
;;

let create_maps input =
  let input : string list list = split_when_empty_string input in
  List.filter_map input ~f:(fun s ->
    if phys_equal (List.length s) 0
    then None
    else (
      let parsed =
        List.filter s ~f:(fun str -> not @@ String.is_empty str)
        |> function
        | _ :: tl ->
          List.map tl ~f:(fun l ->
            match
              String.split l ~on:' '
              |> List.filter_map ~f:(fun s -> Int.of_string_opt s)
            with
            | destination :: source :: range :: _ -> destination, source, range
            | _ -> raise (failwith "Invalid source destination range format"))
        | _ -> raise (failwith "Couldn't parse maps")
      in
      Some parsed))
;;

let map_destinations seeds maps =
  List.map seeds ~f:(fun seed ->
    List.fold maps ~init:seed ~f:(fun seed map ->
      let found_dest =
        List.find map ~f:(fun (_, src, rng) ->
          if seed >= src && seed < src + rng then true else false)
      in
      match found_dest with
      | Some (dest, src, _) ->
        let out = seed - src + dest in
        out
      | None -> seed))
;;

let part_1 input =
  let input = String.split_on_chars input ~on:[ '\n' ] in
  let seeds, maps =
    match input with
    | seeds :: maps ->
      String.split seeds ~on:':'
      |> (function
      | _ :: seeds :: _ ->
        let seeds =
          String.split seeds ~on:' '
          |> List.filter_map ~f:(fun s -> Int.of_string_opt s)
        in
        seeds, create_maps maps
      | _ -> raise (failwith "Couldn't split on :"))
    | _ -> raise (failwith "Invalid input")
  in
  let destinations = map_destinations seeds maps in
  List.min_elt destinations ~compare:Int.compare |> Option.value_exn
;;

let generate_nums start length =
  let rec loop acc curr = function
    | 0 -> curr :: acc
    | rem -> loop (curr :: acc) (curr + 1) (rem - 1)
  in
  loop [] start length |> List.rev
;;

let create_seeds seeds =
  let rec loop acc = function
    | [] -> acc
    | start :: length :: rest -> loop ((generate_nums start length) :: acc) rest
    | _ :: [] ->
      raise (failwith "Loose item that should have been in a pair, check input")
  in
  loop [] seeds |> Stdlib.List.flatten
;;

let part_2 input =
  let input = String.split_on_chars input ~on:[ '\n' ] in
  let seeds, maps =
    match input with
    | seeds :: maps ->
      String.split seeds ~on:':'
      |> (function
      | _ :: seeds :: _ ->
        let seeds =
          String.split seeds ~on:' '
          |> List.filter_map ~f:(fun s -> Int.of_string_opt s)
        in
        create_seeds seeds, create_maps maps
      | _ -> raise (failwith "Couldn't split on :"))
    | _ -> raise (failwith "Invalid input")
  in
  let destinations = map_destinations seeds maps in
  List.min_elt destinations ~compare:Int.compare |> Option.value_exn
;;

let%test_module "Day 5" =
  (module struct
    let test_input =
      {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|}
    ;;

    let%test_unit "Part 1" = [%test_eq: int] (part_1 test_input) 35
    let%test_unit "Part 2" = [%test_eq: int] (part_2 test_input) 46
  end)
;;
