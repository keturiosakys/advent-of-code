open Core

let part_1 (input : string) =
  let input =
    String.split_on_chars input ~on:[ '\n' ]
    |> List.filter ~f:(fun s -> not @@ String.is_empty s)
  in
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
        seeds, maps
      | _ -> raise (failwith "Couldn't split on :"))
    | _ -> raise (failwith "Invalid input")
  in
  0
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
  end)
;;
