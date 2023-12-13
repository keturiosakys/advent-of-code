open Core
open Stdio
open Aoc_2023

let usage = "Usage: aoc_2023 --day <day> --part <part 1 or 2>"
let day = ref 0
let part = ref 0

let anon_args _ =
  raise (Arg.Bad "Invalid args: no anonymous arguments can be present")
;;

let flags =
  [ "--day", Arg.Set_int day, "Day to run"
  ; "--part", Arg.Set_int part, "Part to run"
  ]
;;

let read_file day =
  let file_name = "inputs/day" ^ Int.to_string day ^ ".txt" in
  Stdio.In_channel.with_file file_name ~f:(fun ch ->
    In_channel.input_all ch |> String.split_lines)
;;

let () =
  let () = Arg.parse flags anon_args usage in
  let res =
    match !day, !part with
    | 1, 1 -> Day1.part_1 (read_file !day)
    | 1, 2 -> Day1.part_2 (read_file !day)
    | 2, 1 -> Day2.part_1 (read_file !day)
    | 2, 2 -> Day2.part_2 (read_file !day)
    | 3, 1 -> Day3.part_1 (read_file !day)
    | _, _ -> raise (Failure "Day or part not implemented")
  in
  printf "Day %d\nPart %d\nResult: %d\n" !day !part res
;;
