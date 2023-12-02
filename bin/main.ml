open Core
open Stdio
open Aoc_2023
open Utils

let usage = "Usage: aoc_2023 --day <day> --part <part 1 or 2>"
let day = ref 0
let part = ref 0
let anon_fun _ = raise (Arg.Bad "no anonymous arguments")

let specs =
  [ "--day", Arg.Set_int day, "Day to run"
  ; "--part", Arg.Set_int part, "Part to run"
  ]
;;

let () =
  let () = Arg.parse specs anon_fun usage in
  let res =
    match !day, !part with
    | 1, 1 -> Day1.part_1 (read_file !day)
    | 1, 2 -> Day1.part_2 (read_file !day)
    | 2, 1 -> Day2.part_1 (read_file !day)
    | 2, 2 -> Day2.part_2 (read_file !day)
    | _, _ -> raise (Failure "Day or part not implemented")
  in
  printf "Day %d\nPart %d\nResult: %d\n" !day !part res
;;
