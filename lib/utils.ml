open Core

let read_file day =
  let file_name = "inputs/day" ^ Int.to_string day ^ ".txt" in
  Stdio.In_channel.with_file file_name ~f:(fun ch ->
    In_channel.input_all ch |> String.split_lines)
;;
