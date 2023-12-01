open! Base

let read_file day =
  let file_name = "inputs/day" ^ Int.to_string day ^ ".txt" in
  In_channel.with_open_text file_name In_channel.input_lines
;;

