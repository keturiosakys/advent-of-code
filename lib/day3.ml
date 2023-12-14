open Core

type part =
  { value : int
  ; row : int
  ; start_pos : int
  ; end_pos : int
  }

type symbol =
  { row : int
  ; col : int
  ; sym : char
  }

let directions = [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ]

let symbols =
  List.init 32 ~f:(fun num -> Char.of_int_exn (num + 33))
  |> List.filter ~f:(fun c -> not @@ (Char.is_alphanum c || Char.equal '.' c))
;;

let is_number item =
  match item with
  | '0' .. '9' -> true
  | _ -> false
;;

let is_symbol item = List.mem symbols item ~equal:Char.equal

let parse_parts_and_symbols input =
  let parts, symbols =
    List.unzip
    @@ List.mapi input ~f:(fun row line ->
         let line_items = String.to_list line in
         let rec loop idx start_idx curr_part parts symbols line_items =
           let resolve_parts curr_part parts =
             match
               List.rev curr_part |> String.of_char_list |> Int.of_string_opt
             with
             | Some part ->
               { value = part
               ; row
               ; start_pos = start_idx |> Option.value_exn
               ; end_pos = idx - 1
               }
               :: parts
             | None -> parts
           in
           match line_items with
           | [] ->
             let parts = resolve_parts curr_part parts in
             parts, symbols
           | item :: rest when is_number item ->
             loop
               (idx + 1)
               (if Option.is_some start_idx then start_idx else Some idx)
               (item :: curr_part)
               parts
               symbols
               rest
           | item :: rest ->
             let parts = resolve_parts curr_part parts in
             let symbols =
               if is_symbol item
               then { row; col = idx; sym = item } :: symbols
               else symbols
             in
             loop (idx + 1) None [] parts symbols rest
         in
         loop 0 None [] [] [] line_items)
  in
  ( Stdlib.List.flatten parts
  , Stdlib.List.flatten
      symbols (* TODO: revise this with a tail-recursive function *) )
;;

let is_attached (part : part) symbols =
  List.find directions ~f:(fun (row, col) ->
    List.exists symbols ~f:(fun symbol ->
      if Int.equal symbol.row (part.row + row)
      then
        Int.equal symbol.col (part.start_pos + col)
        || Int.equal symbol.col (part.end_pos + col)
      else false))
  |> Option.is_some
;;

let part_1 (input : string list) =
  let parts, symbols = parse_parts_and_symbols input in
  let engine_parts =
    List.filter parts ~f:(fun part -> is_attached part symbols)
  in
  List.sum (module Int) engine_parts ~f:(fun part -> part.value)
;;

let find_adjacent_parts symbol (parts : part list) =
  let rec loop acc = function
    | [] -> acc
    | _ :: _ when phys_equal (List.length acc) 2 -> acc
    | (part : part) :: rest ->
      if List.exists directions ~f:(fun (row, col) ->
           if Int.equal symbol.row (part.row + row)
           then
             Int.equal symbol.col (part.start_pos + col)
             || Int.equal symbol.col (part.end_pos + col)
           else false)
      then loop (part :: acc) rest
      else loop acc rest
  in
  let adj = loop [] parts in
  if List.length adj < 2
  then 0
  else List.fold adj ~init:1 ~f:(fun acc part -> part.value * acc)
;;

let part_2 (input : string list) =
  let parts, symbols = parse_parts_and_symbols input in
  List.fold symbols ~init:0 ~f:(fun acc symbol ->
    match symbol.sym with
    | '*' -> acc + find_adjacent_parts symbol parts
    | _ -> acc)
;;

let%test_module "Day 3" =
  (module struct
    let test_input =
      {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}
    ;;

    let%test_unit "part 1" =
      [%test_eq: int] (part_1 (String.split_lines test_input)) 4361
    ;;

    let%test_unit "part 2" =
      [%test_eq: int] (part_2 (String.split_lines test_input)) 467835
    ;;
  end)
;;
