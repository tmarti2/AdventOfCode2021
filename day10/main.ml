open Base

let score1 = function
  | `RParen -> 3 | `RBracket -> 57 | `RBrace -> 1197 | `RChevron -> 25137

let score2 = function
  | `RParen -> 1 | `RBracket -> 2 | `RBrace -> 3 | `RChevron -> 4

let find_closing = function
  | `LParen -> `RParen  | `LBracket -> `RBracket
  | `LBrace -> `RBrace  | `LChevron -> `RChevron

let rec partition_line ?(stack=[]) = function
  | [] -> Either.First stack
  | ((`LParen | `LBracket | `LBrace | `LChevron) as c) :: rest ->
    partition_line ~stack:(find_closing c::stack) rest
  | ((`RParen | `RBracket | `RBrace | `RChevron) as c) :: _
    when List.is_empty stack || Poly.((List.hd_exn stack) <> c) ->
    Either.second c
  | _ :: rest -> partition_line ~stack:(List.tl_exn stack) rest

let solve1 l = List.sum (module Int) ~f:score1 l

let solve2 l =
  let scores =
    let count l = List.fold ~init:0 ~f:(fun acc c -> acc * 5 + score2 c) l in
    List.map ~f:count l |> List.sort ~compare
  in List.nth_exn scores (List.length scores / 2)

let parse_file data =
  let conv = function
    | '(' -> `LParen   | ')' -> `RParen
    | '[' -> `LBracket | ']' -> `RBracket
    | '{' -> `LBrace   | '}' -> `RBrace
    | '<' -> `LChevron | '>' -> `RChevron
    | _ -> failwith "Wrong input"
  in
  List.map data ~f:(fun s -> String.to_list s |> List.map ~f:conv)

let main file =
  let data = Stdio.In_channel.read_lines file |> parse_file in
  let legal, illegal = List.partition_map data ~f:partition_line in
  Stdio.printf "Part 1 : %d\nPart 2 : %d\n" (solve1 illegal) (solve2 legal)

let _ = main "example.txt"; main "input.txt"