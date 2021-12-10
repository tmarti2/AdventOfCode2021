open Base

let score1 = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> assert false

let score2 = function
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | _ -> assert false

let find_closing = function
  | '(' -> ')'
  | '[' -> ']'
  | '{' -> '}'
  | '<' -> '>'
  | _ -> assert false

let partition_line line =
  let s = Stack.create () in
  let rec aux = function
    | [] -> Either.First s
    | (('(' | '[' | '{' | '<') as c) :: rest ->
      Stack.push s (find_closing c); aux rest
    | ((')' | ']' | '}' | '>') as c) :: rest ->
      if Stack.is_empty s || not @@ Char.equal (Stack.pop_exn s) c then
        Either.second c
      else
        aux rest
    | _ -> assert false
  in
  aux (String.to_list line)

let solve1 l = List.sum (module Int) ~f:score1 l

let solve2 l =
  let res =
    List.map ~f:(fun s ->
        Stack.fold ~init:0 ~f:(fun acc c -> acc * 5 + score2 c) s
      ) l |> List.sort ~compare
  in List.nth_exn res (List.length res / 2)

let main file =
  let data = Stdio.In_channel.read_lines file in
  let legal, illegal = List.partition_map data ~f:partition_line in
  Stdio.printf "Part 1 : %d\nPart 2 : %d\n" (solve1 illegal) (solve2 legal)

let _ = main "example.txt"; main "input.txt"