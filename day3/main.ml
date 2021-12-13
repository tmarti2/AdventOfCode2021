open Base

let parse_file data =
  List.map ~f:String.to_list data
  |> List.map ~f:(fun l -> List.map l ~f:(Poly.(=) '1'))

let invert l = List.map ~f:(fun v -> not v) l

let list_to_string l =
  List.fold ~init:"" ~f:(fun acc v -> acc ^ (if v then "1" else "0")) l

let list_to_int l = Int.of_string @@ "0b" ^ (list_to_string l)

let most_common_pos data pos =
  let rotate = List.transpose_exn data in
  let acc = List.nth_exn rotate pos |> List.count ~f:Fn.id in
  let tt = List.length data |> Float.of_int in
  Poly.(Float.of_int acc >= (tt /. 2.0))

let most_common data =
  let rotate = List.transpose_exn data in
  let acc = List.map ~f:(fun bitlist -> List.count ~f:Fn.id bitlist) rotate in
  let tt = List.length data |> Float.of_int in
  List.map acc ~f:(fun v -> Poly.(Float.of_int v >= (tt /. 2.0)))

let rec calc f pos data =
  match data with
  | [] -> assert false
  | [x] -> list_to_int x
  | _ ->
    let chosen = f (most_common_pos data) pos in
    List.filter data ~f:(fun l -> Poly.(List.nth_exn l pos = chosen))
    |> calc f (pos+1)

let solve1 data =
  let g = most_common data in
  (list_to_int g) * (invert g |> list_to_int)

let solve2 data =
   calc Fn.id 0 data * calc Fn.non 0 data

let main file =
  let data = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "File : %s\n       Part 1 : %d\n       Part 2 : %d\n"
    file (solve1 data) (solve2 data)

let _ = main "example.txt";  main "input.txt"