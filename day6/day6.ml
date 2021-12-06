open Base

let parse_file data =
  match data with
  | [] -> failwith "Empty file"
  | [l] ->
    let arr = Array.create ~len:9 0 in
    String.split ~on:',' l |> List.map ~f:Int.of_string
    |> List.iter ~f:(fun v -> arr.(v) <- arr.(v) + 1);
    Array.to_list arr
  | _ -> assert false

let solve days start =
  let rec step days_left current =
    match current with
    | _ when days_left = 0 -> current
    | j0::j1::j2::j3::j4::j5::j6::j7::j8::[] ->
      step (days_left-1) (j1::j2::j3::j4::j5::j6::j0+j7::j8::j0::[])
    | _ -> assert false
  in
  step days start
  |> List.fold_left ~init:0 ~f:(fun acc i -> acc+i)

let main file =
  let start = Stdio.In_channel.read_lines file |> parse_file in
  let res1 = solve 80 start in
  let res2 = solve 256 start in
  Stdio.printf "Part 1 : %d\nPart 2 : %d\n" res1 res2

let _ = main "example.txt"; main "input.txt"