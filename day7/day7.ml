open Base

let parse_file data =
  List.hd_exn data |> String.split ~on:',' |> List.map ~f:Int.of_string

let count f data v =
  List.sum (module Int) ~f:(fun v' -> f @@ abs (v-v')) data

let solve f data =
  let min = Option.value_exn (List.min_elt ~compare data) in
  let max = Option.value_exn (List.max_elt ~compare data) in
  List.range ~start:`inclusive ~stop:`inclusive (min+1) max
  |> List.fold ~init:(count f data min) ~f:(fun acc v ->
      count f data v |> Int.min acc
    )

let main file =
  let data = Stdio.In_channel.read_lines file |> parse_file in
  let res1 = solve (fun n -> n) data in
  let res2 = solve (fun n -> n * (n + 1) / 2) data in
  Stdio.printf "Part 1 : %d\nPart 2 : %d\n" res1 res2

let _ = main "example.txt"; main "input.txt"