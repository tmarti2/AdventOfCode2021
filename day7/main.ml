open Base

let parse_file data =
  List.hd_exn data |> String.split ~on:',' |> List.map ~f:Int.of_string

let solve f data v =
  List.sum (module Int) ~f:(fun v' -> f @@ abs (v - v')) data

let solve1 data =
  let arr = List.sort ~compare data |> List.to_array in
  (arr.((Array.length arr - 1) / 2) + arr.(Array.length arr / 2)) / 2
  |> solve Fn.id data

let solve2 data =
  let f = (fun n -> n * (n + 1) / 2) in
  let mean = List.sum (module Int) ~f:Fn.id data / (List.length data) in
  min (solve f data mean) (solve f data (mean+1))

let main file =
  let data = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "Part 1 : %d\nPart 2 : %d\n"
    (solve1 data) (solve2 data)

let _ = main "example.txt"; main "input.txt"