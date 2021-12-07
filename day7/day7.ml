open Base

let parse_file data =
  List.hd_exn data |> String.split ~on:',' |> List.map ~f:Int.of_string

let median data =
  let arr = List.sort ~compare data |> List.to_array in
  (arr.((Array.length arr - 1) / 2) + arr.(Array.length arr / 2)) / 2

let average data =
  Float.of_int @@ List.length data
  |> (/.) (List.map ~f:Float.of_int data |> List.sum (module Float) ~f:Fn.id)
  |> (fun mean -> Float.round_down mean, Float.round_up mean)
  |> (fun (down,up) -> Float.to_int down, Float.to_int up)

let solve v f data =
  List.sum (module Int) ~f:(fun v' -> f @@ abs (v - v')) data

let solve1 data = solve (median data) Fn.id data

let solve2 data =
  let down,up = average data in
  let f = (fun n -> n * (n + 1) / 2) in
  min (solve down f data) (solve up f data)

  let main file =
    let data = Stdio.In_channel.read_lines file |> parse_file in
    Stdio.printf "Part 1 : %d\nPart 2 : %d\n"
    (solve1 data) (solve2 data)

let _ = main "example.txt"; main "input.txt"