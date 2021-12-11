open Base

let parse_file data =
  List.map ~f:(fun s ->
      match String.split ~on:'|' s with
      | [s1;s2] ->
        String.strip s1 |> String.split ~on:' ' |>
        List.sort ~compare:(fun s1 s2 -> compare (String.length s1) (String.length s2)),
        String.strip s2 |> String.split ~on:' '
      | _ -> assert false
    ) data

let count_uniq (_,r) =
  List.count ~f:(fun s ->
      let len = String.length s in
      len = 2 || len = 3 || len = 4 || len = 7
    ) r

let solve1 l =
  List.sum (module Int) ~f:count_uniq l

let main file =
  let data = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "Part 1 : %d\n" (solve1 data)

let _ = main "example.txt"
; main "input.txt"