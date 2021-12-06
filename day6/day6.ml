open Base

let parse_file data =
  let l = List.hd_exn data |> String.split ~on:',' |> List.map ~f:Int.of_string in
  List.init 9 ~f:(fun i -> List.count l ~f:((=) i))

let solve days start =
  let rec step n l =
    match n,l with
    | 0, _-> l
    | _, [j0;j1;j2;j3;j4;j5;j6;j7;j8] ->
      step (n-1) [j1;j2;j3;j4;j5;j6;j0+j7;j8;j0]
    | _ -> assert false
  in
  step days start

let main file =
  let count = List.reduce_exn ~f:(+) in
  let start = Stdio.In_channel.read_lines file |> parse_file in
  let res1 = solve 80 start in
  let res2 = solve 176 res1 in
  Stdio.printf "Part 1 : %d\nPart 2 : %d\n" (count res1) (count res2)

let _ = main "example.txt"; main "input.txt"