open Base

let parse_file data =
  let coords, folds = List.split_while ~f:(fun line -> not @@ String.is_empty line) data in
  List.map ~f:(fun line ->
      match String.split ~on:',' line with
      | [x;y] -> Int.of_string x, Int.of_string y
      | _ -> assert false
    ) coords
, List.tl_exn folds |> List.map ~f:(fun line ->
      match String.split ~on:' ' line |> List.last_exn |> String.split ~on:'=' with
      | ["x";v] -> `H, Int.of_string v
      | ["y";v] -> `V, Int.of_string v
      | _ -> assert false
    )

let print_code coords =
  let xl, yl  =  List.unzip coords in
  let max_x, max_y = List.reduce_exn ~f:max xl, List.reduce_exn ~f:max yl in
  for y = 0 to max_y do
    for x = 0 to max_x do
      if List.exists coords ~f:(Poly.equal (x, y)) then
        Stdio.printf "#" else Stdio.printf " "
    done;
    Stdio.printf "\n"
  done

let fold coords (dir, v) =
  match dir with
  | `V -> List.map coords ~f:(fun (x,y) -> (x, v - abs (y - v)))
  | `H -> List.map coords ~f:(fun (x,y) -> (v - abs (x - v), y))

let solve coords folds =
  let rec aux coords todo =
    match todo with
    | [] -> List.dedup_and_sort ~compare:Poly.compare coords
    | next::rest -> aux (fold coords next) rest
  in aux coords folds

let solve1 coords folds = solve coords [(List.hd_exn folds)] |> List.length
let solve2 coords folds = solve coords folds |> print_code

let main file =
  let coords, folds = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "File : %s\n       Part 1 : %d\n       Part 2 : \n"
    file (solve1 coords folds);
  (solve2 coords folds)


  let _ = main "example.txt";  main "input.txt"