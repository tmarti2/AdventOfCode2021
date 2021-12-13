open Base

let parse_file data =
  Array.of_list_map ~f:(fun s ->
      Array.of_list_map ~f:(fun c ->
          Char.to_string c |> Int.of_string
        ) (String.to_list s)
    ) data

let in_range a (x,y) =
  y >= 0 && y < Array.length a && x >= 0 && x < Array.length a.(0)

let get a (x,y) =
  if in_range a (x,y) then Some a.(y).(x) else None

let find_neighbors a (x,y) =
  let deltas = [(-1, 0); (0, -1); (1, 0); (0, 1);(-1, -1); (1, -1); (1, 1); (-1, 1)] in
  let coords = List.map ~f:(fun (dx, dy) -> (x + dx, y + dy)) deltas in
  List.filter ~f:(in_range a) coords

let inc_neighbors a (x,y) =
  let f (x',y') =
    match get a (x',y') with
    | Some v when v > 0 && v < 9 -> a.(y').(x') <- v + 1; None
    | Some v when v = 9          -> a.(y').(x') <- 0; Some (x',y')
    | _ -> None
  in List.filter_map ~f (find_neighbors a (x,y))

let flash a init =
  let rec aux cpt = function
    | [] -> cpt
    | next :: rest -> aux (cpt +1) (rest @ inc_neighbors a next)
  in aux 0 init

let step a =
  Array.foldi ~init:[] ~f:(fun y acc row ->
      Array.foldi ~init:acc ~f:(fun x acc v ->
          if v >= 0 && v < 9 then (a.(y).(x) <- v + 1; acc)
          else (a.(y).(x) <- 0; (x,y)::acc)
        ) row
    ) a
  |> flash a

let n_step n a = List.init n ~f:Fn.id |> List.map ~f:(fun _ -> step a)

let step_unit_sync a =
  let sync = Array.length a * Array.length a.(0) in
  let rec aux nb_step =
    if step a = sync then nb_step else aux (nb_step+1)
  in aux 1

let copy a =  Array.map ~f:(fun row -> Array.copy row) a

let solve1 a = copy a |> n_step 100 |> List.reduce_exn ~f:(+)
let solve2 a = copy a |> step_unit_sync

let main file =
  let data = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "Part 1 : %d\nPart 2 : %d\n" (solve1 data) (solve2 data)

let _ = main "example.txt"; main "input.txt"