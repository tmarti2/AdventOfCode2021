open Base

let parse_file data =
  Array.of_list_map ~f:(fun s ->
      Array.of_list_map ~f:(fun c ->
          false, Char.to_string c |> Int.of_string
        ) (String.to_list s)
    ) data

let in_range a (x,y) =
  y >= 0 && y < Array.length a && x >= 0 && x < Array.length a.(0)

let get a (x,y) =
  if in_range a (x,y) then Some a.(y).(x) else None

let find_neightboors a (x,y) =
  let deltas = [(-1, 0); (0, -1); (1, 0); (0, 1)] in
  let coords = List.map ~f:(fun (dx, dy) -> (x + dx, y + dy)) deltas in
  List.filter ~f:(in_range a) coords

let find_neightboors_higher a (x,y)=
  let (_,low) = a.(y).(x) in
  let f (x',y') =
    match get a (x',y') with
    | Some (false,v') when v' <> 9 && v' > low -> Some (x',y')
    | _ -> None
  in
  List.filter_map ~f (find_neightboors a (x,y))

let is_low a (x,y) =
  find_neightboors a (x,y)
  |> List.exists ~f:(fun (x',y') -> snd @@ a.(y').(x') <= snd @@ a.(y).(x))
  |> not

let find_lows a =
  Array.foldi a ~init:[] ~f:(fun y cpt row ->
      Array.foldi row ~init:cpt ~f:(fun x cpt _ ->
          if is_low a (x,y) then (x,y)::cpt else cpt
        )
    )

let find_bassin arr (x,y) =
  arr.(y).(x) <- true, snd arr.(y).(x);
  let s = Stack.create () in
  Stack.push s (x,y);
  let rec aux cpt =
    if Stack.is_empty s then
      cpt
    else begin
      find_neightboors_higher arr (Stack.pop_exn s) |>
      List.iter ~f:(fun (x,y) ->
          arr.(y).(x) <- true, snd arr.(y).(x);
          Stack.push s (x,y)
        );
      aux (cpt+1)
    end
  in
  aux 0

let solve1 a =
  find_lows a |> List.sum (module Int) ~f:(fun (x,y) -> 1 + snd a.(y).(x))

let solve2 a =
  let sizes =
    find_lows a |> List.map ~f:(find_bassin a) |> List.sort ~compare |> List.rev
  in
  assert (List.length sizes >= 3);
  List.nth_exn sizes 0 * List.nth_exn sizes 1 * List.nth_exn sizes 2

let main file =
  let data = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "Part 1 : %d\nPart 2 : %d\n" (solve1 data) (solve2 data)

let _ = main "example.txt"; main "input.txt"