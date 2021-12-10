open Base

let parse_file data =
  List.map ~f:(fun s ->
      String.to_list s |> List.to_array |>
      Array.map ~f:(fun c ->
          let v = Char.to_string c |> Int.of_string in
          (false,v))
    ) data |> List.to_array

let get arr (x,y) : (bool*int) option =
  if y < 0 || y >= Array.length arr then None
  else begin
    if x < 0 || x >= Array.length arr.(y) then None
    else Some arr.(y).(x)
  end

let get_dir dir x y =
  match dir with
  | `O -> (x-1),y
  | `N -> x,(y-1)
  | `E -> (x+1),y
  | `S -> x,(y+1)

let find_neightboors arr x y : (bool*int) option list =
  [get arr (get_dir `O x y);
   get arr (get_dir `N x y);
   get arr (get_dir `E x y);
   get arr (get_dir `S x y)]

let find_neightboors_low arr x y =
  let (_,low) = arr.(y).(x) in
  let f (x,y) =
    match get arr (x,y) with
    | Some (false,v') when v' <> 9 && v' > low -> Some (x,y)
    | _ -> None
  in
  List.filter_map ~f [get_dir `O x y; get_dir `N x y; get_dir `E x y; get_dir `S x y]

let is_low arr x y =
  find_neightboors arr x y
  |> List.exists ~f:(function None -> false | Some v -> snd v <= snd @@ arr.(y).(x))
  |> not

let find_lows arr =
  Array.foldi arr ~init:[] ~f:(fun y cpt row ->
      Array.foldi row ~init:cpt ~f:(fun x cpt _ ->
          if is_low arr x y then (x,y)::cpt else cpt
        )
    )

let count_low arr =
  find_lows arr |> List.sum (module Int) ~f:(fun (x,y) -> snd arr.(y).(x))

let find_bassin arr (x,y) =
  arr.(y).(x) <- true, snd arr.(y).(x);
  let s = Stack.create () in
  Stack.push s (x,y);
  let rec aux cpt =
    if Stack.is_empty s then cpt
    else begin
      let (x,y) = Stack.pop_exn s in
      find_neightboors_low arr x y
      |> List.iter ~f:(fun (x,y) ->
          arr.(y).(x) <- true, snd arr.(y).(x);
          Stack.push s (x,y)
        );
      aux (cpt+1)
    end
  in
  aux 0

let solve1 data = count_low data

let solve2 data =
  let l = find_lows data in
  let sizes = List.map l ~f:(find_bassin data) |> List.sort ~compare |> List.rev in
  assert (List.length sizes >= 3);
  List.nth_exn sizes 0 *
  List.nth_exn sizes 1 *
  List.nth_exn sizes 2

  let main file =
    let data = Stdio.In_channel.read_lines file |> parse_file in
    Stdio.printf "Part 1 : %d\nPart 2 : %d\n" (solve1 data) (solve2 data)

let _ = main "example.txt"; main "input.txt"