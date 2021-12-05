open Base

let max v1 v2 v3 =
  if v1 >= v2 && v1 >= v3 then v1
  else if v2 >= v1 && v2 >= v3 then v2
  else v3

let parse_file data =
  let sign_delta i = match Int.sign i with Neg -> -1 | Zero -> 0 | Pos -> 1 in
  let parsed_data,mx,my =
    List.fold_left ~init:([],0,0) ~f:(fun (parsed,mx,my) line ->
        Caml.Scanf.sscanf line "%d,%d -> %d,%d" (fun x1 y1 x2 y2 ->
            let delta = sign_delta (x2 - x1), sign_delta (y2 - y1) in
            ((x1,y1),(x2,y2),delta)::parsed,
            (max mx x1 x2), (max my y1 y2)
          )
      ) data
  in
  List.rev parsed_data,mx+1,my+1

let is_diag dx dy = dx <> 0 && dy <> 0

let fill_line ~diag map ((x1,y1),(x2,y2),(dx,dy)) cpt =
  if (diag && is_diag dx dy) || (not diag && not (is_diag dx dy)) then
    let rec next x y n =
      map.(x).(y) <- map.(x).(y) + 1;
      let n = if map.(x).(y) = 2 then n + 1 else n in
      if x = x2 && y = y2
      then n
      else next (x + dx) (y + dy) n
    in
    next x1 y1 cpt
  else cpt

let solve ~diag ?(cpt=0) map data =
  List.fold ~init:cpt ~f:(fun n line ->
      fill_line ~diag map line n
    ) data

let main file =
  let data, dimx, dimy = Stdio.In_channel.read_lines file |> parse_file in
  let map = Array.make_matrix ~dimx ~dimy 0 in
  let res1 = solve ~diag:false map data in
  let res2 = solve ~diag:true ~cpt:res1 map data in
  Stdio.printf "Part 1 : %d\npart 2 : %d\n" res1 res2

let _ = main "example.txt"; main "input.txt"