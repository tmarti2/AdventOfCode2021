open Base

let max v1 v2 v3 =
  if v1 >= v2 && v1 >= v3 then v1
  else if v2 >= v1 && v2 >= v3 then v2
  else v3

let parse_file data =
  let parsed_data,mx,my =
    List.fold_left ~init:([],0,0) ~f:(fun (parsed,mx,my) line ->
        Caml.Scanf.sscanf line "%d,%d -> %d,%d" (fun x1 y1 x2 y2 ->
            ((x1,y1),(x2,y2))::parsed,(max mx x1 x2),(max my y1 y2)
          )
      ) data
  in
  List.rev parsed_data,mx+1,my+1

let fill_col map x y1 y2 cpt =
  let f,t = if y1 <= y2 then y1,y2 else y2,y1 in
  Array.foldi ~init:cpt ~f:(fun y n v ->
      if y >= f && y <= t
      then (map.(x).(y) <- v + 1; if v + 1 = 2 then n+1 else n)
      else n
    ) map.(x)

let fill_row map y x1 x2 cpt =
  let f,t = if x1 <= x2 then x1,x2 else x2,x1 in
  Array.foldi ~init:cpt ~f:(fun x n c ->
      if x >= f && x <= t
      then (c.(y) <- c.(y) + 1; if c.(y) = 2 then n + 1 else n)
      else n
    ) map

let fill_diag map (x1,y1) (x2,y2) cpt =
  let offset_x = if x1 > x2 then -1 else 1 in
  let offset_y = if y1 > y2 then -1 else 1 in
  let rec next x y n =
    map.(x).(y) <- map.(x).(y) + 1;
    let n = if map.(x).(y) = 2 then n + 1 else n in
    if x = x2 && y = y2
    then n
    else next (x + offset_x) (y + offset_y) n
  in
  next x1 y1 cpt

let reset_map map =
  Array.iter ~f:(fun c ->
      Array.iteri ~f:(fun i _ ->
          c.(i) <- 0
        ) c
    ) map

let solve ~diag map data =
  List.fold ~init:0 ~f:(fun n ((x1,y1),(x2,y2)) ->
      if x1 = x2 then
        fill_col map x1 y1 y2 n
      else if y1 = y2 then
        fill_row map y1 x1 x2 n
      else if diag then
        fill_diag map (x1,y1) (x2,y2) n
      else n
    ) data

let main file =
  let data, dimx, dimy = Stdio.In_channel.read_lines file |> parse_file in
  let map = Array.make_matrix ~dimx ~dimy 0 in
  let res1 = solve ~diag:false map data in
  reset_map map;
  let res2 = solve ~diag:true map data in
  Stdio.printf "Part 1 : %d\npart 2 : %d\n" res1 res2


let _ = main "example.txt"; main "input.txt"