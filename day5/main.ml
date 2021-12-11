open Base

let max v1 v2 v3 =
  if v1 >= v2 && v1 >= v3 then v1
  else if v2 >= v1 && v2 >= v3 then v2
  else v3

let is_diag dx dy = dx <> 0 && dy <> 0

let parse_file data =
  let sign_delta i = match Int.sign i with Neg -> -1 | Zero -> 0 | Pos -> 1 in
  let lines,diags,mx,my =
    List.fold_left ~init:([],[],0,0) ~f:(fun (lines,diags,mx,my) line ->
        Caml.Scanf.sscanf line "%d,%d -> %d,%d" (fun x1 y1 x2 y2 ->
            let dx,dy = sign_delta (x2 - x1), sign_delta (y2 - y1) in
            let lines,diags =
              if is_diag dx dy
              then lines,((x1,y1),(x2,y2),(dx,dy))::diags
              else ((x1,y1),(x2,y2),(dx,dy))::lines,diags
            in lines, diags, (max mx x1 x2), (max my y1 y2)
          )
      ) data
  in
  lines,diags,mx+1,my+1

let fill_line map ((x1,y1),(x2,y2),(dx,dy)) cpt =
  let rec next x y n =
    map.(x).(y) <- map.(x).(y) + 1;
    let n = if map.(x).(y) = 2 then n + 1 else n in
    if x = x2 && y = y2
    then n
    else next (x + dx) (y + dy) n
  in
  next x1 y1 cpt

let solve map data cpt =
  List.fold ~init:cpt ~f:(fun n line ->
      fill_line map line n
    ) data

let main file =
  let lines,diags, dimx, dimy = Stdio.In_channel.read_lines file |> parse_file in
  let map = Array.make_matrix ~dimx ~dimy 0 in
  let res1 = solve map lines 0 in
  let res2 = solve map diags res1 in
  Stdio.printf "Part 1 : %d\npart 2 : %d\n" res1 res2

let _ = main "example.txt"; main "input.txt"