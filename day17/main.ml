open Base

type coords = {x:int;y:int}
type target = {pmin:coords;pmax:coords}

let parse_file data =
  Caml.Scanf.sscanf (List.hd_exn data) "target area: x=%d..%d, y=%d..%d"
    (fun xmin xmax ymin ymax -> {pmin={x=xmin;y=ymin};pmax={x=xmax;y=ymax}})

let has_reached_target s t =
  s.x >= t.pmin.x && s.x <= t.pmax.x && s.y >= t.pmin.y && s.y <= t.pmax.y

let has_missed_target s t = s.x > t.pmax.x || s.y < t.pmin.y

let next_vel v =
  {x = v.x + (if v.x = 0 then 0 else if v.x < 0 then 1 else -1); y = v.y-1}

let test_velocity t vel =
  let rec aux maxy s v =
    if has_missed_target s t then None
    else if has_reached_target s t then Some (max maxy s.y)
    else aux (max maxy (s.y+v.y)) {x=s.x+v.x;y=s.y+v.y} (next_vel v)
  in aux Int.min_value {x=0;y=0} vel

let solve t =
  let xv = List.range ~start:`inclusive ~stop:`inclusive 0 t.pmax.x in
  let yv = List.range ~start:`inclusive ~stop:`inclusive (- (abs t.pmin.y)) (abs t.pmin.y) in
  let l =
    List.fold xv ~init:[] ~f:(fun acc x ->
      List.fold yv ~init:acc ~f:(fun acc y ->
          match test_velocity t {x;y} with
          | None -> acc
          | Some v -> v :: acc
        )
    )
  in
  l

let solve1 t =
  Option.value_exn (solve t |> List.max_elt ~compare:(fun my my' -> compare my my'))

let solve2 t = solve t |> List.length

let main file =
  let t = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "File : %s\n       Part 1 : %d\n       Part 2 : %d\n"
    file (solve1 t) (solve2 t)

let _ = main "example.txt"; main "input.txt"