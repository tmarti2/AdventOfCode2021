open Base
open Graph

let string_to_int_list s =
  String.to_list s |> List.map ~f:(Fn.compose Int.of_string Char.to_string)

let parse_map data =
  List.map data ~f:string_to_int_list
  |> Array.of_list_map ~f:(Array.of_list_map ~f:Fn.id)

let in_range (w,h) (x,y) =
  y >= 0 && y < h && x >= 0 && x < w

let find_neighbors g (x,y) =
  let deltas = [(-1, 0); (0, -1); (1, 0); (0, 1)] in
  let coords = List.map ~f:(fun (dx, dy) -> (x + dx, y + dy)) deltas in
  List.filter ~f:(in_range g) coords

let risk map (x,y) =
  let mx,my = Array.length map.(0), Array.length map in
  let r = map.(y % my).(x % mx) + (y / my + x / mx) in
  if r <= 9 then r else r-9

let solve ~big data =
  let map = parse_map data in
  let mapx, mapy = Array.length map.(0), Array.length map in
  let mx,my = if big then mapx*5, mapy*5 else mapx, mapy in
  let module G = struct
    type t = int*int
    module V = struct
      type t = int * int
      let compare = Poly.compare
      let hash = Hashtbl.hash
      let equal = Poly.(=)
    end
    module E = struct
      type t = V.t * V.t
      type label = unit
      let label _ = ()
      let src (src,_) = src
      let dst (_,dst) = dst
      let create src _ dst = src,dst
    end
    let iter_vertex _ _ = ()
    let fold_vertex _ _ a = a
    let iter_succ _ _ _ = ()
    let iter_succ_e f g src = List.iter ~f:(fun dst -> f (src,dst)) (find_neighbors g src)
    let fold_edges_e _ _ a = a
    let nb_vertex _ = 0
  end
  in
  let module W = struct
    type t = int
    type edge = G.E.t
    let weight (_,dst) = risk map dst
    let compare = (-)
    let add = (+)
    let zero = 0
  end
  in
  let module D = Path.Dijkstra(G)(W) in
  snd @@ D.shortest_path (mx,my) (0,0) (mx-1,my-1)

let solve1 = solve ~big:false
let solve2 = solve ~big:true

let main file =
  let data = Stdio.In_channel.read_lines file in
  Stdio.printf "File : %s\n       Part 1 : %d\n       Part 2 : %d\n"
    file (solve1 data) (solve2 data)

let _ = main "example.txt"; main "input.txt"