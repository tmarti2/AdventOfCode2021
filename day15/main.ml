open Base
open Graph

module Node = struct
  type t = int * int
  let compare = Poly.compare
  let hash = Hashtbl.hash
  let equal = Poly.(=)
  let default = 0
end

module Label = struct
  include Int
  let default = 0
end

module G = Imperative.Digraph.ConcreteLabeled(Node)(Label)

module Weight = struct
  type edge = G.E.t
  type t = int
  let weight = G.E.label
  let compare = compare
  let add = (+)
  let zero = 0
end

module D = Path.Dijkstra(G)(Weight)

let in_range a (x,y) =
  y >= 0 && y < Array.length a && x >= 0 && x < Array.length a.(0)

let find_neighbors a (x,y) =
  let deltas = [(-1, 0); (0, -1); (1, 0); (0, 1)] in
  let coords = List.map ~f:(fun (dx, dy) -> (x + dx, y + dy)) deltas in
  List.filter ~f:(in_range a) coords

let string_to_int_list s =
  String.to_list s |> List.map ~f:(Fn.compose Int.of_string Char.to_string)

let inc_dedup l =
  let rec aux acc last n =
    if n = 5 then acc
    else begin
      let l = List.map last ~f:(fun i -> i % 9 + 1) in
      aux (acc @ l) l (n+1)
    end
  in aux l l 1

let fill_graph g map =
  Array.iteri ~f:(fun y row ->
      Array.iteri ~f:(fun x _ ->
          List.iter ~f:(fun (x',y') ->
              G.add_edge_e g (G.E.create (x,y) map.(y').(x') (x',y'))
            ) (find_neighbors map (x,y))
        ) row
    ) map

let parse_map_full data =
  List.map data ~f:(fun row -> inc_dedup @@ string_to_int_list row)
  |> List.transpose_exn |> List.map ~f:inc_dedup |> List.transpose_exn
  |> Array.of_list_map ~f:(Array.of_list_map ~f:Fn.id)

let parse_map_normal data =
  List.map data ~f:(fun row -> string_to_int_list row)
  |> Array.of_list_map ~f:(Array.of_list_map ~f:Fn.id)

let solve f data =
  let g = G.create () in
  let map = f data in
  fill_graph g map;
  let max = Array.length map.(0) - 1, Array.length map - 1 in
  snd @@ D.shortest_path g (0,0) max

let solve1 = solve parse_map_normal
let solve2 = solve parse_map_full

let main file =
  let data = Stdio.In_channel.read_lines file in
  Stdio.printf "File : %s\n       Part 1 : %d\n       Part 2 : %d\n"
    file (solve1 data) (solve2 data)

let _ = main "example.txt"; main "input.txt"