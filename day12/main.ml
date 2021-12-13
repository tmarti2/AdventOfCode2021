open Base
open Graph

module Node = struct
  type t = {size:[`Big | `Small]; name : string}
  let compare n1 n2 = String.compare n1.name n2.name
  let hash = Hashtbl.hash
  let equal n1 n2 = String.equal n1.name n2.name
  let default = ""
end

module G = Imperative.Graph.Concrete(Node)

let count_paths ~cheat g gstart gend =
  let rec aux token visited n =
    List.sum (module Int) ~f:(fun succ ->
        if Node.equal gstart succ then 0
        else if Node.equal gend succ then 1
        else match succ.size with
          | `Big -> aux token visited succ
          | `Small ->
            if not @@ List.exists visited ~f:(Node.equal succ) then
              aux token (succ::visited) succ
            else if token then
              aux false visited succ
            else 0
      ) (G.succ g n)
  in aux cheat [] gstart

let make_node name =
  let size = if String.equal name (String.uppercase name) then `Big else `Small in
  Node.{size;name}

let parse_file g data =
  List.iter data ~f:(fun line ->
      match String.split ~on:'-' line with
      | [src;dst] ->
        G.add_edge_e g (G.E.create (make_node src) () (make_node dst))
      | _ -> assert false
    )

let solve g cheat = count_paths ~cheat g (make_node "start") (make_node "end")

let main file =
  let g = G.create () in
  Stdio.In_channel.read_lines file |> parse_file g;
  Stdio.printf "File : %s\n       Part 1 : %d\n       Part 2 : %d\n"
    file (solve g false) (solve g true)

let _ =
  main "example.txt"; main "example2.txt";
  main "example3.txt"; main "input.txt"