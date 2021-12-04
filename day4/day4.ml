type cell = Notinit | Notmarked of int | Marked of int

type board = {
  grid : cell Array.t Array.t;
  mutable win : bool;
}

let get_first_Notinit arr =
  let rec aux i =
    if i < Array.length arr then begin
      if Array.get arr i = Notinit
      then i
      else aux (i+1)
    end
    else assert false
  in
  aux 0

let parse_file data =
  let rec aux todo current_grid numbers grids =
    match todo with
    | [] ->
      (match current_grid with
       | None -> numbers,grids
       | Some grid -> numbers,({grid;win=false}::grids))
    | hd :: tail ->
      if String.contains hd ',' then
        aux tail current_grid (List.map int_of_string @@ String.split_on_char ',' hd) grids
      else begin
        try
          if String.equal hd "" then
            match current_grid with
            | None -> aux tail None numbers grids
            | Some grid -> aux tail None numbers ({grid;win=false}::grids)
          else begin
            Scanf.sscanf (String.trim hd) "%d %d %d %d %d" (fun a b c d e ->
                match current_grid with
                | None ->
                  let curr = Array.init 5 (fun _ -> Array.make 5 Notinit) in
                  List.iteri (fun i v ->
                      Array.set (Array.get curr i) 0 (Notmarked v)
                    ) [a;b;c;d;e];
                  aux tail (Some curr) numbers grids
                | Some grid ->
                  let first = get_first_Notinit (Array.get grid 0) in
                  List.iteri (fun i v ->
                      Array.set (Array.get grid i) first (Notmarked v)
                    ) [a;b;c;d;e];
                  aux tail (Some grid) numbers grids
              )
          end
        with
        | _ -> aux tail current_grid numbers grids
      end
  in
  aux data None [] []

let mark_number_grid v b =
  let pos = ref None in
  Array.iteri ( fun x c ->
      Array.iteri ( fun y n ->
          match n with
          | Notmarked n when n = v ->
            Array.set c y (Marked n);
            pos := Some (x,y)
          | _ -> ()
        ) c
    ) b.grid;
  !pos,b

let is_bingo_r (_,y) g =
  let exception NotBingo in
  try
    Array.iter (fun c ->
        match Array.get c y with
        | Notmarked _ -> raise NotBingo
        | _ -> ()
      ) g;
    true
  with
    NotBingo -> false

let is_bingo_c (x,_) g =
  let exception NotBingo in
  let c = Array.get g x in
  try
    Array.iter (fun v ->
        match v with
        | Notmarked _ -> raise NotBingo
        | _ -> ()
      ) c;
    true
  with
    NotBingo -> false

let compute_score nb g =
  let cpt = ref 0 in
  Array.iter (fun c ->
      Array.iter (fun v ->
          match v with
          | Notmarked x -> cpt := !cpt + x
          | _ -> ()
        ) c

    ) g;
  !cpt * nb

let is_winner pos g =
  match pos with
  | Some (x,y) ->
    is_bingo_c (x,y) g || is_bingo_r (x,y) g
  | _ -> false

let solv1 nbs (grids:board list) =
  let rec aux todo =
    match todo with
    | [] -> failwith "No win"
    | n :: tail ->
      let res = List.map (fun b -> mark_number_grid n b) grids in
      let winner =
        List.find_opt (fun (pos,b) -> is_winner pos b.grid) res
      in
      match winner with
      | None -> aux tail
      | Some (_,g) -> Printf.printf "Part 1 : %d\n" (compute_score n g.grid)
  in aux nbs


let solv2 nbs grids =
  let rec aux todo grids last =
    match todo with
    | [] -> Printf.printf "Part 2 : %d\n" last
    | n :: tail ->
      let res = List.map (fun b -> mark_number_grid n b) grids in
      let winners =
        List.find_all (fun (pos,b) -> is_winner pos b.grid) res
      in
      match winners with
      | [] -> aux tail grids last
      | (_,hd)::_ ->
        List.iter (fun (_,b) -> b.win <- true) winners;
        aux tail (List.filter (fun b -> not b.win) grids) (compute_score n hd.grid)
  in aux nbs grids 0


let main file =
  let nbs,grids = Arg.read_arg file |> Array.to_list |> parse_file in
  Printf.printf "%d tirages, %d grilles\n" (List.length nbs) (List.length grids);
  solv1 nbs grids;
  solv2 nbs grids


let _ = main "example.txt"; main "input.txt"