open Base

type board = {
  grid : (bool*int) Array.t Array.t;
  mutable win : bool;
}

let to_int_arr s =
  String.split ~on:' ' s
  |> List.filter ~f:(fun s -> not @@ String.equal "" s)
  |> List.map ~f:(fun s -> (false, Int.of_string s))
  |> List.to_array

let parse_file data =
  match data with
  | [] -> failwith "empty file"
  | nbs :: grids ->
    let numbers = String.split ~on:',' nbs |> List.map ~f:Int.of_string in
    let rec aux to_read grids =
      match to_read with
      | [] -> numbers, grids
      | "" :: rest -> aux rest grids
      | l1 :: l2 :: l3 :: l4 :: l5 :: rest ->
        let arr_lines = List.map ~f:to_int_arr [l1;l2;l3;l4;l5] in
        let grid = List.to_array arr_lines |> Array.transpose_exn in
        aux rest ({grid;win=false}::grids)
      | _ -> failwith "wrong file format"
    in
    aux grids []

let mark_number_grid v b =
  let pos = ref None in
  Array.iteri ~f:(fun x c ->
      Array.iteri ~f:(fun y (status,n) ->
          if not status && n = v
          then begin
            pos := Some (x,y);
            c.(y) <- (true, n)
          end
        ) c
    ) b.grid;
  !pos

let is_bingo_col x g =
  not @@ Array.exists ~f:(fun (status,_) -> not status) g.(x)

let is_bingo_row y g =
  Array.transpose_exn g |> is_bingo_col y

let compute_score nb g =
  Array.fold g ~init:0 ~f:(fun n c ->
      Array.fold c ~init:n ~f:(fun n' (status,v) ->
          if not status then n'+v else n'
        )
    ) * nb

let is_winner pos g =
  match pos with
  | Some (x,y) ->
    is_bingo_col x g || is_bingo_row y g
  | _ -> false

let solv1 nbs grids =
  let rec aux todo =
    match todo with
    | [] -> failwith "No win"
    | n :: tail ->
      let winner =
        List.map ~f:(fun b -> mark_number_grid n b) grids |>
        List.findi ~f:(fun i pos ->
            is_winner pos (List.nth_exn grids i).grid
          )
      in
      match winner with
      | None -> aux tail
      | Some (i,_) ->
        Stdio.printf "Part 1 : %d\n"
          (compute_score n (List.nth_exn grids i).grid)
  in aux nbs

let solv2 nbs grids =
  let rec aux todo grids last =
    match todo with
    | [] -> Stdio.printf "Part 2 : %d\n" last
    | n :: tail ->
      let winners =
        let cells = List.map ~f:(fun b -> mark_number_grid n b) grids in
        List.filteri ~f:(fun i b ->
            is_winner (List.nth_exn cells i) b.grid
          ) grids
      in
      match winners with
      | [] -> aux tail grids last
      | hd::_ ->
        List.iter ~f:(fun b -> b.win <- true) winners;
        aux tail (List.filter ~f:(fun b -> not b.win) grids) (compute_score n hd.grid)
  in aux nbs grids 0

let reset_board b =
  Array.iter ~f:(fun c ->
      Array.iteri ~f:(fun y (_,i) ->
          c.(y) <- (false,i)
        ) c;
    ) b.grid

let reset_boards boards =
  List.iter ~f:reset_board boards

let main file =
  let nbs,grids = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "%d tirages, %d grilles\n" (List.length nbs) (List.length grids);
  solv1 nbs grids;
  reset_boards grids;
  solv2 nbs grids

let _ = main "example.txt"; main "input.txt"