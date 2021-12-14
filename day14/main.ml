open Base

let inc_m ?(v=1) k m =
  Map.update m k ~f:(function Some v' -> v + v' | None -> v)

let pmap s =
  let rec aux todo m =
    match todo with
    | c1 :: c2 :: rest ->
      aux (c2::rest) (inc_m (String.of_char_list [c1; c2]) m)
    | _ -> m
  in aux (String.to_list s) (Map.empty (module String))

let fmap s =
  String.fold s ~init:(Map.empty (module Char)) ~f:(fun m c -> inc_m c m)

let parse_file data =
  match data with
  | word :: "" :: rules ->
    List.fold ~init:(Map.empty (module String)) ~f:(fun m l ->
        Caml.Scanf.sscanf l "%s -> %c" (fun p c -> Map.add_exn m ~key:p ~data:c)
      ) rules, pmap word, fmap word
  | _ -> assert false

let step patterns (pm, fm) =
  let aux ~key ~data (pm, fm) =
    match Map.find patterns key with
    | None -> (pm, fm)
    | Some c ->
      let p1 = String.of_char_list [key.[0];c] in
      let p2 = String.of_char_list [c;key.[1]] in
      let pm = inc_m ~v:(-data) key pm |> inc_m ~v:data p1 |> inc_m ~v:data p2 in
      (pm, inc_m ~v:data c fm)
  in
  Map.fold pm ~init:(pm,fm) ~f:aux

let solve n (patterns, pmap, fmap) =
  let _,fmap = Fn.apply_n_times ~n (step patterns) (pmap,fmap) in
  let min_max ~key:_ ~data (mi,ma) = Int.min mi data, Int.max ma data in
  let min,max = Map.fold ~init:(Int.max_value, 0) ~f:min_max fmap
  in max - min

let main file =
  let data = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "File : %s\n       Part 1 : %d\n       Part 2 : %d\n"
    file (solve 10 data) (solve 40 data)

let _ = main "example.txt";  main "input.txt"