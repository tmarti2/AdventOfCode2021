type dir = Forward of int | Down of int | Up of int

let part_one data =
  let pos, depth =
    List.fold_left (fun (pos,depth) instr ->
        match instr with
        | Forward v -> (pos+v,depth)
        | Down v -> (pos,depth+v)
        | Up v -> (pos,depth-v)
      ) (0,0) data
  in pos * depth

let part_two data =
  let pos, depth, _ =
    List.fold_left (fun (pos,depth,aim) instr ->
        match instr with
        | Forward v -> (pos+v,depth+aim*v,aim)
        | Down v -> (pos,depth,aim+v)
        | Up v -> (pos,depth,aim-v)
      ) (0,0,0) data
  in pos * depth

let parse_line line =
  Scanf.sscanf line "%s %d" (fun dir value ->
      match dir with
      | "forward" -> Forward value
      | "down" -> Down value
      | "up" -> Up value
      | _ -> failwith "Wrong instruction"
    )

let main () =
  let data = Arg.read_arg "input.txt" |> Array.to_list |> List.map parse_line in
  let res1, res2 = part_one data, part_two data in
  Printf.printf "Part one : %d\n" res1;
  Printf.printf "Part two : %d\n" res2


let _ = main ()