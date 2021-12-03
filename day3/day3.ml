let invert arr =
  Array.map (fun v -> if v = 1 then 0 else 1) arr

let arr_to_string arr =
  Array.fold_left (fun acc v -> acc ^ (string_of_int v)) "" arr

let arr_to_int arr =
  int_of_string @@ "0b" ^ (arr_to_string arr)

let part_one data size =
  let sum = (Array.make size 0) in
  let tt = List.length data in
  List.iter (fun s ->
      String.iteri (fun i c ->
          let v = if c = '1' then 1 else 0 in
          Array.set sum i (Array.get sum i + v)
        ) s
    )  data;
  let g = Array.map (fun v -> if v > tt / 2 then 1 else 0) sum in
 let e = invert g in
 (arr_to_int g) * (arr_to_int e)

let main file =
  let data = Arg.read_arg file |> Array.to_list in
  let size = String.length (List.hd data) in
  let res1 = part_one data size in
  Printf.printf "Part one : %d\n" res1


let _ = main "example.txt"; main "input.txt"