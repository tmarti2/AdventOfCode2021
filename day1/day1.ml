let catch_errors = function
  | Result.Ok res -> Printf.printf "Result : %d\n" res
  | Result.Error (`Msg err) -> Printf.printf "Error : %s" err
  | Result.Error _ -> Printf.printf "Unknown error"

let part_one chan =
  let rec aux acc prev =
    try
      let depth = Scanf.bscanf chan "%d\n" (fun depth -> depth) in
      match prev with
      | None -> aux 0 (Some depth)
      | Some depth' ->
        if depth > depth' then
          aux (acc+1) (Some depth)
        else
          aux acc (Some depth)
    with
    | End_of_file -> Result.ok acc
    | Failure msg
    | Scanf.Scan_failure msg -> Result.error (`Msg msg)
  in
  aux 0 None

let sum q =
  Queue.fold (fun acc v -> acc + v) 0 q

let part_two chan =
  let prev_queue = Queue.create () in
  let rec aux acc prev =
    try
      let depth = Scanf.bscanf chan "%d\n" (fun depth -> depth) in
      match Queue.length prev_queue with
      | 0 | 1 | 2 ->
        Queue.push depth prev_queue;
        aux acc prev
      | 3 ->
        ignore(Queue.pop prev_queue);
        Queue.push depth prev_queue;
        let curr = sum prev_queue in
        if curr > prev then
          aux (acc+1) curr
        else
          aux acc curr
      | _ -> Result.error (`Msg "Queue larger than 4, should not be possible")
    with
    | End_of_file -> Result.ok acc
    | Failure msg
    | Scanf.Scan_failure msg -> Result.error (`Msg msg)
  in
  aux 0 0

let main () =
  let chan1 = Scanf.Scanning.from_file "./input.txt" in
  let chan2 = Scanf.Scanning.from_file "./input.txt" in
  part_one chan1 |> catch_errors;
  part_two chan2 |> catch_errors;
  Scanf.Scanning.close_in chan1;
  Scanf.Scanning.close_in chan2

let _ = main ()