let catch_errors = function
  | Result.Ok (res1,res2) ->
    Printf.printf "Result : \n Part one : %d\n Part two : %d\n" res1 res2
  | Result.Error (`Msg err) -> Printf.printf "Error : %s" err
  | Result.Error _ -> Printf.printf "Unknown error"

(* Remove head and push new element, Queue like *)
let cycle l v = List.tl l @ [v]

(* Sum list elements *)
let count l = List.fold_left (fun acc v -> acc+v) 0 l

let parse_data chan =
  let rec aux acc1 acc2 prev_queue prev_window =
    try
      let depth = Scanf.bscanf chan "%d\n" (fun depth -> depth) in
      let size = List.length prev_queue in
      match size with
      | 0 -> aux acc1 acc2 [depth] prev_window
      | 1 | 2 ->
        let acc1 = (* Part one, compare to previous depth *)
          let prev = List.nth prev_queue (size-1) in
          if depth > prev then acc1+1 else acc1
        in
        (* First three-measurement sliding window *)
        let curr_window = if size = 2 then count prev_queue + depth else 0 in
        aux acc1 acc2 (prev_queue@[depth]) curr_window
      | 3 ->
        let acc1 = (* Part one, compare to previous depth *)
          let prev = List.nth prev_queue (size-1) in
          if depth > prev then acc1+1 else acc1
        in
        let prev_queue = cycle prev_queue depth in (* Next sliding window *)
        let curr_window = count prev_queue in
        if curr_window > prev_window then (* Part two, compare to previous 3 values *)
          aux acc1 (acc2+1) prev_queue curr_window
        else
          aux acc1 acc2 prev_queue curr_window
      | _ -> assert false
    with
    | End_of_file -> Result.ok (acc1,acc2)
    | Failure msg
    | Scanf.Scan_failure msg -> Result.error (`Msg msg)
  in
  aux 0 0 [] 0

let main () =
  let chan = Scanf.Scanning.from_file "./input.txt" in
  parse_data chan |> catch_errors;
  Scanf.Scanning.close_in chan

let _ = main ()