type operator = {id : int; packets : packet list}
and ptype = Literal of int | Operator of operator
and packet = {version : int; ptype : ptype}

let parse_literal l =
  let rec aux acc todo =
    match%bitstring todo with
    | {| false : 1; v : 4; rest : -1 : bitstring |} ->
      Literal (acc lsl 4 lor v), rest
    | {| true : 1; v : 4;rest : -1 : bitstring |} ->
      aux (acc lsl 4 lor v) rest
  in aux 0 l

let rec parse_operator id o =
  match%bitstring o with
  | {| false : 1; len : 15; subp : len : bitstring; rest : -1 : bitstring |} ->
    Operator({id;packets = packet_list [] subp}), rest
  | {| true : 1; gsize : 11; rest : -1 : bitstring |} ->
    let packets, rest = packet_group gsize [] rest in
    Operator({id;packets}), rest

and parse_packet b =
  match%bitstring b with
  | {| version : 3; id : 3; rest : -1 : bitstring |} ->
    let ptype, rest =
      match id with
      | 4 -> parse_literal rest
      | _ -> parse_operator id rest
    in {version;ptype}, rest
  | {| _ |} -> assert false

and packet_list acc bl =
  if Bitstring.bitstring_length bl = 0 then List.rev acc
  else let p, r = parse_packet bl in packet_list (p::acc) r

and packet_group n acc bg =
  if n = 0 then List.rev acc,bg
  else
    let p, r = parse_packet bg in
    packet_group (n-1) (p::acc) r

let rec solve1 p =
  p.version +
  match p.ptype with
  | Literal _ -> 0
  | Operator {packets;_} -> Base.List.sum (module Base.Int) ~f:solve1 packets

let rec solve2 p =
  match p.ptype with
  | Literal i -> i
  | Operator {id;packets} ->
    let vl = List.map solve2 packets in
    match id,vl with
    | 0, _ -> List.fold_left (+) 0 vl
    | 1, _ -> List.fold_left ( * ) 1 vl
    | 2, _ -> Base.List.min_elt vl ~compare |> Option.get
    | 3, _ -> Base.List.max_elt vl ~compare |> Option.get
    | 5, [p1;p2] -> if p1 > p2 then 1 else 0
    | 6, [p1;p2] -> if p1 < p2 then 1 else 0
    | 7, [p1;p2] -> if p1 = p2 then 1 else 0
    | _ -> assert false

let main file =
  let data = Stdio.In_channel.read_lines file in
  let data =
    `Hex (List.hd data) |> Hex.to_string
    |> Bitstring.bitstring_of_string
  in
  let packet,_ = parse_packet data in
  Stdio.printf "File : %s\n       Part 1 : %d\n       Part 2 : %d\n"
    file (solve1 packet) (solve2 packet)

let _ = main "example.txt";main "example2.txt";main "example3.txt"
      ; main "input.txt"