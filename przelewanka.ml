(* Autor: Jonasz Aleszkiewicz *)

exception Found of int

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let filter_array f t = Array.of_list @@ List.filter f @@ Array.to_list t

let przelewanka arr =
  try
    let arr = filter_array (fun (cap, _) -> cap > 0) arr in
    let n = Array.length arr in
    let cap = Array.map fst arr in
    let goal = Array.map snd arr in
    let zeroes = Array.make n 0 in

    if goal = zeroes then raise @@ Found 0;
    if not @@ Array.exists (fun (cap, goal) -> goal = 0 || goal = cap) arr
    then raise Not_found;
    if not @@ Array.for_all (fun x -> x mod Array.fold_left gcd 1 goal = 0) goal
    then raise Not_found;

    let iter_substates f state =
      for i = 0 to n - 1 do
        let a = state.(i) in

        state.(i) <- 0;
        f state;
        state.(i) <- cap.(i);
        f state;

        for j = 0 to n - 1 do
          if i <> j
          then (
            let b = state.(j) in
            let delta = min a (cap.(j) - b) in

            state.(i) <- a - delta;
            state.(j) <- b + delta;
            f state;

            state.(j) <- b)
        done;

        state.(i) <- a
      done
    in

    let seen = Hashtbl.create 1 in
    let queue = Queue.create () in
    Queue.push (zeroes, 0) queue;
    while not @@ Queue.is_empty queue do
      let state, steps = Queue.take queue in
      let steps' = steps + 1 in
      state
      |> iter_substates (fun state' ->
             if state' = goal then raise (Found steps');
             if not @@ Hashtbl.mem seen state'
             then (
               Hashtbl.add seen (Array.copy state') ();
               Queue.push (Array.copy state', steps') queue))
    done;
    raise Not_found
  with
  | Found n -> n
  | Not_found -> -1
