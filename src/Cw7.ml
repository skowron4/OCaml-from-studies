(*zad 1*)

module type QUEUE_FUN =
sig
    type 'a t
     exception Empty of string
     val empty: unit -> 'a t
     val enqueue: 'a * 'a t -> 'a t
     val dequeue: 'a t -> 'a t
     val first: 'a t -> 'a
     val isEmpty: 'a t -> bool
end;;

module QueueList : QUEUE_FUN =
struct
    type 'a t = 'a list
    exception Empty of string

    let empty() = []
    let enqueue(e, queue) = queue @ [e]
    let dequeue queue =
        match queue with
        | _::t -> t
        | _ -> []
    let first queue =
        match queue with
        | h::_ -> h
        | _ -> raise (Empty "module QueueList: first")
    let isEmpty queue =
        match queue with
        | [] -> true
        | _ -> false
end;;

module QueueDubleList : QUEUE_FUN =
struct
    type 'a t = 'a list * 'a list
    exception Empty of string

    let normalize q =
        match q with
        | ([], yl) -> ((List.rev yl), [])
        | _ -> q
    let empty() = ([],[])
    let isEmpty q = fst q = []
    let enqueue(elem, q) = normalize (fst q, elem::snd q)
    let dequeue q =
        match q with
        | ([], _) -> ([], [])
        | (_::tl, yl) -> normalize (tl, yl)
    let first q =
        if (isEmpty q) then raise (Empty "module: QueueListPair fun: first")
        else List.hd (fst q)
end;;

(*zad 2*)

module type QUEUE_MUT =
sig
 type 'a t

 exception Empty of string
 exception Full of string

 val empty: int -> 'a t
 val enqueue: 'a * 'a t -> unit
 val dequeue: 'a t -> unit
 val first: 'a t -> 'a
 val isEmpty: 'a t -> bool
 val isFull: 'a t -> bool
end;;

module QueueCyclicArray : QUEUE_MUT =
struct
    type 'a t = {mutable f: int; mutable r: int; mutable size: int; mutable arr: 'a option array}

    exception Empty of string
    exception Full of string
    exception NotPositiveSize of string

    let empty(s) =
        if (s<=0) then raise (NotPositiveSize "module: QueueArray fun: empty")
        else {f = 0; r = 0; size = s + 1; arr = Array.make (s + 1) None}
    let isFull queue = queue.f = ((queue.r + 1) mod queue.size)
    let enqueue (e, queue) =
        if (isFull queue)
        then raise (Full "Queue is full")
        else
            queue.arr.(queue.r) <- Some e;
            queue.r <- ((queue.r + 1) mod queue.size)
    let dequeue queue =
        queue.arr.(queue.f) <- None;
        queue.f <- ((queue.f + 1) mod queue.size)
    let first queue =
        match queue.arr.(queue.f) with
        | Some e -> e
        | None -> raise (Empty "Queue is Empty!!!")
    let isEmpty queue =
        queue.arr.(queue.f) = None
end;;

(*module QueueArray: QUEUE_MUT = struct*)
(*    type 'a t = {arr: 'a option array; mutable f: int; mutable r: int; size: int}*)
(*    exception Empty of string*)
(*    exception Full of string*)
(*    exception NotPositiveSize of string*)

(*    let empty s =*)
(*        if (s<=0) then raise (NotPositiveSize "module: QueueArray fun: empty")*)
(*        else {arr = Array.make (s + 1) None; f = 0; r = 0; size = s + 1}*)
(*    let isEmpty q = q.f = q.r*)
(*    let isFull q = q.f = ((q.r + 1) mod q.size)*)
(*    let enqueue(elem, q) =*)
(*        if (isFull q) then raise (Full "module: QueueArray fun: enqueue")*)
(*        else*)
(*            q.arr.(q.r) <- Some elem;*)
(*            q.r <- ((q.r + 1) mod q.size)*)
(*    let dequeue q = q.f <- ((q.f + 1) mod q.size)*)
(*    let first q =*)
(*        if (isEmpty q) then raise (Empty "module: QueueArray fun: first")*)
(*        else Option.get q.arr.(q.f)*)
(*end;;*)

let q = (QueueCyclicArray.empty 4);;
QueueCyclicArray.enqueue (3, q);;
QueueCyclicArray.enqueue (3, q);;
QueueCyclicArray.enqueue (3, q);;
QueueCyclicArray.enqueue (3, q);;

(*zad 1.1.a*)
module Queue: QUEUE_FUN =
struct
    type 'a t = 'a list
    exception Empty of string

    let empty() = []
    let enqueue(el, queue) = queue @ [el]
    let dequeue queue =
        match queue with
        | _::tl -> tl
        | _ -> []
    let first queue =
        match queue with
        | hd::_ -> hd
        | _ ->  raise (Empty "queue is empty")
    let isEmpty queue = queue == []
end;;

(*zad 1.1.b*)
module DubleListQueu: QUEUE_FUN =
struct
    type 'a t = 'a list * 'a list
    exception Empty of string

    let normalize queue =
        match queue with
        | ([], l2) -> (List.rev l2, [])
        | (_, _) -> queue
    let empty() = ([], [])
    let isEmpty queue =
        match queue with
        | ([], []) -> true
        | (_, _) -> false
    let enqueue(el, queue) =
        match queue with
        | (l1, l2) -> normalize (l1, el::l2)
    let dequeue queue =
        match queue with
        | (_::tl, l2) -> normalize(tl, l2)
        | ([], _) -> ([], [])
    let first queue =
        match queue with
        | (hd::_, _) -> hd
        | ([], []) -> raise (Empty "queue is empty")
end;;

(*zad 2.1*)
module type QUEUE_MUT1 =
sig
 type 'a t

 exception Empty of string
 exception Full of string

 val empty: int -> 'a t
 val enqueue: 'a * 'a t -> unit
 val dequeue: 'a t -> unit
 val first: 'a t -> 'a
 val isEmpty: 'a t -> bool
 val isFull: 'a t -> bool
end;;

module QueueCyclic: QUEUE_MUT1 =
struct
    type 'a t = {mutable f: int; mutable r: int; mutable size: int; mutable arr: 'a option array}

    exception Empty of string
    exception Full of string
    exception NoPositiveSize of string

    let empty (s) =
        match s with
        | s when s > 0 -> {f = 0; r = 0; size =  s + 1; arr =  Array.make(s + 1) None}
        | _ -> raise (NoPositiveSize "size <= 0")
    let isFull queue =
            queue.f == ((queue.r + 1) mod queue.size)
    let enqueue(e, queue) =
        if (isFull queue) then raise (Full "Queue is full")
        else
            queue.arr.(queue.r) <- Some e;
            queue.r <- ((queue.r + 1) mod queue.size)
    let dequeue queue =
        queue.arr.(queue.f) <- None;
        queue.f <- ((queue.f + 1) mod queue.size)
    let first queue =
        match queue.arr.(queue.f) with
        | Some e -> e
        | None -> raise (Empty "Queue is Empty!!!")
    let isEmpty queue =
        queue.arr.(queue.f) == None;
end;;

let q = (QueueCyclicArray.empty 4);;
QueueCyclicArray.enqueue (3, q);;
QueueCyclicArray.enqueue (3, q);;
QueueCyclicArray.enqueue (3, q);;
QueueCyclicArray.enqueue (3, q);;

let f y x z = (x z) (y x)