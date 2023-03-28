(* zad. 1
	a) ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

	b) 'a -> 'a list -> (b' -> 'a list)

*)

(* zad. 2 *)

let f2 x = raise(Division_by_zero);;

(* zad. 3 *)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt
type 'a graph = Graph of ('a -> 'a list)


let breadthSearch startNode =
    let rec bfsInner visited queue =
        match queue with
        |   [] -> []
        |   node :: tl -> if List.mem node visited then bfsInner visited tl
            else match node with
            |   Node(value, left, right) -> value :: bfsInner (node :: visited) (tl @ (left :: [right]))
            |   Empty -> bfsInner visited tl
    in
    bfsInner [] [startNode]
;;

let tt =
    Node(1,
         Node(2,
            Node(4,
                Empty,
                Empty
            ),
            Empty
         ),
         Node(3,
            Node(5,
                Empty,
                Node(6,
                    Empty,
                    Empty
                )
            ),
        Empty
        )
    );;

breadthBT tt;
(* zad. 4 *)

let depthInner node =
	let rec depthInnerInner node d =
		match node with
		|	Empty -> 0
		|	Node(value, left, right) -> (depthInnerInner left (d+1)) + (depthInnerInner right (d+1)) + d
	in
	depthInnerInner node 0
;;

let depthOuter node =
	let rec depthOuterInner node d =
		match node with
		|	Empty -> d
		|	Node(value, left, right) -> (depthOuterInner left (d+1)) + (depthOuterInner right (d+1))
	in
	depthOuterInner node 0
;;

(*zad 5*)
let depthSearch (Graph g) startNode =
    let rec depthSearchInn visited node =
        if List.mem node visited then visited
        else List.fold_left depthSearchInn (node::visited) (g node)
    in
    List.rev(depthSearchInn [] startNode)
;;

(*zad 3.1*)
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt

let tt =
    Node(1,
         Node(2,
            Node(4,
                Empty,
                Empty
            ),
            Empty
         ),
         Node(3,
            Node(5,
                Empty,
                Node(6,
                    Empty,
                    Empty
                )
            ),
        Empty
        )
    );;

let breadthBT startNode =
    let rec breadthBT_inner queue visited=
        match queue with
        | [] -> List.rev visited
        | hd::tl -> match hd with
                    | Node(v, l, r) -> breadthBT_inner (tl @ [l; r]) (v::visited)
                    | Empty -> breadthBT_inner tl visited
    in
    breadthBT_inner [startNode] []
;;

breadthBT tt;

(*zad 4.1.a*)
let t = Node(1, Empty, Empty);;

let depthInner node =
    let rec depthInnerRec node acc =
        match node with
        | Empty -> 0
        | Node(v, l, r) -> depthInnerRec l (acc + 1) + depthInnerRec r (acc + 1) + acc
    in
    depthInnerRec node 0
;;

depthInner tt;;

(*zad 4.1.b*)
let depthOuter node =
    let rec depthOuterRec node acc =
        match node with
        | Empty -> acc
        | Node(v, l, r) -> depthOuterRec l (acc + 1) + depthOuterRec r (acc + 1)
    in
    depthOuterRec node 0
;;

depthOuter tt;

(*zad 5*)
type 'a graph = Graph of ('a -> 'a list)
let g = Graph (function
    0 -> [3]
    | 1 -> [0;2;4]
    | 2 -> [1]
    | 3 -> []
    | 4 -> [0;2]
    | n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist"))
;;

let depthSearch (Graph g) startNode =
    let rec depthSearchRec visited node =
        match visited with
        | visited when List.mem node visited -> visited
        | _ -> List.fold_left depthSearchRec (node::visited) (g node)
    in
    List.rev (depthSearchRec [] startNode)
;;

depthSearch g 4;;