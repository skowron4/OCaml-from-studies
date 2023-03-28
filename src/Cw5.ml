type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

let rec itake = function
    | (0, _) -> []
    | (_, LNil) -> []
    | (n, LCons(x, lazy xs)) -> x::itake(n - 1, xs)
;;

(*zad1*)
let irepeat (k, lazylist) =
    let rec irepeat_rec a llist =
        match (a, llist) with
        | (_, LNil) -> LNil
        | (0, LCons(_, lazy tl)) -> irepeat_rec k tl
        | (_, LCons(hd, _)) -> LCons(hd, lazy(irepeat_rec (a - 1) llist))
    in
    irepeat_rec k lazylist
;;

itake(10, irepeat(3, (lfrom 1)));;

(*zad2*)
let lfib =
    let rec fib_rec a b =
        LCons(a, lazy (fib_rec b (a + b)))
    in
    fib_rec 0 1
;;

itake(10, lfib);;

(*let lfib =*)
(*    let rec lfibIn(p, n) =*)
(*        LCons(p+n, lazy(lfibIn(n, p+n))) in*)
(*    LCons(1, lazy(LCons(1, lazy(lfibIn(1, 1)))));;*)

(*itake(15, lfib);;*)

(*zad3*)
type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;

(*b*)
let rec lTree n =
    LNode(n, (fun() -> lTree (2 * n)), (fun() -> lTree(2 * n + 1)));;
;;

(*a*)
let lBreadth tree =
    let rec lBreadth_rec queue =
        match queue with
        | [] -> LNil
        | LEmpty::tl -> lBreadth_rec tl
        | LNode(v, n1, n2)::tl -> LCons(v, lazy (lBreadth_rec (tl@[n1(); n2()])))
    in
    lBreadth_rec [tree]
;;

itake(20,lBreadth(lTree 1));;

(*zad 1.1*)
let lrepeat k lazyList =
    let rec lreapeat_rec n ll =
        match (n, ll) with
        | (_, LNil) -> LNil
        | (0, LCons(_, lazy ltl)) -> lreapeat_rec k ltl
        | (x, LCons(v, lazy ltl)) -> LCons(v, lazy (lreapeat_rec (x - 1) ltl))
    in
    lreapeat_rec k lazyList
;;
itake(10, irepeat(3, (lfrom 1)));;

(*zad 2.1.a*)
let lFib =
    let rec lFib_rec f s =
        LCons(f + s, lazy (lFib_rec s (f + s)))
    in
    lFib_rec 0 1
;;

(*zad 3.1.a*)
let lBreadth tree =
    let rec lBreadthRec queue =
        match queue with
        | [] -> LNil
        | LNode(v, l, p)::tl -> LCons(v, lazy(lBreadthRec (tl@[l(); p()])))
        | LEmpty::tl -> lBreadthRec tl
    in
    lBreadthRec [tree]
;;

(*zad 3.1.b*)
let rec lTree n =
    LNode(n, (fun() -> lTree (2*n)), (fun() -> lTree (2* n + 1)))
;;