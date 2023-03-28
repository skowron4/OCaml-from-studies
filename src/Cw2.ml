(*zad2*)
let rec fib n =
    if n <= 1
    then n
    else fib(n - 1) + fib(n - 2)
;;

fib(42);;

let fibTail n =
    let rec fibTailRec n a b =
        if n = 0
        then a
        else if n = 1
        then b
        else fibTailRec(n - 1) b (a + b)
    in
    fibTailRec n 0 1
;;

fibTail(42);;

(*zad2*)
let root3 a =
    let x0 =
        match a with
        | x when x > 1. -> a/.3.
        | _ -> a
    in
    let dokladnosc = 1e-13 in
    let rec root3Rec x =
        if (abs_float(x *. x *. x -. a) <= dokladnosc *. abs_float(a))
        then (x+.((a/.(x*.x))-.x)/.3.)
        else root3Rec(x+.((a/.(x*.x))-.x)/.3.)
    in
    root3Rec x0
;;

root3 2.;;

(*zad4*)
let zad4a =
    let _::_::x::_ = [-2;-1;0;1;2]
    in
    x
;;

zad4a;;

let zad4b =
    let (_,_)::(x,_)::_ = [(1,2); (0,1)]
    in
    x
;;

zad4b;;

(*zad5*)
let rec initSegment (list1, list2) =
    match (list1, list2) with
    | ([], _) -> true
    | (_, []) -> false
    | (hd1::tl1, hd2::tl2) ->   if hd1 != hd2
                                then false
                                else initSegment(tl1, tl2)

;;

let l1 = [1; 2; 3; 4];;
let l2 = [1; 2; 3; 4];;
initSegment (l1, l2);;

(*zad6*)

let rec replaceNth list n e =
    match list with
    | [] -> []
    | hd::tl -> if n = 0
                then e::tl
                else replaceNth tl (n - 1) e
;;

replaceNth ['o';'l';'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a'] 1 's';;

(*zad 2.1.a*)
let rec fib n =
    match n with
    | n when n == 0 -> 0
    | n when n == 1 || n == 2 -> 1
    | _ -> fib (n - 1) + fib (n - 2)
;;

fib 0;;
fib 1;;
fib 2;;
fib 3;;
fib 4;;
fib 5;;
fib 6;;
fib 14;;
(*zad 2.1.b*)
let fibTail n =
    let rec fibTail_rec n f s =
        match n with
        | n when n > 0 -> fibTail_rec (n - 1) s (f + s)
        | _ -> f
    in
    fibTail_rec n 0 1
;;
fibTail 0;;
fibTail 1;;
fibTail 2;;
fibTail 3;;
fibTail 4;;
fibTail 5;;
fibTail 6;;
fibTail 14;;

(*zad 3.1*)
let root3 a =
    let x =
        match a with
        | a when a > 1. -> a/.3.
        | _ -> a
    in
    let dokladnosc = 1e-14 in
    let rec root3_inner x =
        match x with
        | x when abs_float(x *. x *. x -. a) <= dokladnosc *. abs_float(a) -> x
        | _ ->  root3_inner (x +. (a /. (x *. x) -. x) /. 3.)
    in
    root3_inner x
;;
root3 3.;;

(*zad 4.1.a*)
let zad4a =
    let _::_::x::_ = [-2; -1; 0; 1; 2]
    in
    x
;;

(*zad 4.1.b*)
let zad4b =
    let (_,_)::(x,_)::_ = [ (1,2); (0,1) ]
    in
    x
;;

(*zad 5.1*)
let rec initSegment (list1, list2) =
    match (list1, list2) with
    | ([], _) -> true
    | (_::_, []) -> false
    | (hd1::tl1, hd2::tl2) when hd1 == hd2 -> initSegment (tl1, tl2)
    | _ -> false
;;

let l1 = [1; 2; 3; 4; 6];;
let l2 = [1; 2; 3; 4];;
initSegment (l1, l2);;

(*zad 6.1.a*)
let rec replaceNth list1 nr e =
    match (list1, nr) with
    | (hd::tl, nr) when nr > 0 -> hd::(replaceNth tl (nr - 1) e)
    | (hd::tl, _) -> e::tl
;;

replaceNth ['o';'l';'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a'] 1 's';;