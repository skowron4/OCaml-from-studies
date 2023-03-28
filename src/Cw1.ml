(*zad 1*)
let rec flatten1 list =
    if list == [] then []
    else List.hd list @ flatten1 (List.tl list)
;;

flatten1 [[1;2;3];[1]];;
let flatten2 list =
    if list == [] then []
    else List.fold_left (@) [] list
;;

(*zad 2*)
let count (e, list) =
    let rec count_inner (e, list, acc) =
        match list with
        | hd::tl when hd == e -> count_inner(e, tl, acc + 1)
        | [] -> acc
        | _::tl -> count_inner(e, tl, acc)
    in
    count_inner (e, list, 0)
;;

count(1, [1; 2; 2; 4; 4; 1; 1; 1]);;

(*zad 3*)
let rec replicate (e, n) =
    if n <= 0 then []
    else e::replicate(e, n - 1)
;;

replicate(3, 5);;

(*zad 4*)
let rec sqrtLIST list =
    match list with
    | hd::tl -> [hd * hd] @ sqrtLIST tl
    | [] -> []
;;

let sqrList list =
    List.fold_right (fun a l -> a*a::l) list []
;;
let sqrl list =
    List.map (fun a -> a*a) list
;;
sqrtLIST [1;2;3;4];;
sqrList [1;2;3;4];;
sqrl [1;2;3;4];;

(*zad 5*)
let palindrome list =
    list == List.rev list
;;

(*zad 6*)
let listLenght list =
    let rec listLenght_inner list acc =
        match list with
        | _::tl -> listLenght_inner tl (acc + 1)
        | [] -> acc
    in
    listLenght_inner list 0
;;
listLenght[1;2;2;4;3];;