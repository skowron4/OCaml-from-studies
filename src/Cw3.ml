(*zad2*)
let curry3 f x y z = f (x, y, z);;
let curry3a f = fun x y z -> f (x, y, z);;

let uncurry3 f (x, y, z) = f x y z;;
let uncurry3a f = fun (x, y, z) -> f x y z;;

let plus (x, y, z) = x + y + z;;
let pluswls = fun (x, y, z) -> x + y + z;;

let add x y z = x + y + z;;
let addwls = fun x y z -> x + y + z;;

curry3 plus 3 4 5;;
curry3a plus 3 4 5;;
curry3 pluswls 3 4 5;;

uncurry3 add (3, 4, 5);;
uncurry3a add (3, 4, 5);;
uncurry3 addwls (3, 4 ,5);;

(*zad3.1.*)
let sumProd list = List.fold_left(fun acc head -> (fst acc + head, snd acc * head)) (0, 1) list;;
sumProd [1; 2; 3; 4; 5; 6];;
(*zad3.2.*)
let sumProd2 list = List.fold_left(fun (first, second) head -> (first + head, second * head)) (0, 1) list;;
sumProd2 [1; 2; 3; 4; 5; 6];;



(*zad4 a*) (*nieskończona rekurencja*)
let rec quicksort = function
    [] -> []
    | [x] -> [x]
    | xs -> let small = List.filter (fun y -> y < List.hd xs ) xs
    and large = List.filter (fun y -> y >= List.hd xs ) xs
    in quicksort small @ quicksort large;;

(*zad4 b*) (*nie porownujemy dwoch gowien*)
let rec quicksort' = function
    [] -> []
    | x::xs -> let small = List.filter (fun y -> y < x ) xs
    and large = List.filter (fun y -> y > x ) xs
    in quicksort' small @ (x :: quicksort' large);;


(*zad5 a*)
(*1*)
let rec insertion f list el =
    match list with
    | [] -> [el]
    | hd::tl when f hd el -> hd::insertion f list el
    | _ -> el::list
;;

let insertionsort f list = List.fold_left (insertion f) [] list;;

insertionsort (>) [3;2;4;5;1;2;2;7;6];;
insertionsort (<) [3;2;4;5;1;2;2;7;6];;

(*2*)

let rec insertsort f list =
    match list with
    | [] -> []
    | hd::tl -> insertion f (insertionsort f tl) hd
;;

insertsort (>) [3;2;4;5;1;2;2;7;6];;
insertsort (<) [3;2;4;5;1;2;2;7;6];;

(*zad5 b*)

let rec merge f list1 list2 =
    match (list1, list2) with
    | (_, []) -> list1
    | ([], _) -> list2
    | (hd1::tl1, hd2::tl2) ->
        match f hd1 hd2 with
        | true -> hd1::merge f tl1 list2
        | _ -> hd2::merge f list1 tl2
;;

let halfList list =
    let rec halfListRec list l1 l2 =
        match list with
        | [] -> (l1, l2)
        | hd::tl -> halfListRec tl (hd::l2) l1
    in
    halfListRec list [] []
;;

let rec mergesort f list =
    match list with
    | [] -> []
    | [x] -> [x]
    | _ -> let (l1, l2) = halfList list in
                merge f (mergesort f l1) (mergesort f l2)
;;

mergesort (>) [3;2;4;5;1;2;2;7;6];;
mergesort (<) [3;2;4;5;1;2;2;7;6];;

(*zad 2.1*)
let curry3 f x y z = f (x, y, z);;
let carry3WLS f = fun x y z -> f(x, y, z);;

let uncurry3 f x y z = f (x, y, z);;
let uncarry3WLS f = fun (x, y, z) -> f x y z;;

(*zad 3.1*)
let sumProd list =
    List.fold_left (fun acc hd -> (hd + fst acc, hd * snd acc)) (0, 1) list
;;
sumProd [1; 2; 3; 4; 5; 6];;

(*zad 4.1.a*)
let rec quicksort = function
 [] -> []
 | [x] -> [x]
 | xs -> let small = List.filter (fun y -> y <= List.hd xs ) xs
 and large = List.filter (fun y -> y > List.hd xs ) xs
 in quicksort small @ quicksort large;;
quicksort [1; 3; 2; 7; 5; 6];;


(*zad 4.1.*)
 let rec quicksort' = function
 [] -> []
 | x::xs -> let small = List.filter (fun y -> y < x ) xs
 and large = List.filter (fun y -> y > x ) xs
 in quicksort' small @ (x :: quicksort' large);;
quicksort [1; 3; 2; 7; 5; 6];;

(*zad 5.1.a*)
let rec insertion f list el =
    match list with
    | [] -> [el]
    | hd::tl when f hd el -> hd::insertion f list el
    | _ -> el::list
;;

let insertionSort list f =
    List.fold_left (insertion f) [] list
;;

let rec insertsort list f =
    match list with
    | [] -> []
    | hd::tl -> insertion f (insertionSort tl f) hd
;;

insertsort [3;2;4;5;1;2;2;7;6] (>);;

(*zad 5.1b*)
let split list =
    let rec split_rec list l1 l2 =
        match list with
        | [] -> (l1, l2)
        | hd::tl -> split_rec tl (hd::l2) l1
    in
    split_rec list [] []
;;

let rec mergesort_inner f l1 l2 =
    match (l1, l2) with
    | ([], _) -> l2
    | (_, []) -> l1
    | (hd1::tl1, hd2::tl2) ->
        match f hd1 hd2 with
        | true -> hd1::mergesort_inner f tl1 l2
        | _ -> hd2::mergesort_inner f l1 tl2
;;

let rec mergesort f list =
    match list with
    | [] -> []
    | [x] -> [x]
    | _ ->  let (l1, l2) = split list in
                mergesort_inner f (mergesort f l1) (mergesort f l2)
;;

mergesort (>) [3;2;4;5;1;2;2;7;6];;
mergesort (<) [3;2;4;5;1;2;2;7;6];;