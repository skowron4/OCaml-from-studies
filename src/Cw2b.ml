let kreski() = (
    "------------------"
);;

(*zad1*)
let rec flatten1 (xss) = (
    if xss = []
    then []
    else List.hd xss @ flatten1(List.tl xss)
);;
flatten1([[1;2;3];[4;5];[6;7]])

(*zad2*)
let rec count (x, list) = (
   if list = []
   then 0
   else if List.hd list = x
   then count(x, List.tl list) + 1
   else count(x, List.tl list)
);;
count(2, [2;2;3;3])

(*zad3*)
let rec replicate(x, n) = (
    if n <= 0
    then []
    else [x] @ replicate(x, n-1)
);;
replicate(3, 2)

(*zad4*)
let rec sqrList(xs) = (
    if xs = []
    then []
    else [List.hd xs * List.hd xs] @ sqrList(List.tl xs)
);;
sqrList([1;2;3;4])

(*zad5*)
let palindrome(xs) = (
    xs = List.rev xs
);;
palindrome([1;2;1])

(*zad6*)
let rec listLenght(xs) = (
    if xs = []
    then 0
    else listLenght(List.tl xs) + 1
);;
listLenght([1;2;2;4;3])

(*zad7*)
let rec zad7(n, c) = (
    let rec k = (fun (i) -> if i = 1 then 0 else k(i / 2) + 1) in
    c * (k(n) * k(n) + k(n)) / 2 + 1
);;
zad7(2,3)


