2 + 2 // val it : int = 4
6 / 5 // val it : int = 1
6 / -5 // val it : int = -1
true || false // val it : bool = true
true && false // val it : bool = false
not false // val it : bool = true
2 * 2 = 4 // val it : bool = true
2 * 2 <> 5 // val it : bool = true
2 = "два"
// https://msdn.microsoft.com/en-us/library/dd233230.aspx
123213123;;
333333333333333333333333333333333333333333333333333333333333333
333333333333333333333333333333333333333333333333333333333333333I
6.0 / 5. // val it : float = 1.2

42 + 3.4
let v = 42. + 3.6
1.1 + (float 2)
9. - v
max 1 2
2. + max 2.5 5.6 + min 3.4 (max 2.3 4.5)
2 - 2 - 2
// Напишите выражение, высчитывающее минимальное число среди 10, 2, 5, -1, 99
let inc x = x + 1 // val succ : int -> int
inc 1
let dec x = x - 1
let dec x = x - 1. // -.
let rec fac n = if n = 0 then 1 else n * fac (dec n)
fac 5
let square x = x * x
let squareIfLess5 x = if x < 5 then square x else x
let square (x:float) = x * x
let squareIfLess5 x = if x < 5. then square x else x
let m = "Марьванна" // val it : string = "Марьванна"
let h = "Hello"
let hm  = h + " " + m
// prfix infix
(-) 3 4
let add x y = x + y
add 3 7
2 |>add<| 2
let (++) x y = x + y
2 ++ 3
let lostNumbers = [4;8;15;16;23;42]
let cards = [3; 7; 12]
let mix = lostNumbers @ cards
List.concat [[1;2;3];[4;5]]
List.append [1;2;3] [12;3;3]
let lostNumbers = [13;666]
mix // ?
[1; "Гарри Поттер"; 16.5] // ?
[] // val it : 'a list = []
[[1; 2]; [3; 4]] // val it : int list list = [[1; 2]; [3; 4]]
[[1; 2]; [3; 4]] @ [[5; 6]] // int list list
[[]; []; []] // 'a list list
[1; [2; 3; 4]]
let fcards = 1 :: cards
[3] :: [1; 3; 4]
[3] :: [[5; 6]] // int list list
mix.[3] // !!
List.nth mix 3
13 :: [] // ?
[1;2;3;4]
1 :: 2 :: 3 :: 4 :: []
[1..4]
List.head [1..5]
[1..-2..-5]
[1..2..100]
List.head []
List.head [13]
List.tail [1..5] 
List.length [2..6] 
List.isEmpty [1..5]
List.isEmpty []
List.empty = []
let isEmpty x = if x = List.empty then true else false // 'a list -> bool when 'a : equality
let isEmpty x = x = List.empty
let isEmpty x =
  match x with
  | [] -> true
  | _ -> false
let isEmpty = function // наперёд
  | [] -> true
  | _ -> false
let rec length xs = 
  if List.isEmpty xs
  then 0
  else 1 + length (List.tail xs)
// unit suxx
length [1..5]
let rec length xs =
    match xs with
    | []    -> 0
    | y::ys -> 1 + length ys
Seq.take 4 [1..10]
[1, 2, 3, 4]
Seq.take 3 (Seq.initInfinite (fun i -> i + 1)) // наперёд
// take - самостоятельно
// получение элемента самостоятельно















let rec take n xs = 
    if n = 0 then []
    else (List.head xs) :: (take (n-1) (List.tail xs))

let rec take n xs =
    match (n, xs) with
    | 0, _ -> []
    | m, (y::ys) -> y :: take (m-1) ys
    | m, [] -> []

take 3 [1..10]

let (a,b,c) = (1,2,3)
let x :: xs = [1..5]

let decouple a = 
    match a with
    | (x,y,z) -> z
    | _ -> 1

decouple (1,2,3)
take 4 [1..10]

let rec get xs n = 
    if n = 1 then List.head xs else get (List.tail xs) (n - 1)

get [1..5] 3

List.min [1..10]
List.min ['a'..'z']

let elem what list = get list what > 0 
elem 4 [1..10] // true
elem 666 [1..10] // false
// написать elem

let elem el xs = List.exists (fun x -> el = x) xs

let rec elem el xs =
    match (el, xs) with
    | el, y::ys when y = el -> true
    | el, y::ys -> elem el ys
    | _, [] -> false

elem 666 []

List.exists (fun x -> x = 2) [1..10]

[1..2..20]
['a'..'z']
(1,2)
("Гарри Поттер", 13, 2.5)
fst (1,2)
snd (1,2)
fst (1,2,3) // ?
let (a,b,c) = (1,2,3) //(сопоставление с образцом, 3с. ниже)
b
List.length [("Гарри Поттер", 13, 2.5);("Колобок", 1, 5.5)]
sum для списка?
max для списка?









let rec sum xs =
    match xs with
    | []    -> 0
    | y::ys -> y + sum ys

sum [1..9]

(int 5.0)

let max (w::ws) =
    let rec max' xs x = 
        match xs with
        | [] -> x
        //| y::ys -> if x > y then max' ys x else max' ys y
        | y::ys when x > y -> max' ys x
        | y::ys -> max' ys y
    max' ws w

max [1;4;5;9;3]         

( Опишите функцию, которая для данного числа n
создает список из всех попарных сумм чисел от
1 до n. ( Т.е. [1+1, 1+2, 1+3, ..., 1+n, 2+1, 2+2, ...,
n+n] - всего n*n элементов) *)

let g m n = [m .. m + n] 
g 1 3
let rec f xs n = 
    match xs with
    | [] -> []
    | y::ys -> g y n @ f ys n

f [1..3] 3



let gen n = [1..n] |> List.map (fun x -> [x + 1 .. x + n]) |> List.concat
gen 3

[1;2;3] @ [5;6;7]

let sayWhat i = 
  match i with
      | 1 -> "место встречи изменить нельзя"
      | 2 -> "суббота вечер"
      | 3 -> "я ничего не знаю"
      | 4 -> "ненавижу FP, заберите меня отсюда"
sayWhat 3
sayWhat 55
let sayWhat i = 
  match i with
      | 1 -> "место встречи изменить нельзя"
      | 2 -> "суббота утро"
      | 3 -> "я ничего не знаю"
      | 4 -> "ненавижу FP, заберите меня отсюда"
      | _ -> "кто здесь?"

let sayWhat i = 
  match i with
      | 1 -> "место встречи изменить нельзя"
      | _ -> "кто здесь?"
      | 2 -> "суббота утро"
      | 3 -> "я ничего не знаю"
      | 4 -> "ненавижу FP, заберите меня отсюда"
sayWhat 4  

let opinion man = "Я, " + snd man + " считаю \"" + sayWhat (fst man) + "\""
opinion (4, "Пупкин Васёк")
let opinion (num, name) = "Я, " + name + " считаю \"" + sayWhat num + "\""    

let bmiTell weight height = 
  let c = weight / height ** 2. in
  match c with
  | a when a <= 18.5 -> "You're underweight, you emo, you!"
  | a when a <= 25.0 -> "You're supposedly normal. Pffft, I bet you're ugly"
  | a when a <= 30.0 -> "You're fat! Lose some weight, fatty!"
  | _ -> "You're a whale, congratulations!"

bmiTell 65.0 170.0

let bmiTell weight height = 
  let c = weight / height ** 2. 
  let skinny = 18.5
  let normal = 25.0
  let fat = 30.0
  match c with
    | a when a <= skinny -> "You're underweight, you emo, you!"
    | a when a <= normal -> "You're supposedly normal. Pffft, I bet you're ugly"
    | a when a <= fat -> "You're fat! Lose some weight, fatty!"
    | _ -> "You're a whale, congratulations!"

let bmiTell weight height = 
  let c = weight / height ** 2. 
  let skinny, normal, fat = 18.5, 25.0, 30.0
  match c with
    | a when a <= skinny -> "You're underweight, you emo, you!"
    | a when a <= normal -> "You're supposedly normal. Pffft, I bet you're ugly"
    | a when a <= fat -> "You're fat! Lose some weight, fatty!"
    | _ -> "You're a whale, congratulations!"

// 1) Целочисленный остаток от деления: rem'
// 2) Значение целочисленного деления: quot'
let sign = function
  | a when a > 0 -> 1
  | a when a = 0 -> 0
  | _ -> -1
(-3) / (2)
abs (-2)














let rem a b =
  let rec rem' a b = 
    match (a-b) with
    | c when c < 0 -> a
    | _ -> rem' (a-b) b
  sign a * sign b * rem' (abs a) (abs b)

let rem a b =
  let rec rem' a b = 
    match (a-b) with
    | c when c < 0 -> a
    | c -> rem' c b
  sign a * sign b * rem' (abs a) (abs b)


// Можно сократить по высоте
let rem a b =
  let rec rem' a b = match (a-b) with | c when c < 0 -> a | _ -> rem' (a-b) b;
  sign a * sign b * rem' (abs a) (abs b)












let rec quot' a b =
  if a < b then 0 else 1 + quot' (a-b) b

   
//Наибольший общий делитель: gcd
   






let rec gcd a b = 
  match (a,b) with
    | (a,b) when a = b -> a
    | (a,b) when a > b -> gcd (a-b) b
    | (a,b) when b > a -> gcd a (b-a) 

let rec gcd a b = 
  match (a,b) with
    | (a,b) when a = b -> a
    | (a,b) when a > b -> gcd (a-b) b
    | _ -> gcd a (b-a)


// hs
replicate' :: (Num i, Ord i) => i -> a -> [a]
take' :: (Num i, Ord i) => i -> [a] -> [a]
reverse' :: [a] -> [a]
repeat' :: a -> [a]
elem' :: (Eq a) => a -> [a] -> Bool
zip' :: [a] -> [b] -> [(a,b)] -- зачем он нужен?
append' :: [a] -> [a] -> [a]

// replicate i:int -> a:'a -> 'a list
// List.replicate
List.replicate 6 'a' // take

let rec replicate i a =
  match i with 
  | i when i < 1 -> []
  | _ -> a :: (replicate (i-1) a)

replicate 300000 'a'
Seq.replicate 300000000 'a'
List.replicate 300000000 'a'

let rec take n list =
  match (n,list) with
  | (_,[]) -> []
  | (i,_) when i < 1 -> []
  | (i,(x::xs)) -> x :: (take (i-1) xs)

// zip a:'a list -> b:'b list -> ('a * 'b) list List.zip
// самостоятельно
List.zip [1;2;3] ['a';'b';'c']






let rec append list1 list2 =
  match list1 with
  | [] -> list2
  | (x::xs) -> x :: (append xs list2) 

append [1..4] [5..9]



let rec zip a b =
  match (a,b) with
  | ([],_) | (_,[]) -> []
  | (x::xs, y::ys) -> (x,y) :: zip xs ys 

let rec zip a b =
  match (a,b) with
  | (x::xs, y::ys) -> (x,y) :: zip xs ys
  | _ -> [] 

Seq.zip (Seq.initInfinite (fun x->x)) (seq ["a"; "b"; "c"])
List.unzip [(1,2);(3,4)]

Seq.zip (Seq.initInfinite (fun x->x)) (seq ["a"; "b"; "c"]) 
  |> Seq.toList 
  |> List.unzip

// append a:'a list -> b:'a list -> 'a list 
// @ самостоятельно











let rec append a b =
  match a with
  | [] -> b
  | (x::xs) -> x :: (append xs b)



// factorial
let rec fac = function
  | n when n <= 0I -> 1I
  | n -> (n * fac (n-1I))

fac 300000I

// аккумулятор!

let rec fac = 
  let rec fac' acc = function
    | n when n <= 0 -> acc
    | n -> fac' (acc * n) (n-1)
  fac' 1

let rec fac = 
  let rec fac' acc n = 
    match n with
    | n when n <= 0I -> acc
    | n -> fac' (acc * n) (n-1I)
  fac' 1I

fac 300000I

List.rev [1;2;3]
// reverse  'a list -> 'a list List.rev

let reverse<'a> = 
  let rec reverse' acc = function
    | [] -> acc
    | (x::xs) -> reverse' (x::acc) xs
  reverse' []

// fib - через аккумулятор сами









let fib n =
    let rec fib' a b = function
        | n when n <= 0 -> a
        | n -> fib' b (a + b) (n - 1)
    fib' 1 1 n


let rec private fib' a b = function
  | 0 -> a
  | n -> fib' b (a+b) (n-1)

let fib = fib' 1 1


// В F# строки как строки, а не список символов
let explode (s:string) = [for c in s -> c]
explode "sfsdaf"

['a']

(*
Функция delete :: char -> char list -> char list, кото-
рая принимает на вход строку и символ и возвращает
строку, в которой удалены все вхождения символа. При-
мер: delete ’l’ "Hello world!" должно возвращать "Heo word!".
Функция substitute :: char -> char -> char list -> char list,
которая заменяет в строке указанный символ на заданный.
Пример: substitute ’e’ ’i’ "eigenvalue" возвращает
"iiginvalui" 
*)















let rec delete symbol = function
    | [] -> []
    | x :: xs when x = symbol -> delete symbol xs
    | x :: xs -> x :: (delete symbol xs) 

delete 'l' (explode "Hello!")
    
let delete symbol list =
    let rec delete' acc = function
        | [] -> acc
        | x :: xs when x = symbol -> delete' acc xs 
        // | x :: xs -> delete' (acc @ [x]) xs
        | x :: xs -> delete' (x :: acc) xs
    List.rev (delete' [] list)

let rec substitude symbol replace = function
    | [] -> []
    | x :: xs when x = symbol -> replace :: (substitude symbol replace xs)
    | x :: xs -> x :: (substitude symbol replace xs) 


let substitude symbol replace list =
    let rec substitude' acc = function
        | [] -> acc
        | x :: xs when x = symbol -> substitude' (replace :: acc) xs
        | x :: xs -> substitude' (x :: acc) xs
    List.rev (substitude' [] list)

substitude 'l' 'x' (explode "Hello")

(*

Write a recursive function which verifies the balancing of parentheses
in a string, which we represent as a List[Char] not a String. For
example, the function should return true for the following strings:
(if (zero? x) max (/ 1 x))
I told him (that it’s not (yet) done). (But he wasn’t listening)
The function should return false for the following strings:
:-)
())(
The last example shows that it’s not enough to verify that a string
contains the same number of opening and closing parentheses.
balance :: String → Bool

*)

true && false

let balance list = 
    let rec balance' list n = 
        match (list, n) with
        | ([], 0) -> true
        | ([], _) -> false
        | (_, n) when n < 0 -> false
        | (x::xs, n) when x = '(' -> balance' xs (n + 1)
        | (x::xs, n) when x = ')' -> balance' xs (n - 1)
        | (x::xs, n) -> balance' xs n
    balance' list 0      

balance (explode "(if (zero? x) max (/ 1 x))")
balance (explode "())(")

reverseAll — функция, получающая на вход
списочную структуру и обращающая все её
элементы, а также её саму.
// 'a list list -> 'a list list

reverseAll [[1;2];[3;4;5];[6]]
