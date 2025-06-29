(* Évaluation d'expressions *)
5 + 3 * 2;;
4.0 /. 2.0 +. 1.5;;
true && (5 > 3);;
not (3 = 3);;
"Hello" ^ " " ^ "World";;
'A';;
int_of_char 'A';;

let x = 2 in x * x;;
let y = 3.5 in y +. 2.5;;
let a = 10 in let b = 20 in a + b;;
let p = true in let q = false in p || q;;
let str1 = "OCaml" in let str2 = " is fun!" in str1 ^ str2;;

let x = 2 in let y = x * 3 in y + 4;;     
let a = 5 in let b = 6 in let c = a + b in c * 2;;
3 + let x = 4 in let y = 5 in x * y;;
(let x = 3 in x + 1) * (let y = 2 in y + 2);;
let greeting = "Hello" in let name = "OCaml" in greeting ^ ", " ^ name;;

let x = 7 in let y = 2 * x in let z = y + 3 in z / x;;
let a = 4.5 in let b = 3.5 in let c = a +. b in c *. 2.0;;
let flag = true in let result = if flag then 1 else 0 in result + 5;;
let str = "Functional" in let num = 101 in str ^ " programming " ^ string_of_int num;;
let char1 = 'O' in let char2 = 'C' in int_of_char char1 + int_of_char char2;;

(* Priorités *)
let f x = (x * x * x) in ((f 4) + (f 2));;
let f x = (2 * x) in (let g x = (3 * x) in (f (g 2)));;
let m a b = if (a > b) then a else b in (m (5 + 2) (3 * 2));;
let a x = if x >= 0 then x else (-x) in (a (-5) + (abs 3));;

(fun x -> x + 1) (if 3 > 2 then 4 else 5);;
let f = (fun x -> x + 1) in (let g = (fun x -> 2 * x) in (g (f 3)));;
3 + ((fun x y -> if x > y then x else y) (5 - 3) (3 + 1));;
let f = (fun x -> if (x mod 2 = 0) then (x / 2) else (3 * x + 1)) in (f 7);;

(* Types *)
fun x -> x + 1;;
fun x y -> x * y;;
fun x -> if x then "true" else "false";;
fun x y -> if x > y then x else y;;

let f1 x y = x + y in let f2 = f1 3 in f2 5;;
let compose f g x = f (g x) in compose (fun x -> x + 1) (fun x -> 2 * x) 3;;

(fun f x -> f (f x));;
(fun f g x -> if x mod 2 = 0 then f x else g x);;
(fun f g x -> if f x then g x else x);;
(fun f n x -> if n then f x else f (f x));;

(* Typage et curryfication *)
let f = fun x -> fun y -> fun z -> x + y * z;;
let f3 = f 3;;
let f32 = f3 2;;
let f321 = f32 1;;

(* Typage avancé *)
let z f x = x;;

let s = fun n -> fun f -> fun x -> f (n f x);;

let to_int p = p (fun x -> x + 1) 0;;

let a = fun cm -> fun cn -> fun f -> fun x -> cm f (cn f x);;
let m = fun cm -> fun cn -> fun f -> cm (cn f);;
