(*
  in  : int p, int q, strictly positive
  out : bool - true if p is divisible by q; false otherwise
 *)
let divide p q =
  p mod q = 0

(*
  in  : int a, positive 
  out : bool - true if a is even; false otherwise
 *)
let even_number a =
  divide a 2

(*
  in  : int a, positive 
  out : bool - true if a is odd; false otherwise
 *)
let odd_number a =
  not (even_number a)

(*
  in  : int a, int b strictly positives
  out : int - greatest common divisor of a and b
 *)
let gcd p q =
  if p=q then
    p
  else
    let max = max p q in
    let divider = ref (max/2) in
    let isgcd = ref false in
    while (not !isgcd) && (!divider <> 1) do
      if (divide p !divider) && (divide q !divider) then
        isgcd := true
      else
        divider := !divider - 1
    done;
    !divider

(*
  in  : (int p, int q) a rational number p/q
  out : (int, int) irreducible fraction equal to the input
 *)
let irreducible_fraction (p,q) =
  let divider = gcd p q in
  let pbis = p/divider in
  let qbis = q/divider in
  (pbis, qbis)

(*
  in  : (int p1, int q1) and (int p2, int q2)
  out : (int, int) the sum of the two rational numbers given in the input
 *)
let add_rat (p1, q1) (p2, q2) =
  irreducible_fraction (p1*q2+p2*q1, q1*q2)

(*
  in  : (int p1, int q1) and (int p2, int q2)
  out : (int, int) the difference of the two rational numbers given in the input
 *)
let sub_rat (p1, q1) (p2, q2) =
  irreducible_fraction (p1*q2-p2*q1, q1*q2)

(*
  in  : (int p1, int q1) and (int p2, int q2)
  out : (int, int) the product of the two rational numbers given in the input
 *)
let mul_rat (p1, q1) (p2, q2) =
  irreducible_fraction (p1*p2, q1*q2)

(*
  in  : (int p1, int q1) and (int p2, int q2)
  out : (int, int) the quotient of the two rational numbers given in the input
 *)
let div_rat (p1, q1) (p2, q2) =
  irreducible_fraction (p1*q2, q1*p2)

(*
  in  : int n positive
  out : approximation of pi via arithmetic quadrature
        using rational numbers
 *)
let approx_pi n =
  if n<= 0 then
    float_of_int n
  else
    let sum = ref (1,1) in
    for i=2 to n do
      if (i mod 2) = 0 then          
        sum := sub_rat !sum (1,2*i-1)
      else
        sum := add_rat !sum (1,2*i-1)
    done;
    let (p,q) = !sum in
    4.0 *. (float_of_int p) /. (float_of_int q)

(*
  in  : int n positive
  out : approximation of pi via arithmetic quadrature
        using real numbers (float)
 *)
let approx_pi_f n =
  if n<= 0 then
    float_of_int n
  else
    let sum = ref 1.0 in
    for i=2 to n do
      if (i mod 2) = 0 then          
        sum := !sum -. 1.0 /. (float_of_int (2*i-1))
      else
        sum := !sum +. 1.0 /. (float_of_int (2*i-1)) 
    done;
    4.0 *. !sum

(*
  in  : int n, positive
  out : int, n!
 *)
let factorial n =
  if n = 0 then
    1
  else
    let product = ref 1 in
    for i=1 to n do
      product := !product * i
    done;
    !product

(*
  in  : int n, positive
  out : approximation of e via Taylor series
        using real numbers (float)
 *)
let approx_e n =
  let sum = ref 0.0 in
  for i=0 to n do      
    sum := !sum +. (1.0 /. float (factorial i))
  done;
  !sum

(*
  u_{n+1} = 1 + 1/u_n, u_0=1
  in  : int n, positive
  out : approximation of phi with u_n
 *)
let approx_phi n =
  let phi = ref 1.0 in
  for i=1 to n do
    phi := 1.0 +. 1. /. !phi
  done;
  !phi

(*
  u_{n+1} = 1 + 1/u_n, u_0=1
  in  : -
  out : approximation of phi with u_{n+1} = u_n
 *)
let approx_phi2 () =
  let phi = ref 1.0 in
  let phi_next = ref 2.0 in
  while phi <> phi_next do
    phi := !phi_next;
    phi_next := 1.0 +. 1.0 /. !phi_next
  done;
  !phi_next

(* Main expression - Tests *)
let () =
  Printf.printf "Numerical approximations.\n";
  let res1 = divide 40 10 in 
  let res2 = divide 30 4 in
  Printf.printf "Divide : 40 by 10 %B - 30 by 4 %B\n" res1 res2;
  let pgcd = gcd 72 30 in
  Printf.printf "PGCD 72 30 : %d\n" pgcd;
  let parite1 = even_number(9) in
  let parite2 = odd_number(9) in
  Printf.printf "Parity 9 : pair %B - impair %B\n" parite1 parite2;
  let (p,q) = irreducible_fraction (30,72) in
  Printf.printf "Irreducible fraction 30/72 %d, %d\n" p q;
  let (p,q) = add_rat (30, 72) (9, 12) in
  Printf.printf "Addition of two rational numbers 30/72+9/12%d, %d\n" p q;
  let (p,q) = sub_rat (30, 72) (9, 12) in
  Printf.printf "Subtraction of two rational numbers 30/72-9/12 %d, %d\n" p q;
  let (p,q) = mul_rat (30, 72) (9, 12) in
  Printf.printf "Multiplication of two rational numbers 30/72x9/12 %d, %d\n" p q;
  let (p,q) = div_rat (30, 72) (9, 12) in
  Printf.printf "Division of two rational numbers 30/72 / 9/12 %d, %d\n" p q;
  let approx1 = approx_pi 10 in
  Printf.printf "Approximation pi rat : %1.10f\n" (Float.pi -. approx1);
  let approx2 = approx_pi_f 10000000 in
  Printf.printf "Approximation pi float : %1.10f\n" (Float.pi -. approx2);
  let res = factorial 5 in
  Printf.printf "Factoriel 5 : %d\n" res;
  let res2 = factorial 20 in
  Printf.printf "Factoriel 20 : %d\n" res2;
  let res2 = factorial 21 in
  Printf.printf "Factoriel 21 : %d\n" res2;
  let approx3 = approx_e 10 in
  Printf.printf "Approximation e : %1.10f\n" approx3;
  Printf.printf "Approximation e diff : %1.10f\n" (Float.exp(1.) -. approx3);
  let approx4 = approx_phi 38 in
  Printf.printf "Approximation phi 38 : %1.20f\n" approx4;
  let approx5 = approx_phi 39 in
  Printf.printf "Approximation phi 39 : %1.20f\n" approx5;
  let approx6 = approx_phi2 () in
  Printf.printf "Approximation phi equality : %1.20f\n" approx6





