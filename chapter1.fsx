//1.1
let g n = n + 4;;
g 5;;
g 6;;

//1.2
let h (x,y) = System.Math.Sqrt (x*x + y*y);;
h (2.0,4.0);;

//1.4
let f n= 
    let rec f' n acc = 
        match n with 
        | 0 -> acc
        | x -> f' (x-1) (acc+x)
    f' n 0;;

f 4;;

//1.5
let rec F n = 
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> F (n-1) + F(n-2);;


F 4;;
(*
    F(4-1) + F(4-2) // F4 = F 3 + F 2
    F(3-1) + F(2-2) // F3 = F 2 + F 1
    F(2-1) + 0     //  F2 = F 1 + 0 --> 1 + 0 = 1
    1 + 0              F1 = 1           -

*)

//1.6
let rec sum(m,n) =
    match (m,n) with
    | (m,0) -> m
    | (m,n) -> m + n + sum(m,n-1);;

sum(1,3);;
   
let sum(m,n)=
    let rec sum' m n acc = 
        match n with 
        | 0 -> acc
        | n -> sum' m (n-1) (acc+m+n)
    sum' m n m;;

