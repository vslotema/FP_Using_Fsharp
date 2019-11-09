//2.1
let f n = n%2=0;;

f 2;;
f 24;;
f 27;;
f 29;;
f 30;;

//2.2
let pow(s,n) =
    let rec pow' s n acc = 
        match n with
        | 0 -> acc
        | n -> pow' s (n-1) (acc+s)
    pow' s n "";;

pow("hello",5);;

//2.3
let isIthChar(str,i,ch)=
    let isIthChar' (str:string) (i:int) (ch:char) =
        if ch=str.Chars(i) then true else false
    isIthChar' str i ch;;

isIthChar ("Hello",3,'l');;

//2.4
let occFromIth(str,i,ch)=
    let size = String.length str
    if i >= size then 0 else
        let sub = str.[i..]
        let n = (String.length sub) - 1
        let rec occ (str':string) (ch':char) (n':int) (acc:int) = 
            match n' with
            | 0 -> acc
            | n -> if ch = str'.Chars(n) then occ str' ch' (n-1) (acc+1) else occ str' ch' (n-1) acc
        occ sub ch n 0;;
 
occFromIth ("Helllo",2,'e');;
let sub (s:string) (i:int) = s.[i..];;
sub "helllo" 2;;
//2.5
let occInString(str,ch) = 
    let l = (String.length str) - 1
    let rec occum str' ch' length' acc = 
        match length' with 
        | 0 -> acc
        | n -> if ch = str.Chars(n) then occum str' ch' (length'-1) (acc+1) else occum str' ch' (length'-1) (acc)
    occum str ch l 0;;

//2.6
let notDivisible(d,n) = n%d<>0;;
notDivisible(7,9);;

//2.7.1
let test(a,b,c)=notDivisible(a,c) && notDivisible(a+1,c) && notDivisible(b,c);;      
test(3,2,7);;

//2.7.2
let prime (n:int) =
    let numbers = [1..n]
    let rec isPrime (n:int) (numbers':int list) (acc:int list)=
        match numbers' with
        | [] -> if acc.Length = 2 && acc.Item(1) = 1 && acc.Item(0) = n then true else false 
        | x::xs -> if n%x=0 then isPrime n xs (x::acc) else isPrime n xs acc
    isPrime n numbers [];;

prime 11;;
//2.7.3
let nextPrime n = 
    let l = [n+1..n+20]
    let rec findNext l' = 
        match l' with
        | [] -> findNext [n+20..n+50]
        | x::xs -> if prime x then x else findNext xs
    findNext l;;

nextPrime 11;;

//2.8
let rec pascal(n,k) = 
  if k = 0 || k = n then 1 
  else 
     pascal (n-1,k-1) + pascal (n-1,k);;
 
 //2.9
 (*
     1. The type is int*int -> int
     2. (x,y)
     3. f(2,3) -> f(2-1,2*3) -> f(1,6)
        f(1,6) -> f(1-1,1*6) -> f(0,6)
        f(0,6) -> 6 
     4. Factorial 
 *)
let rec f = function
    | (0,y) -> y
    | (x,y) -> f(x-1,x*y);;

f(2,3);;

//2.10
(*
    1. bool*int -> int
    2. It will result in stackoverflow since we already try to calculate the factorial function without testing
       it to be true or not.
    3. It will go straight to 0 since it will always be false.
*)
let test(c,e)= if c then e else 0;;

//2.13
let curry f = f (g,h);;
let uncurry g (f,h) = g f h;;

   