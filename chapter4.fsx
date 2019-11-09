//4.1
let upto n = [1..n];;

//4.2
let downto1 n = 
    let list = [1..n]
    List.rev list;;

//4.3
let evenN n = [0..2..n];;

//4.4
let rec altsum = function
    | [] -> 0
    | x::xs -> printfn "%A" x
               x - altsum xs;;

altsum [2;-1;3];;

//4.5
let rmod list = 
    let rec rmod' list' newList' = 
        match list' with
        | [] -> List.rev newList'
        | x::xs -> if x%2<>0 then rmod' xs (x::newList') else rmod' xs (newList')
    rmod' list [];;

//4.6
let rEven list = 
    let rec rEven' list' newList' = 
        match list' with
        | [] -> List.rev newList'
        | x::xs -> if x%2=0 then rEven' xs (x::newList') else rEven' xs (newList')
    rEven' list [];;

let list1 = upto 10;;
rEven list1;;

//4.7
let multiplicity x xs =
    let rec mult xs' occ = 
        match xs' with 
        | [] -> occ
        | y::ys -> if y=x then mult ys (occ+1) else mult ys occ
    mult xs 0;; 

let list5 = [5;5;5;5;5];;
multiplicity 5 list5;;

//4.8
let split n xs = 
   let list1, list2 = List.splitAt n xs
   (list1,list2)

//4.9
let zip(list1,list2) = 
    if List.length list1 <> List.length list2 then raise(System.ArgumentException("lists should have the same size"))
    else 
    let rec zip'(list1',list2') newList=
        match (list1',list2') with 
        | ([],[]) -> List.rev newList
        | (x::xs,y::ys) -> zip'(xs,ys) ((x,y)::newList)
    zip'(list1,list2) [];;


zip([1;2;3;4],[1;2;3;4]);;

//4.10
let prefix m n = 
    if List.length m <= List.length n 
    then
        let rec check (m',n') = 
            match (m',n') with
            | ([],_) -> true
            | (x::xs,y::ys) -> if x=y then check (xs,ys) else false
        check (m,n)
    else false;;


//4.11
//1.
let count(xs,x)=
    let rec count' xs' x' occ = 
        match xs' with 
        | [] -> occ
        | y::ys -> if y=x' then count' ys x' (occ+1) else count' ys x' occ
    count' xs x 0;;

count([1;1;1;4;5;8],1);;
//2.
let insert (xs,x) = 
    let rec insert' xs' index =
        match xs' with
        | [] -> xs@[x]
        | [y] -> if x >= y then xs@[x]
                 else 
                     let (list1,list2) = List.splitAt (index-1) xs 
                     list1@[x]@list2  
        | y::y2::ys -> if x >= y && x <= y2 
                       then 
                            let (list1,list2) = List.splitAt (index+1) xs  
                            list1@[x]@list2 
                       else insert' (y2::ys) (index+1)               
    insert' xs 0;;

//3.
let intersect ((xs:int list),(ys:int list)) = 
    let rec intrsct (xs':int list) (ys':int list) (nList:int list) = 
        match xs' with
        | [] -> List.rev nList
        | x::xs'' -> if List.contains x ys'
                     then 
                        let indX = List.findIndex (fun m -> m=x) ys'
                        let (list1,list2) = List.splitAt (indX+1) ys'
                        intrsct xs'' list2 (x::nList) 
                     else intrsct xs'' ys' nList
    intrsct xs ys [];;
     
let list1 = [1;1;1;2;2;4;4;5];;
let list2 = [1;1;2;4;5;6];;     
intersect (list1,list2);;

//4 
let plus (xs,ys) = 
    let rec plus' (xs',ys') nList = 
        match (xs',ys') with 
        | ([],[]) -> nList
        | (xs'',[]) -> let newL = List.rev nList 
                       newL@xs''
        | ([], ys'') -> let newL = List.rev nList
                        newL@ys'
        | (x::xs'',y::ys'') -> if x <= y then plus' (xs'',(y::ys'')) (x::nList)
                               else plus' ((x::xs''),ys'') (y::nList)
    plus' (xs,ys) [];;

plus ([1;2;4],[1;1;2;4;6;7;8]);;


let returnNewList l1 l2= 
    match l1 with
    | [] -> l2 
    | [x] -> l2 
    | x::xs ->  let indx = List.length l1 - 1
                let (list1,list2) = List.splitAt indx l1 
                list1@l2;;
               
let l1 = [1;2];;

let indx = List.length l1-1;;

let (list1,list2) = List.splitAt indx l1;;
//5 
let rec minus (xs,ys) =
    match (xs,ys) with  
    | (x::xs',y::ys') -> if List.contains y xs || x=y
                         then 
                         let indxY = List.findIndex (fun m -> m = y) xs
                         let (list1,list2) = List.splitAt (indxY+1) (xs)
                         let list1' = returnNewList list1 list2                       
                         minus(list1',ys')
                         else 
                         minus((xs),ys')
    | (_,_) -> xs ;;
                         
minus ([1;1;2;3],[1;1;1;2;2]);;

let (list1,list2) = List.splitAt 0 [1;1;1;2;2];;

let indxY = List.findIndex (fun m -> m = y) xs'