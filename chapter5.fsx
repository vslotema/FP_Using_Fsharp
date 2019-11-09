//5.1
let filter x list= List.foldBack (fun elem acc ->if elem = x then (elem::acc) else acc) list [];;

filter 5 [5;5;1];; 

//5.2
let fold xs = List.fold (fun acc elem -> elem::acc) [] xs;;
let revrev listofL = List.fold (fun acc xs -> (fold xs)::acc ) [] listofL;;

revrev [[1;2];[3;4;5]];;

//5.3
let sum(p,xs) = List.foldBack (fun elem acc -> if p then elem::acc else acc) xs [];;

//5.4

let downto1 f n e = List.foldBack (fun elem acc -> if n > 0 then f elem acc else e) [1..n] e;;

downto1 (fun x acc -> x*acc) 4 1;;
downto1 (fun x acc -> x::acc) 4 [];;

//5.5
(*Higher order of: areNb 
                   canBeExtBy
                   extColouring
                   countries
                   colCntrs
                    *)
type Country = string;;
type Map = (Country * Country) list;;
type Colour = Country list;;
type Colouring = Colour list;;

let exMap = [("a","b");("c","d");("d","a")];;
let colors = [["a";"c"];["b";"d"]];;

//Checks if two countries are neighbours 
let areNb m c1 c2= 
    let newL = List.filter (fun x -> x = (c1,c2) || x = (c2,c1)) m 
    List.length newL >= 1;;
 
areNb exMap "f" "a";;

//makes a unique list of countries from map
let countryList m = List.foldBack (fun (c1,c2) acc -> let list1 = if (List.contains c1 acc) then [] else [c1]
                                                      let list2 = if (List.contains c2 acc) then list1 else c2::list1
                                                      list2@acc) m [];;

countryList exMap;;


let Clist = countryList exMap;;

//gets the list from the list and checks if the element is either already there or if it is a neighbour. If so then don't add
//if not then it can be add to the list 


let newAcc m elem xs= 
    let rec new' xs'=
        match xs' with
        | [] -> [[elem]]@xs
        | y::ys -> List.map (fun x -> if (x <> elem) && not(areNb m x elem)
                                      then elem::x
                                      else []) y
    new' xs;;

newAcc exMap "a" [["c"];["b"]];;

areNb exMap "b" "c";;

let decideColor l m = 
   List.foldBack (fun elem acc -> if List.length acc = 0 
                                  then [[elem]]@acc 
                                  else let acc = 

   
   
   let list'=accListofL m acc elem 
                                  printfn "%A" elem
                                  printfn "%A" list'
                                  list'@acc) l [];;

accListofL exMap [["b";"c"]] "a";;
 
decideColor Clist exMap;;










let canBeExtBy m col c= 
    let newL = List.filter (fun x -> (areNb m x c) || x = c ) col
    if List.length newL = 0 then col@[c] else col;;
    
canBeExtBy exMap [] "a";;

let extColouring m cols c = 
    List.foldBack (fun elem acc -> let list = canBeExtBy m elem c
                                   printfn "%A" list
                                   list::acc) cols cols;;

extColouring exMap [["c"]] "d";;

let createColorMap m = 
    let countryL = List.foldBack (fun (c1,c2) acc -> let list1 = if (List.contains c1 acc) then [] else [c1]
                                                     let list2 = if (List.contains c2 acc) then list1 else c2::list1
                                                     list2@acc) m []
    List.foldBack (fun elem acc -> printfn "%A" acc
                                   printfn "%A" elem
                                   extColouring m acc elem) countryL [[]];;
 
 

createColorMap exMap;;
    List.foldBack (fun (c1,c2) acc -> let list1 = extColouring m acc c1
                                      printfn "%A" c1
                                      printfn "%A" list1 
                                      let list2 = extColouring m list1 c2 
                                      printfn "%A" list2
                                      list2@acc) m [[]];;

createColorMap exMap;;