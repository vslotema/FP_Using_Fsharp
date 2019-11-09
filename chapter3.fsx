//3.1 triple
let (%!) a b= a % b;;

let rec timeDay (h,m,f)=
    match (h,m,f) with
    | (h,m,f) when m >= 60 -> 
                        let min = (%!) m 60
                        let hours = m/60
                        timeDay(h+hours,min,f)
    | (h,m,f) when h >= 24 -> 
                        let hours = (%!) h 24
                        if hours < 12 then timeDay(hours,m,"AM") else timeDay((%!) hours 12,m,"PM")
    | (h,m,f) -> if h < 12 then (h,m,"AM") else ((%!) h 12,m,"PM");;                    

(%!) 18 12;;

timeDay(15,77,"PM");;

let time1 = timeDay(64,33,"PM");;
let time2 = timeDay(15,19,"AM");;

let timeFirst a b = 
    let t1 = europeanTime a
    let t2 = europeanTime b
    if (t1-t2) < 0 then a else b;;
   
let europeanTime x = 
    match x with
    | (h,m,f) -> if f = "PM" then (h+12)*100+m else h*100+m;;

timeFirst time1 time2;;

//3.1 record
type timeOfDay = {hour: int; min: int; f:string };;

let time1' = {hour = 6; min = 30; f="AM"};;
let time2' = {hour = 6; min = 23; f = "AM"};;

let timeFirst' a b = 
    if a.f = "AM" && b.f = "PM" then a 
    else if b.f = "AM" && a.f = "PM" then b
    else if a.hour > b.hour then b
    else if b.hour > a.hour then a 
    else if a.min < b.min then a 
    else b;;

timeFirst' time1' time2';;

//3.2 

let rec normalizer = function
    | (p,sh,pen) when pen < 0 -> normalizer(p, sh-1,pen+12)
    | (p,sh,pen) when sh < 0 -> normalizer(p-1,sh+20,pen)
    | (p,sh,pen) when pen >= 12 -> normalizer(p,sh + pen/12,pen%12)
    | (p,sh,pen) when sh >= 20 -> normalizer(p + sh/20, sh%20,pen)
    | (p,sh,pen) ->(p,sh,pen);;

let addOrSub c1 c2 op = 
    match op with
    | "+" -> let (p,sh,pen) = c1
             let (p',sh',pen') = c2
             normalizer(p+p',sh+sh',pen+pen')
    | "-" -> let (p,sh,pen) = c1
             let (p',sh',pen') = c2
             normalizer(p-p',sh-sh',pen-pen') ;;


let amount1 = (12,11,14);;
let amount2 = (11,10,23);;

addOrSub amount1 amount2 "-";;

//3.2 Record
type currency = {p : int; sh : int; pen : int};;

let addOrSub' c1 c2 op = 
    match op with
    | "+" -> let (p',sh',pen') = normalizer(c1.p+c2.p, c1.sh+c2.sh, c1.pen+c2.pen)
             {p=p';sh=sh';pen=pen'}
    | "-" -> let (p',sh',pen') = normalizer(c1.p-c2.p, c1.sh-c2.sh, c1.pen-c2.pen)
             {p=p';sh=sh';pen=pen'};;

let cur1 = {p=12;sh=11;pen=14};;
let cur2 = {p=11;sh=20;pen=23};;

addOrSub' cur1 cur2 "-";;