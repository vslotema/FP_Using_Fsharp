type instruction = 
    | ADD
    | SUB
    | MULT
    | DIV
    | SIN
    | COS
    | LOG
    | EXP
    | PUSH of float;;

type Stack = float list;;

let intpInstr (s: Stack) (i:  instruction)= 
    match s,i with
    | x::y::rest, ADD -> (x+y)::rest
    | x::y::rest, SUB -> (x-y)::rest
    | x::y::rest, MULT -> (x*y)::rest
    | x::y::rest, DIV -> (x/y)::rest
    | x::rest, SIN -> (sin x)::rest
    | x::rest, COS -> (cos x)::rest
    | x::rest, LOG -> (log x)::rest
    | x::rest, EXP -> (exp x)::rest
    | stack, PUSH(x) -> x::stack;;

    
 
