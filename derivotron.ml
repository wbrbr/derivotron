type expr = Const of int
          | X
          | Add of expr * expr
          | Mul of expr * expr
          | Pow of expr * int
          | Minus of expr;;


let rec differentiate e = match e with
    | Const _ -> Const 0
    | X -> Const 1
    | Add (e1,e2) -> Add ((differentiate e1),(differentiate e2))
    | Mul (e1,e2) -> Add ((Mul ((differentiate e1),e2)),(Mul (e1,(differentiate e2))))
    | Pow (e1,k) -> Mul (Const k,(Mul (differentiate e1,(Pow (e1,(k-1))))))
    | Minus e1 -> Minus (differentiate e1);;

let rec simplify e = match e with
    | Add (e1, e2) -> (match (simplify e1, simplify e2) with
                      | (Const 0, s) -> s
                      | (s, Const 0) -> s
                      | (s1, s2) -> Add (s1, s2))
    | Mul (e1, e2) -> (match (simplify e1, simplify e2) with
                      | (Const 0, s) -> Const 0
                      | (Const 1, s) -> s
                      | (s, Const 0) -> Const 0
                      | (s, Const 1) -> s
                      | (s1, s2) -> Mul (s1, s2))
    | Pow(e1, k) -> (match (simplify e1, k) with
                    | (_, 0) -> Const 1
                    | (s, 1) -> s
                    | (Const 0, _) -> Const 0
                    | (Const 1, _) -> Const 1
                    | (s, k) -> Pow (s, k))
    | Minus e1 -> (match (simplify e1) with
                  | Const i -> Const (-i)
                  | Minus s -> s
                  | s -> Minus s)
    | _ -> e;;

let rec to_string e = match e with
    | Const i -> string_of_int i
    | X -> "x"
    | Add (e1,e2) -> (to_string e1) ^ " + " ^ (to_string e2)
    | Mul (e1,e2) -> (to_string e1) ^ " * " ^ (to_string e2)
    | Pow (e1,k) -> (to_string e1) ^ "^" ^ (string_of_int k)
    | Minus e1 -> "-" ^ (to_string e1);;

let e = Add ((Minus (Pow (X,2))),(Mul ((Const 3),X))) in 
let e2 = differentiate e in
let e3 = simplify e2 in
print_string (to_string e3);;
