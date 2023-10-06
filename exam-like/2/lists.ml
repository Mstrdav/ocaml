let rec switch = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: rest -> y :: x :: switch rest
;;

let rec unpair = function
  | [] -> []
  | (x,y) :: rest -> x :: y :: unpair rest
;;

let rec remove_succ = function 
  | [] -> []
  | [x] -> [x]
  | x :: y :: rest -> if y = x+1 then remove_succ (y :: rest) else x :: remove_succ (y :: rest)
;;

let rec combine l1 l2 = 
  match (l1, l2) with
    | ([], []) -> []
    | (x :: rest, []) -> failwith "Lengths not equal"
    | ([], x :: rest) -> failwith "Lengths not equal"
    | (x :: rest1, y :: rest2) -> (x,y) :: (combine rest1 rest2)
;;

let rec keep l1 l2 = 
  match (l1, l2) with
    | ([], []) -> []
    | (a :: r, []) -> failwith "Lengths not equal"
    | ([], a :: r) -> failwith "Lengths not equal"
    | (x :: rest1, y :: rest2) -> if y then x :: (keep rest1 rest2) else keep rest1 rest2
;;

let rec map2 f l1 l2 =
  match (l1, l2) with
    | ([], []) -> []
    | (a :: r, []) -> failwith "Lengths not equal"
    | ([], a :: r) -> failwith "Lengths not equal"
    | (x :: rest1, y :: rest2) -> f x y :: map2 f rest1 rest2
;;

let rec interleave l1 l2 =
  match (l1, l2) with
    | ([], []) -> []
    | ([], _) -> interleave l2 l1
    | (x :: rest, _) -> x :: interleave l2 rest
;; 