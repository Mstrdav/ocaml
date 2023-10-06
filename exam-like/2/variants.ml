type bool3 = BTrue | BFalse | Unknown

let and3 a b = 
  match (a, b) with
    | (BFalse, _) -> BFalse
    | (_, BFalse) -> BFalse
    | (Unknown, _) -> Unknown
    | (_, Unknown) -> Unknown
    | _ -> BTrue
;;

let not3 a = 
  match a with
    | BTrue -> BFalse
    | BFalse -> BTrue
    | Unknown -> Unknown
;;

type instruction = Plus of int | Mul of int ;;

let rec apply_instructions value l =
  match l with
    | [] -> value
    | Plus x :: rest -> apply_instruction (value + x) rest
    | Mul x :: rest -> apply_instruction (value * x) rest
;;

let rec to_funlist l =
  match l with
    | [] -> []
    | Plus x :: rest -> (fun a -> a + x) :: to_funlist rest
    | Mul x :: rest -> (fun a -> a * x) :: to_funlist rest
;;

let rec to_fun l =
  match l with
    | [] -> (fun a -> a)
    | Plus x :: rest -> (fun a -> to_fun rest (a + x))
    | Mul x :: rest -> (fun a -> to_fun rest (a * x))
;;

let rec compact l =
  match l with
    | [] -> []
    | [x] -> [x]
    | Plus x :: Plus y :: rest -> compact (Plus (x+y) :: rest)
    | Mul x :: Mul y :: rest  -> compact (Mul (x*y) :: rest)
    | op :: rest -> op :: compact rest 
;;

let rec to_string start l =
  match l with
    | [] -> start
    | Plus x :: rest -> to_string (start ^ " + " ^ (string_of_int x)) rest
    | Mul x :: rest -> to_string ("(" ^ start ^ ") * " ^ (string_of_int x)) rest
;;

type 'a element = Single of 'a | Pair of 'a * 'a ;;
type 'a fonct = One_arg of ('a -> 'a) | Two_args of ('a -> 'a -> 'a) ;;

let rec count_elements l =
  match l with
    | [] -> 0
    | Single _ :: rest -> 1 + count_elements rest
    | Pair _ :: rest -> 2 + count_elements rest
;;

let rec apply_list fun_l arg_l =
  match (fun_l, arg_l) with
    | ([], []) -> []
    | (One_arg f :: rest_f, Single x :: rest_x) -> (f x) :: apply_list rest_f rest_x
    | (Two_args f :: rest_f, Pair (x, y) :: rest_x) -> (f x y) :: apply_list rest_f rest_x
    | _ -> failwith "Invalid arguments"
;;

let rec partial_apply x fun_l =
  match fun_l with
    | [] -> []
    | One_arg f :: rest -> f :: partial_apply x rest
    | Two_args f :: rest -> f x :: partial_apply x rest
;;