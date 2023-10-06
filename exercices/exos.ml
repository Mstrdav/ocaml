(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

(* Affichage *)
Printf.printf "Parties 1 & 2\n----------------\n\n";;
let log = Printf.printf ;;

(* Fonctions *)


(* div_pair (x,y) divides x by y or returns 0 if y is 0 *)
let div_pair ( (x), (y) ) =
  if y = 0 then 0 else x / y
;;

log "div_pair (4,2) = %d\n" (div_pair (4,2)) ;;

let addp ( (x), (y) ) =
  x + y
;;

log "addp (4,2) = %d\n" (addp (4,2)) ;;

let choose ( (a), (b), (c) ) =
  if a < (addp (b, c)) then true else false
;;

log "choose (4,2,3) = %b\n" (choose (4,2,3)) ;;

let check = function
  | (0,0) -> true
  | (x,y) -> x < y
;;

log "check (4,2) = %b\n" (check (4,2)) ;;

let confused1 = function
  | true -> 1
  | flase -> 2
;;

log "confused1 true = %d\n" (confused1 true) ;;

let divp (x:int) (y:int) =
  if y = 0 then 0 else x / y
;;

log "divp 4 2 = %d\n" (divp 4 2) ;;

let tup_add = fun (a,b) (c,d) -> (a+c, b+d) ;;

let (x, y) = tup_add (4,2) (3,1) in
log "tup_add (4,2) (3,1) = (%d, %d)\n" x y ;;

let cchoose a b c =
  if a < (addp (b, c)) then true else false
;;

log "cchoose 4 2 3 = %b\n" (cchoose 4 2 3) ;;

let pmul(a, b) =
  ((a+b), (a*b))
;;

let (x, y) = pmul (4,2) in
log "pmul (4,2) = (%d, %d)\n" x y ;;

let cpmul a b =
  ((a+b), (a*b))
;;

let (x, y) = cpmul 4 2 in
log "cpmul 4 2 = (%d, %d)\n" x y ;;

let mul a b =
  a * b
;;

log "mul 4 2 = %d\n" (mul 4 2) ;;

let mul2, mul3 = (mul 2, mul 3);;

log "mul2 4 = %d\n" (mul2 4) ;;
log "mul3 4 = %d\n" (mul3 4) ;;

let mul_list = [mul2 ; mul3];;

let lambda_list = [ (fun x -> x * 2) ; (fun x -> x * 3) ];;

let f = fun (x,y,z) -> x-y*z;;

let mk_pair a b = (a,b);;
let mk_list a b = [a;b];;

let f x = if x < 0 then None else Some x;;

(* sample lists to test *)
let l1 = [1;2;3;4] ;;
let l2 = [(1,true) ; (2, false)] ;;

let tts_db (a,b) = Printf.sprintf "(%d,%b)" a b ;;
let tts_bd (b,a) = Printf.sprintf "(%b,%d)" b a ;;
let tts_dd (a,b) = Printf.sprintf "(%d,%d)" a b ;;

log "\nListes :\n" ;;
log "l1 = " ;;
let () = List.iter (log "%d ") l1 ;; log "\n" ;;
log "l2 = " ;;
let () = List.iter (log "%s ") (List.map tts_db l2) ;; log "\n" ;;

let rec length = function
  | [] -> 0
  | x :: rest -> 1 + length rest
;;

log "\nOperations sur les listes :\n" ;;

log "length l1 = %d\n" (length l1) ;;
log "length l2 = %d\n" (length l2) ;;

let rec incr = function
  | [] -> []
  | x :: rest -> x + 1 :: incr rest
;;

log "incr l1 = " ;;
let () = List.iter (log "%d ") (incr l1) ;; log "\n" ;;

let rec sum = function
  | [] -> 0
  | x :: rest -> x + sum rest
;;

log "sum l1 = %d\n" (sum l1) ;;

let rec acc_sum acu = function
  | [] -> acu
  | x :: rest -> x + acc_sum acu rest
;;

log "acc_sum 0 l1 = %d\n" (acc_sum 0 l1) ;;

let rec perms = function
  | [] -> []
  | (a,b) :: rest -> (b,a) :: perms rest
;;

log "perms l2 = " ;;
let () = List.iter (log "%s ") (List.map tts_bd (perms l2)) ;; log "\n" ;;

let rec acc_perms acu = function
  | [] -> acu
  | (a,b) :: rest -> (b,a) :: acc_perms acu rest
;;

log "acc_perms [] l2 = " ;;
let () = List.iter (log "%s ") (List.map tts_bd (acc_perms [] l2)) ;; log "\n" ;;

let rec mk_list max = function
  | [] -> []
  | x :: rest -> if max = 0 then x :: rest else  mk_list (max - 1) rest
;;

log "mk_list 2 l1 = " ;;
let () = List.iter (log "%d ") (mk_list 2 l1) ;; log "\n" ;;

let fsum f = f 0 + f 1 + f 2 + f 3 ;;

let rec fsumlist f = function
  | [] -> 0 
  | x :: rest -> f x + fsumlist f rest
;;

log "fsumlist mul2 l1 = %d\n" (fsumlist mul2 l1) ;;

let rec acc_fsumlist f acu = function 
  | [] -> acu
  | x :: rest -> acc_fsumlist f (acu + (f x)) rest
;;

log "acc_fsumlist mul3 0 l1 = %d\n" (acc_fsumlist mul3 0 l1) ;;

let rec map f = function
  | [] -> []
  | x :: rest -> f x :: map f rest
;;
let foi = float_of_int ;;

log "map foi l1 = " ;;
let () = List.iter (log "%f ") (map foi l1) ;; log "\n" ;;

let rec find a = function
  | [] -> None
  | x :: rest -> if x = a then Some x else find a rest
;;

log "find 2 l1 = " ;;
let () = match find 2 l1 with
  | None -> log "None\n"
  | Some x -> log "Some %d\n" x

let omap f = function
  | None -> None
  | Some a -> Some (f a)
;;

log "omap mul2 (find 2 l1) = " ;;
let () = match omap mul2 (find 2 l1) with
  | None -> log "None\n"
  | Some x -> log "Some %d\n" x
;;

let rec filter f = function
  | [] -> []
  | x :: rest -> if f x then x :: filter f rest else filter f rest
;;

log "\nPartie 3\n" ;;
log "----------------\n" ;;

log "\n[Pattern matching]\n" ;;

let pm_int = function
  | 0 -> "zero"
  | 1 -> "one"
  | 2 -> "two"
  | _ -> "many"
;;

log "pm_int 0 = %s\n" (pm_int 2) ;;

let pm_bool = function
  | true -> "true"
  | false -> "false"
;;

log "pm_bool true = %s\n" (pm_bool true) ;;

let pm_string = function
  | "" -> "empty"
  | "ping" -> "pong"
  | "foo" -> "bar"
  | s -> s
;;

log "pm_string \"ping\" = %s\n" (pm_string "ping") ;;

let pm_tuple = function
  | (0, true) -> 1
  | (0, false) -> -1
  | (_, false) -> 2
  | (_, true) -> 3
;;

log "pm_tuple (0, true) = %d\n" (pm_tuple (0, true)) ;;

let confused2 (a,b) = function
   | (0, true) -> 1
   | (n ,_) -> 10
;;

log "confused2 (3,4) (1, true) = %d\n" (confused2 (3,4) (1, true)) ;;

let calc = function
  | (x, y, "add") -> x + y
  | (x, y, "sub") -> x - y
  | (x, y, "mul") -> x * y
  | (x, 0, "div") -> failwith "Division by zero."
  | (x, y, "div") -> x / y
  | _ -> failwith "Unknown operator."
;;

log "calc (4,2,\"mul\") = %d\n" (calc (4,2,"mul")) ;;

let ccalc (x,y,z) =
  match z with
  | "add" -> x + y
  | "sub" -> x - y
  | "mul" -> x * y
  | "div" -> if y = 0 then failwith "Division by zero." else x / y
  | _ -> failwith "Unknown operator."
;;

log "ccalc (4,2,\"mul\") = %d\n" (ccalc (4,2,"mul")) ;;

let xor = function
  | (true, true) -> false
  | (false, false) -> false
  | _ -> true
;;

log "xor (true, true) = %b\n" (xor (true, true)) ;;

log "\n[Sequences]\n" ;;

let seq_ex x =
  Printf.printf "Calling f with value x = %d\n" x ;
  x * x + 2
;;

let seq = seq_ex 2 ;;
log "result = %d\n" seq ;;

let show f v =
  Printf.printf "Calling f with value v = %d\n" v ;
  f v
;;

let show_ex = show mul2 2 ;;
log "result = %d\n" show_ex ;;

let pshow cv f v =
  Printf.printf "Calling f with value v = %s\n" (cv v) ;
  f v
;;

let pshow_ex = pshow string_of_int mul2 2 ;;
log "result = %d\n" pshow_ex ;;

log "\n[Inner let]\n" ;;

let is_leap_year n =

  (* Test if n is a multiple of x. *)
  let multiple_of x = n mod x = 0 in

  let is_mul4 = multiple_of 4 in
  let is_mul100 = multiple_of 100 in
  let is_mul400 = multiple_of 400 in

  is_mul400 || (is_mul4 && not is_mul100)
;;

log "is_leap_year 2016 = %b\n" (is_leap_year 2016) ;;
log "is_leap_year 2200 = %b\n" (is_leap_year 2200) ;;

let get_triple arg =
  match arg with
  | (a, p) ->
    begin match p with
          | (b,c) -> [ a ; b ; c ]
    end
  ;;
;;

(* Simpler get_triple *)
let simple_get_triple = fun (a, (b,c)) -> [ a ; b ; c ] ;;

log "\n[Expressions and definitions]\n" ;;

let fancy1 x =
  begin
     if x = 0 then
        (fun a b -> a + b)
     else
        (fun a b -> a - b)
  end
  (x-1) (x+1)
;;

let fancy2 a b c =
  (* Remember concatenation? *)
  "Hello " ^ 

  let f =
     if a then
        match b with
        | 0 -> (fun x -> x - c )
        | _ -> (function 4 -> 2 | _ -> 1)
     else
        (fun x -> x + c)
  in
  (* Use f here *)
  string_of_int (f 2)
;;

(* Partie 4 *)

log "\nPartie 4\n" ;;
log "----------------\n" ;;

log "\n[Records]\n" ;;

(* This defines a record type. *)
type coordinates =
   { long: float ;
     lat:  float }
;;

(* This defines only an alias. *)
type path = coordinates list ;;

(* Another record *)
type region =
   { region_name: string ;
     borders: path ;
     has_coastline: bool }
;;

(** Functions are first-class, they can appear in records. **)
(* type test =
   { (* A function which should be tested. *)
     fonc: (int -> int) ;

     (* An argument, which will be given to the function. *) 
     arg: int ;

     (* The expected result. *)
     expect: int }
;; *)

(* A parameterized record type. (see later) *)
type ('a, 'b) test =
   { fonc: ('a -> 'b) ;
     arg: 'a ;
     expect: 'b }
;;

let point1 = { long = -0.3 ;
               lat  = 42.5 }
;;
               
let point2 = { point1 with long = -1.0 } ;;

let get_lat c = c.lat ;;

let get_all { long ; lat } = (long, lat) ;;

let is_good = function
   | { region_name = "" } | { borders = [] } -> false
   | _ -> true
;;

let apply test =
   test.fonc test.arg = test.expect 
;;

let test1 = { fonc = mul2 ; arg = 2 ; expect = 4 } ;;
let test2 = { fonc = mul2 ; arg = 3 ; expect = 4 } ;;

log "apply test1 = %b\n" (apply test1) ;;
log "apply test2 = %b\n" (apply test2) ;;

log "\n[Parameterized types]\n" ;;

(* new test with other types *)

let int_test = { fonc = mul2 ; arg = 2 ; expect = 4 } ;;
let bool_test = { fonc = (fun x -> if x then 1 else 0) ; arg = true ; expect = 1 } ;;
let string_test = { fonc = String.length ; arg = "foo" ; expect = 3 } ;;
let float_test = { fonc = (fun x -> int_of_float (x *. 2.0)) ; arg = 2.0 ; expect = 4 } ;;

log "apply int_test = %b\n" (apply int_test) ;;
log "apply bool_test = %b\n" (apply bool_test) ;;
log "apply string_test = %b\n" (apply string_test) ;;
log "apply float_test = %b\n" (apply float_test) ;;

(* test with no int at all *)

let float_float_test = { fonc = (fun x -> x *. 2.0) ; arg = 2.0 ; expect = 4.0 } ;;

log "apply float_float_test = %b\n" (apply float_float_test) ;;

log "\n[Arrays]\n" ;;

let array1 = [| 1 ; 2 ; 3 |] ;; (* Is this an array ?? *)

let array2 = Array.make 3 0 ;; (* Is this an array ?? *)

(* set a value *)
array2.(0) <- 1 ;;
Array.set array2 1 2 ;;

(* get a value *)
let x = array2.(0) ;;
let y = Array.get array2 1 ;;

(* get the length *)
let l = Array.length array2 ;;

log "array2.(0) = %d\n" x ;;
log "Array.get array2 1 = %d\n" y ;;
log "Array.length array2 = %d\n" l ;;

log "\n[Mutable Records]\n" ;;

type player =
  { name: string ;
    age: int ;
    mutable points: int }
;;

let show_player = function
  | { name ; age ; points } ->
    Printf.printf "%s (%d) has %d points.\n" name age points
;;

let new_player name age =
  { name ; age ; points = 0 }
;;

let add_points p n =
  log "Adding %d points to %s\n" n p.name ;
  p.points <- p.points + n
;;

let p1 = new_player "John" 20 ;;
let p2 = new_player "Jane" 21 ;;

show_player p1 ;;
show_player p2 ;;

add_points p1 10 ;;
add_points p2 20 ;;

show_player p1 ;;
show_player p2 ;;

(* immutable *)

type iplayer =
  { iname: string ;
    iage: int ;
    ipoints: int }
;;

let ishow_player = function
  | { iname ; iage ; ipoints } ->
    Printf.printf "%s (%d) has %d points.\n" iname iage ipoints
;;

let inew_player name age =
  { iname = name ; iage = age ; ipoints = 0 }
;;

let iadd_points p n =
  log "Adding %d points to %s\n" n p.iname ;
  { p with ipoints = p.ipoints + n }
;;

let ip1 = inew_player "John" 20 ;;
let ip2 = inew_player "Jane" 21 ;;

ishow_player ip1 ;;
ishow_player ip2 ;;

let ip1 = iadd_points ip1 10 ;;
let ip2 = iadd_points ip2 20 ;;

ishow_player ip1 ;;
ishow_player ip2 ;;

(* Sets and Maps look immutables, whereas Hastbl and Queue are mutables *)

(* references *)
let ref_test = 
  (* create a ref with val 0 and increment it twice *)
  let r = ref 0 in
  r := !r + 1 ;
  r := !r + 1 ;
  !r
;;

type color = Red | Blue ;;
type role = Player of color * int | Referee ;;

let get_number = function
  | Player (_, n) -> n
  | Referee -> 0
;;

type people =
  { name: string ;
  role: role ; 
  age: int }
;;

let same_team = function (p1, p2) ->
  match p1.role, p2.role with
  | Player (c1, _), Player (c2, _) -> c1 = c2
  | _ -> false
;;

let is_number = function (p, n) ->
  match p.role with
  | Player (_, m) -> m = n
  | Referee -> n = 0
;;

let people1 = { name = "John" ; role = Player (Red, 1) ; age = 20 } ;;
let people2 = { name = "Jane" ; role = Player (Blue, 2) ; age = 21 } ;;
let people3 = { name = "Jack" ; role = Referee ; age = 30 } ;;

log "same_team (people1, people2) = %b\n" (same_team (people1, people2)) ;;
log "same_team (people1, people3) = %b\n" (same_team (people1, people3)) ;;
log "is_number (people1, 1) = %b\n" (is_number (people1, 1)) ;;
log "is_number (people1, 2) = %b\n" (is_number (people1, 2)) ;;
log "is_number (people3, 0) = %b\n" (is_number (people3, 0)) ;;

log "\n[Parameterized and recursive variants]\n" ;;

type 'a mylist = Empty | Cell of 'a * 'a mylist ;;

let myhd = function
  | Empty -> failwith "Empty list"
  | Cell (x, _) -> x
;;

let rec mylength = function
  | Empty -> 0
  | Cell (_, rest) -> 1 + mylength rest
;;

let mylength_tail l =
  let rec loop acu = function l ->
    match l with
  | Cell (_, rest) -> loop (acu + 1) rest
  | Empty -> acu
  in
  loop 0 l
;;

let ohd = function
  | [] -> None
  | x :: _ -> Some x
;;

let otl = function
  | [] -> None
  | _ :: rest -> Some rest
;;

log "myhd (Cell (1, Cell (2, Empty))) = %d\n" (myhd (Cell (1, Cell (2, Empty)))) ;;

log "\n[Exercices on list]\n"

(* keep only referees on list *)
let rec keep_referees = function
  | [] -> []
  | x :: rest -> match x.role with
    | Referee -> x :: keep_referees rest
    | _ -> keep_referees rest
;;

let rec get_younger = function
  | [] -> []
  | x :: rest -> if x.age < 21 then x :: get_younger rest else get_younger rest
;;

let rec find_color = function (color, l) ->
match l with
  | [] -> None
  | x :: rest -> match x.role with
    | Player (c, _) -> if c = color then Some x else find_color (color, rest)
    | _ -> find_color (color, rest)
;;

(* tests *)
let people_list = [people1 ; people2 ; people3] ;;
let people_list2 = [people1 ; people2] ;;

log "keep_referees people_list = " ;;
let () = List.iter (log "%s ") (List.map (fun x -> x.name) (keep_referees people_list)) ;; log "\n" ;;
log "get_younger people_list = " ;;
let () = List.iter (log "%s ") (List.map (fun x -> x.name) (get_younger people_list)) ;; log "\n" ;;
log "find_color people_list = " ;;

let () = match find_color (Red, people_list) with
  | None -> log "None\n"
  | Some x -> log "%s has color %s\n" x.name (match x.role with
    | Player (c, _) -> if c = Red then "Red" else "Blue"
    | _ -> "None") ;;
log "find_color people_list2 = " ;;
let () = match find_color (Blue, people_list2) with
  | None -> log "None\n"
  | Some x -> log "%s has color %s\n" x.name (match x.role with
    | Player (c, _) -> if c = Red then "Red" else "Blue"
    | _ -> "None") ;;

(* filter *)

let rec filter f = function
  | [] -> []
  | x :: rest -> if f x then x :: filter f rest else filter f rest
;;

let keep_referees2 l = filter (fun x -> match x.role with
  | Referee -> true
  | _ -> false) l
;;

let get_younger2 l = filter (fun x -> x.age < 21) l ;;

let has_color color l = filter (fun x -> match x.role with
  | Player (c, _) -> if c = color then true else false
  | _ -> false) l
;;

(* tests *)

log "---- same with filter ----\n" ;;
log "keep_referees2 people_list = " ;;
let () = List.iter (log "%s ") (List.map (fun x -> x.name) (keep_referees2 people_list)) ;; log "\n" ;;
log "get_younger2 people_list = " ;;
let () = List.iter (log "%s ") (List.map (fun x -> x.name) (get_younger2 people_list)) ;; log "\n" ;;
log "has_color Red people_list = " ;;
let () = List.iter (log "%s ") (List.map (fun x -> x.name) (has_color Red people_list)) ;; log "\n" ;;
log "has_color Blue people_list2 = " ;;
let () = List.iter (log "%s ") (List.map (fun x -> x.name) (has_color Blue people_list2)) ;; log "\n" ;;

log "\n\nPartie 5\n" ;;
log "----------------\n" ;;

log "\n[Practicing on lists]\n" ;;

let nth l n =
  let rec loop i = function
    | [] -> failwith "out of bound"
    | x :: rest -> if i = n then x else loop (i + 1) rest
  in
  loop 0 l
;;

let rec nth2 l n =
  match l with
  | [] -> failwith "out of bound"
  | x :: rest -> if n = 0 then x else nth2 rest (n - 1)
;;

let rec rev = function
  | [] -> []
  | x :: rest -> rev rest @ [x]
;;

let append l1 l2 = l1 @ l2 ;;
let rev_append l1 l2 = rev l1 @ l2 ;;

let rec rev_map f = function
  | [] -> []
  | x :: rest -> rev_map f rest @ [f x]
;;

let rev_map_acc f l =
  let rec loop acu = function
    | [] -> acu
    | x :: rest -> loop (f x :: acu) rest
  in
  loop [] l
;;

(* tests *)

log "nth people_list 0 = %s\n" (nth people_list 0).name ;;
log "nth people_list 1 = %s\n" (nth people_list 1).name ;;
log "nth people_list 2 = %s\n" (nth people_list 2).name ;;

log "nth2 people_list 0 = %s\n" (nth2 people_list 0).name ;;
log "nth2 people_list 1 = %s\n" (nth2 people_list 1).name ;;

log "rev people_list = " ;;
let () = List.iter (log "%s ") (List.map (fun x -> x.name) (rev people_list)) ;; log "\n" ;;
log "append people_list people_list2 = " ;;
let () = List.iter (log "%s ") (List.map (fun x -> x.name) (append people_list people_list2)) ;; log "\n" ;;
log "rev_append people_list people_list2 = " ;;
let () = List.iter (log "%s ") (List.map (fun x -> x.name) (rev_append people_list people_list2)) ;; log "\n" ;;
log "rev_map (fun x -> x.name) people_list = " ;;
let () = List.iter (log "%s ") (rev_map (fun x -> x.name) people_list) ;; log "\n" ;;
log "rev_map_acc (fun x -> x.name) people_list = " ;;
let () = List.iter (log "%s ") (rev_map_acc (fun x -> x.name) people_list) ;; log "\n" ;;

let iter f list =
  let rec loop = function
    | [] -> ()
    | x :: rest -> f x ; loop rest
  in
  loop list
;;

log "my iter: \n" ;;
let () = iter (fun x -> log "%s " x.name) people_list ;; log "\n" ;;
iter (Printf.printf "Element: %d\n%!") [1;2;3] ;;

let rec print_list sep conv list =
  match list with
  | [] -> ()
  | [x] -> Printf.printf "%s" (conv x)
  | x :: rest -> Printf.printf "%s%s" (conv x) sep ; print_list sep conv rest
;;

log "print_list \", \" (fun x -> x.name) people_list = " ;;
print_list ", " (fun x -> x.name) people_list ;; log "\n" ;;

let rec print_list_return sep conv list =
  match list with
  | [] -> ""
  | [x] -> conv x
  | x :: rest -> conv x ^ sep ^ print_list_return sep conv rest
;;

log "print_list_return \", \" (fun x -> x.name) people_list = %s\n" (print_list_return ", " (fun x -> x.name) people_list) ;;

let rec fold op list =
  match list with
  | [] -> failwith "Empty list"
  | [x] -> x
  | x :: rest -> op x (fold op rest)
;;

log "fold (fun x y -> x * y) [1;2;3] = %d\n" (fold (fun x y -> x * y) [1;2;3]) ;;
log "fold (+) [1;2;3] = %d\n" (fold (+) [1;2;3]) ;;

let rec fold_acu op acu = function
  | [] -> acu
  | x :: rest -> fold_acu op (op acu x) rest
;;

log "fold_acu (+) [1;2;3] = %d\n" (fold_acu (+) 0 [1;2;3]) ;;

let rec exists pred = function
  | [] -> false
  | x :: rest -> if pred x then true else exists pred rest
;;

let fold_exists pred list = fold (fun x y -> x || y) (List.map pred list) ;;
let fold_acu_exists pred list = fold_acu (fun x y -> x || y) false (List.map pred list) ;;

log "exists (fun x -> x.age < 21) people_list = %b\n" (exists (fun x -> x.age < 21) people_list) ;;
log "fold_exists (fun x -> x.age < 21) people_list = %b\n" (fold_exists (fun x -> x.age < 21) people_list) ;;

let (++) f g = fun x -> f (g x) ;;

let forall pred list = not (exists (fun x -> not (pred x)) list) ;;

log "forall (fun x -> x.age < 21) people_list = %b\n" (forall (fun x -> x.age < 21) people_list) ;;
log "check: show ages for all : %s\n" (print_list_return ", " (fun x -> string_of_int x.age) people_list) ;;

let rec assoc key = function
  | [] -> raise Not_found
  | (k, v) :: rest -> if k = key then v else assoc key rest
;;

let assoc_list = List.map (fun x -> (x.name, x.age)) people_list ;;
let () = List.iter (log "%s ") (List.map (fun x -> x.name) people_list) ;; log "\n" ;;
log "assoc \"John\" assoc_list = %d\n" (assoc "John" assoc_list) ;;

let rec remove_assoc key = function
  | [] -> raise Not_found
  | (k, v) :: rest -> if k = key then rest else (k, v) :: remove_assoc key rest
;;

let new_assoc_list = remove_assoc "John" assoc_list ;;
log "new_assoc_list = " ;;
let () = List.iter (log "%s ") (List.map (fun (x, y) -> x ^ ":" ^ string_of_int y) new_assoc_list) ;; log "\n" ;;

log "\n[Trees]\n" ;;

type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree ;;

let rec depth = function
  | Leaf _ -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)
;;

let rec build depth value = 
  if depth = 0 then Leaf value else Node (value, build (depth - 1) value, build (depth - 1) value)
;;

let print_tree tos tree =
  let rec loop margin = function
    | Leaf x -> Printf.printf "___ %s\n%!" (tos x)
    | Node (value,a,b) ->
       Printf.printf "___%s" (tos value) ;
       loop (margin ^ "|   ") a ;
       Printf.printf "%s|\n%s|" margin margin ;
       loop (margin ^ "    ") b
  in
  loop "   " tree
;;

let tree = build 3 1 ;;
print_tree string_of_int tree ;;

(* build fold apply the provided function to create the value of the nth leaf *)
let rec build_fold depth f =
  let rec loop depth reversed_depth =
    if depth = 0 then Leaf (f reversed_depth) else Node (f reversed_depth, loop (depth - 1) (reversed_depth * 2), loop (depth - 1) (reversed_depth *2 + 1))
  in
  loop depth 1
;;

let tree = build_fold 3 (fun x -> x) ;;
print_tree string_of_int tree ;;

let rec tmap f = function
  | Leaf x -> Leaf (f x)
  | Node (value, left, right) -> Node (f value, tmap f left, tmap f right)
;;

let rec tfind f = function
  | Leaf x -> if f x then Some x else None
  | Node (value, left, right) -> if f value then Some value else match tfind f left with
    | None -> tfind f right
    | Some x -> Some x
;;

let rec tcontains f = function
  | Leaf x -> f x
  | Node (value, left, right) -> f value || tcontains f left || tcontains f right
;;

let tcontains_simple value = tcontains (fun x -> x = value) ;;

let rec treplace f sub = function
  | Leaf x -> if f (Leaf x) then sub else Leaf x
  | Node (value, left, right) -> if f (Node (value, left, right)) then sub else Node (value, treplace f sub left, treplace f sub right)
;;

(* tests *)

log "depth tree = %d\n" (depth tree) ;;
log "depth (Leaf 0) = %d\n" (depth (Leaf 0)) ;;

log "replacing all trees of depth 1 containing 14 by Leaf 0\n"
let new_tree = treplace (fun t -> tcontains_simple 14 t && depth t = 1) (Leaf 0) tree ;;

print_tree string_of_int new_tree ;;

log "\n\nPartie 6\n" ;;
log "----------------\n" ;;

log "\n[Compilation]\n" ;;

(* showdir :
  - print the current directory
  - read the content, sort it using Array.sort and the builtin compare
  - printf it
*)
let showdir () =
  let dir = Sys.getcwd () in
  let files = Sys.readdir dir in
  Array.sort compare files ;
  log "Current directory: %s\n" dir ;
  Array.iter (log "%s\n") files
;;

let () = showdir () ;;

log "\n[Exceptions]\n" ;;

exception Horrible_error ;;
exception Bad_bad_thing of int * string ;;

let test_raise x =
  try
    if x < 0 then raise Not_found
    else if x = 0 then failwith "Zero"
    else if x > 100 then raise Horrible_error
    else if x > 10 then raise (Bad_bad_thing (x, "Too big."))
    else [ string_of_int x ]
  with
    | Not_found -> []
    | Failure s -> [s]
    | Horrible_error -> ["Horrible error"]
    | Bad_bad_thing (x, s) -> [Printf.sprintf "Bad bad thing: %d %s" x s]
;;

let () = List.iter (log "%s ") (test_raise 0) ;; log "\n" ;;
let () = List.iter (log "%s ") (test_raise 1) ;; log "\n" ;;
let () = List.iter (log "%s ") (test_raise 11) ;; log "\n" ;;
let () = List.iter (log "%s ") (test_raise 101) ;; log "\n" ;;

let call f args =
  try
    f args
  with (* print failure *)
    | error -> log "Failure: %s\n" (Printexc.to_string error)
;;

let eval f args =
  try
    Ok (f args)
  with (* return failure *)
    | ex -> Error ex
;;

let rec check_all f = function
  | [] -> true
  | (x, exp) :: rest -> (eval f x) = exp && check_all f rest
;;

let take_two = function
  | [] -> raise Not_found
  | [x] -> raise Not_found
  | x :: y :: rest -> (x, y)
;;

log "TESTS:\n%b%! " (check_all take_two []) ;;
log "%b%! " (check_all take_two [ ([1], Error Not_found) ]) ;;
log "%b%! " (check_all take_two [ ([1], Ok (1,1)) ]) ;;
log "%b%! " (check_all take_two [ ([1], Error Not_found) ; ([4;3;2;1], Ok (4,3)) ]) ;;
log "%b%!\n" (check_all take_two [ ([1], Error Not_found) ; ([4;3;2;1], Error Not_found) ]) ;;

log "\n[Controled effects]\n" ;;

let calc x =
  Printf.printf "Computing x*x with x = %d\n%!" x ;
  x * x
;;

let cache f =
  let tbl = Hashtbl.create 10 in
  let inner arg = 
    try
      Hashtbl.find tbl arg
    with
      | Not_found ->
        let res = f arg in
        Hashtbl.add tbl arg res ;
        res
      in
  inner ;;

let cached_version_of_calc = cache calc ;;

log "Without cache\n" ;;
List.map calc [ 3 ; 2 ; 2 ; 3 ; 1 ; 2 ; 3 ; 2 ; 2 ; 1 ] ;;
log "With cache\n" ;;
List.map cached_version_of_calc [ 3 ; 2 ; 2 ; 3 ; 1 ; 2 ; 3 ; 2 ; 2 ; 1 ] ;;

type 'a tlazy = {
  mutable tvalue: 'a option ;
  fcompute: unit -> 'a
} ;;

let flazy f = { tvalue = None ; fcompute = f } ;;

let get_value l =
  match l.tvalue with
  | None ->
    let res = l.fcompute () in
    l.tvalue <- Some res ;
    log "Computed value: %d\n" res ;
    res
  | Some res -> res
;;

let arglazy f args = flazy (fun () -> f args) ;;

let intshow tlazy_val = log "%d\n" (get_value tlazy_val) ;;

log "\nLazyness\n"

let laz1 = arglazy calc 1
and laz2 = arglazy calc 2 ;;
let _ = intshow laz1 ;;
let _ = intshow laz1 ;;
let _ = intshow laz1 ;;
let _ = intshow laz2 ;;
let _ = intshow laz2 ;;
let _ = intshow laz2 ;;

(* [(x, lazy x) for x = 1 .. 100] *)
let lazy_list = List.map (fun x -> (x, arglazy calc x)) (List.init 100 (fun x -> x + 1)) ;;

let rec choose n = function
  | [] -> raise Not_found
  | (x, lazy_val) :: rest -> if x = n then get_value lazy_val else choose n rest
;;

log "choose 1 lazy_list = %d\n" (choose 1 lazy_list) ;;
log "choose 23 lazy_list = %d\n" (choose 23 lazy_list) ;;
log "choose 23 lazy_list = %d\n" (choose 23 lazy_list) ;;

log "\n[Value restrictions]\n"

let forbid x = function
  | a -> if a = x then failwith "Forbidden" else a ;;

let forbid_0 = forbid 0 ;;
let forbid_1 = forbid 1 ;;

log "forbid_0 1 = %d\n" (forbid_0 1) ;;
try
  log "forbid_0 0 = %d\n" (forbid_0 0) ;
with
  | Failure s -> log "Failure: %s\n" s