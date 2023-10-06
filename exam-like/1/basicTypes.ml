let rec e1q1 n s = 
  if n = 1 then s else s ^ (e1q1 (n-1) s)
;;

Printf.printf "%s\n" (e1q1 3 "ab") ;;

let rec e1q4 n =
  if n / 10 = 0 then 1 else 1 + (e1q4 (n/10))
;;

Printf.printf "%d\n" (e1q4 4543) ;;

let rec e1q2 n s =
  if n = 0 then "" else string_of_int (n mod 10) ^ s ^ (e1q2 (n/10) s)
;;

Printf.printf "%s\n" (e1q2 72 ":") ;;

let e1q3 n s =
  let rec e1q3_acu acu n s =
    if n = 0 then acu else e1q3_acu (string_of_int (n mod 10) ^ s ^ acu) (n/10) s
  in
  e1q3_acu "" n s
;;

Printf.printf "%s\n" (e1q3 72 ":") ;;

let rec e1q5 n even =
  if n = 0 then true else (e1q5 (n/10) even) && (n mod 2 = 0 = even)
;;

Printf.printf "%b\n" (e1q5 1357 false) ;;
Printf.printf "%b\n" (e1q5 1357 true) ;;
Printf.printf "%b\n" (e1q5 2468 false) ;;
Printf.printf "%b\n" (e1q5 2468 true) ;;

let rec e1q6 n m =
  if n = 0 then true else 
    if m = 0 then false else
      n mod 10 <= m mod 10 && e1q6 (n/10) (m/10)
;;

Printf.printf "%b\n" (e1q6 999 1648) ;; (* false *)
Printf.printf "%b\n" (e1q6 1234 999) ;; (* false *)
Printf.printf "%b\n" (e1q6 333 1444) ;; (* true *)