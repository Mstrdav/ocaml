(* 
true if c && (a&&b) || not c && not (a&&b)
without using &&, || and not  
*)
let ext_and a b c = 
  match (a,b,c) with
  | (true, true, true) -> true
  | (false, true, false) -> true
  | (true, false, false) -> true
  | (false, false, false) -> true
  | _ -> false
;;

Printf.printf "ttt: %b\n" (ext_and true true true) ;;
Printf.printf "tft: %b\n" (ext_and true false true) ;;
Printf.printf "tff: %b\n" (ext_and true false false) ;;
Printf.printf "ttf: %b\n" (ext_and true true false) ;;

let encode_char s =
  let replace x = function
    | 'a' -> 'e'
    | 'e' -> 'i'
    | 'i' -> 'o'
    | 'o' -> 'u'
    | 'u' -> 'y'
    | 'y' -> 'a'
    | x -> x
  in
  String.map (replace s) s
;;

(* J'ai pas fait les majuscules *)
Printf.printf "%s\n" (encode_char "Bonjour, je suis Colin") ;; 