let curry3 f a b c = f (a, b, c) ;;
let uncurry f (a, b, c) = f a b c ;;

let apply3 (f1, f2, f3) (v1, v2, v3) = (f1 v1, f2 v2, f3 v3) ;;

let sort2 f g x =
  if f x < g x then (f, g) else (g, f)
;;

let comp3 f g h = function x -> f (g (h x)) ;;