type people =
  { name: string ;
    age:  int }

let rec mk_people_list n =
  if n = 0 then [] else { name = "John-" ^ string_of_int n ; age = n*10 } :: mk_people_list (n-1)
;; 

let rec age_most cmp guy list =
  match list with
    | [] -> guy
    | x :: rest -> if cmp x.age guy.age then age_most cmp x rest else age_most cmp guy rest
;;

type author = Anonymous | Someone of people
type 'a contribution =
  { date: float ;
    author: author ;
    content: 'a }

let mike = { name = "Mike" ; age = 30 }
let rihanna = { name = "Rihanna" ; age = 28 }

let test_contr = [ { date = 100.0 ; author = Anonymous ; content = "afoo1" } ;
                   { date = 90.0  ; author = Someone mike ; content = "bar1" } ;
                   { date = 105.0 ; author = Anonymous ; content = "afoo2" } ;
                   { date = 107.0 ; author = Someone rihanna ; content ="bar2" } ;
                   { date = 102.0 ; author = Someone mike ; content = "bar3" } ] ;;

let rec filter_contributions = function
  | [] -> []
  | contribution :: rest -> if contribution.author = Anonymous then filter_contributions rest else contribution :: filter_contributions rest
;;

let rec map_contributions f = function
  | [] -> []
  | contribution :: rest -> { date = contribution.date ; author = contribution.author ; content = f contribution.content } :: map_contributions f rest
;;

let rec latest = function
  | [] -> failwith "Empty list"
  | [contrib] -> contrib
  | contrib :: rest -> let sub = latest rest in
    if contrib.date > sub.date then contrib else sub
;;

let rec contents_of people list =
  match list with
    | [] -> []
    | contrib :: rest -> if contrib.author = Someone people then contrib :: contents_of people rest else contents_of people rest
;;