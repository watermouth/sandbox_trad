(* IO common *)
open CalendarLib
let date_of_string s = (* s = "yyyy-mm-dd" *)
  let lst = Str.split (Str.regexp_string "-") s in
  let lstInt = List.map (fun x -> int_of_string x) lst in
  let a = Array.of_list lstInt in
  Date.make a.(0) a.(1) a.(2)

(* test *)
let test1 = date_of_string "2012-11-12" = (Date.make 2012 11 12)
let test2 = date_of_string "2013-02-07" = (Date.make 2013 2 7)

let time_of_string s = (* s = "hh:mm:ss" *)
  let lst = Str.split (Str.regexp_string ":") s in
  let lstInt = List.map (fun x -> int_of_string x) lst in
  let a = Array.of_list lstInt in
  Time.make a.(0) a.(1) a.(2)

(* test *)
let test1 = time_of_string "7:00:00" = (Time.make 7 0 0)
let test2 = time_of_string "18:32:04" = (Time.make 18 32 4)


 
