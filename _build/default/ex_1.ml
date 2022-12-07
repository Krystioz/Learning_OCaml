(* let cube x = x *. x *. x;;

print_endline @@ string_of_float @@ cube 1.5;;


let sign x = if x = 0 then 0 else if x < 0 then -1 else 1;;

print_endline @@  string_of_int @@ sign 1;;
print_endline @@  string_of_int @@ sign 0;;
print_endline @@  string_of_int @@ -1;; *)
(* calculating root mean square *)


(* let square x = x *. x;;

let rms x y = square x +. square y |> (fun x -> x /. 2.) |> sqrt;; *)

let valid_date d m = match m with 
| "feb" -> d <= 28 && d >= 1 
| _ -> false
;;

valid_date (-1) "feb" |> string_of_bool |> print_endline;;
