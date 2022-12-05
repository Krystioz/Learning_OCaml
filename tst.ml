let pi = 3.14;;
let square x = x *. x;;
let circle_area r = pi *. (square r);;


let add2 = (+) 2;;

let rec sum n = if n <=0 then 0 else n+ sum(n - 1);;
let rec sum_iter total current num = if current > num then total else sum_iter(total + current)(current + 1)num;;

let rec sum term m n = if m > n then 0 else term m + sum term (m + 1) n;;

let rec sum_int = sum (fun x -> x);;

let rec sum_squares = sum (fun x -> x * x);;

let rec accumulate combiner init term m n = 
  if m > n then init
  else combiner (term m) (accumulate
   combiner init term ((+) m 1) n)

let suma = accumulate ( * ) 1;;


let rec sum_int_cubes s e = if s > e then 0 else s * s * s + sum_int_cubes (s + 1) e;;




let div_mod x y = (x/y, x mod y);;

let translate_point (x,y)  dx dy = (x +. dx, y +. dy)
;;



let distance_point p1 p2 = match p1,p2 with
  | (x1,y1) ,(x2,y2) -> sqrt ((square(x1 -. x2)) +. (square(y1 -. y2)))
;;

let distance_point2 (x1,y1) (x2,y2) = sqrt ((square(x1 -. x2)) +. (square(y1 -. y2)))
;;

let rec enum_int a b = if a > b then [] else a::
  enum_int(a+1)b
;;

let hd l = match l with
    | [] -> failwith "Empty list"
    | x :: _ -> x 
;;

let tl l = match l with
    | [] -> failwith "Empty list"
    | _ :: xs -> xs
;;

let rec nth l n = if n <= 0 then List.hd l
  else nth (List.tl l) (n -1)
;;

let rec length l = match l with
  | [] -> 0
  | hd :: tl -> 1 + length tl
;;

(* List.length is the same as length *)
(* @ is the same as append *)

let rec append l1 l2 = match l1 with
  | [] -> l2
  | x :: xs -> x :: append xs l2
;;

let rec reverse l = match l with
  | [] -> l
  | hd :: tl -> (reverse tl) @ [hd]
;;

reverse [1;2;3;4;5]


let rec list_max l =
  match l with
  | [] -> None
  | hd :: tl -> match list_max tl with
                    | None -> Some hd
                    | Some m -> Some (max hd m)
;;


let rec square_list l=
  match l with
  | [] -> []
  | hd :: tl -> (hd *. hd) :: square_list tl
;;

let rec map l f =
  match l with
  | [] -> []
  | hd :: tl -> (f hd) :: map tl f
;;


List.map (fun x -> x * x) [1;2;3]