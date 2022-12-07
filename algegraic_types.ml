(* type shape = 
              Circle of float
              | Rectangle of (float * float)
              | ComplexShape of shape list
;;
(* Below example of shape list containing the complexShape *)
(* [Circle 2.;Circle 3.;Rectangle (3.,4.);ComplexShape[Circle 2.; Rectangle(1.,1.)]] *)



(* area calculates the area of the certain shape or a list of shapes *)
(* if area is a complex-shape then we recur while calculating the area of each shape contained *)
let rec area s =
  match s with
  | Circle r -> 3.14 *. r *. r
  | Rectangle (w,h) -> w *. h
  | ComplexShape l -> match l with
                      | [] -> 0.
                      | hd :: tl -> area hd +. area (ComplexShape tl)
;;


let rec perimeter s =
  match s with
  | Circle r -> 2. *. Float.pi *. r
  | Rectangle (w,h) -> 2. *. (w +. h)
  | ComplexShape l -> match l with
                        | [] -> 0.
                        | hd :: tl -> perimeter hd +. perimeter (ComplexShape tl)
;; *)
