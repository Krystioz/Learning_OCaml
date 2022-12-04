type 'a bin_tree = 
  | Leaf
  | Node of 'a bin_tree * 'a * 'a bin_tree
;;

(* example of a binary tree as algebraic data type *)

(Node (Node (Leaf,2,Leaf), 1, Node (Node (Leaf,4,Leaf), 3, Leaf)))

(* size - get the number of nodes in a binary tree *)

let rec size t =
  match t with
  | Leaf -> 0
  | Node  (l,_,r) -> size l + 1 + size r
;;

size (Node (Node (Leaf,2,Leaf), 1, Node (Node (Leaf,4,Leaf), 3, Leaf)))

(* Calculate the sum of nodes in a tree of integers *)

let rec sum_tree l = 
  match l with
  | Leaf -> 0
  | Node (l,x,r) -> sum_tree l + x + sum_tree r
;;

sum_tree (Node (Node (Leaf,2,Leaf), 1, Node (Node (Leaf,4,Leaf), 3, Leaf)))

(* either datatype capturing values with 2 possibilities *)
(* OCaml types must be lowercase *)

type ('a,'b) either = Left of 'a | Right of 'b

let safe_div a b = if b <> 0 then Right (a / b) else Left "Division by zero"

type bool = True | False

type 'a mylist = Nil | Cons of 'a * 'a mylist

