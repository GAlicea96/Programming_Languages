(* Guillermo Alicea *)
(* Homework 3, 07/10/16 *)

(* sum a list recursively *)
(* assumes list contains ints, otherwise runtime error will occur *)
(* if list is empty return 0, else first element + list_sum [list excluding first element] *)
let rec list_sum l = 
  match l with
			[] -> 0
		| hd :: tl -> hd + list_sum tl
;;

(* determine the length of a list recursively *)
(* if list is empty return 0, else 1 + list_length [list excluding first element] *)
let rec list_length l =
  match l with
			[] -> 0
		| _ :: tl -> 1 + list_length tl
;;

(* using list_sum and list_length, determine the average of the elements in a list *)
let list_avg l =
  ((float_of_int (list_sum l)) /. (float_of_int (list_length l)))
;;

(* reverse a list *)
(* if list is empty, return an empty list, else concat list_rev [list excluding first element] with first element *)
let rec list_rev l =
	match l with
			[] -> []
		| hd :: tl -> (list_rev tl) @ [hd]
;;

(* combine two lists together *)
(* assumes lists have equal length, otherwise a runtime error will occur *)
(* if both lists are empty, return an empty list, else concat the first elements of each list *)
(* with list_zip [rest of list 1] [rest of list 2] *)
let rec list_zip l1 l2 =
  match (l1, l2) with
			([], []) -> []
		| (h1 :: t1, h2 :: t2) -> [(h1, h2)] @ list_zip t1 t2 
;;