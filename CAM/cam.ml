(* Guillermo Alicea *)
(* 07/27/16 *)

open List;;
open Printf;;
open String;;
open Char;;

(* create our list, arrays, and array indexes *)
let camCode = [];;
let stack = Array.make 50 0;;
let mem = Array.make 50 0;;

let cr = 0;;
let mr = 0;;
let sr = 0;;

(* read in, from "input.txt", each line and insert it into our camCode *)
(* will produce a list of strings, no \n, where each string is a line from input.txt *)
let file = open_in "input.txt";;

let rec input_lines file =

    let line = try [input_line file] with End_of_file -> [] in

        match line with

            [] -> []
            | _ ->  (line) @ (input_lines file)

;;

let camCode = (input_lines file);;

close_in file;;

(* We will trim the representation of instruction "number" that is present in each string *)
(* This will make it easier later. Will fail if input.txt is not formatted correctly *)
let rec trimCode camCode modd =

    match camCode with
    [] -> []
    | hd :: tl -> [(String.sub hd (2 + (modd / 10)) ((String.length hd) - (2 + modd / 10)))] @ (trimCode tl (modd + 1))
;;

(* Add a space at the end of eac string to account for a String.index used later *)
let rec addSpace camCode =

    match camCode with
    [] -> []
    | hd :: tl -> [(hd ^ " ")] @ (addSpace tl)
;;

let camCode = (trimCode camCode 0);;

let camCode = (addSpace camCode);;

(* used to input a value in array and then return the array *)
let addToArray array index value =

        array.(index) <- value;
        array
;;

(* swap two elements in array, then return array *)
let swapArray array index1 index2 =

    let temp = array.(index1) in
    array.(index1) <- array.(index2);
    array.(index2) <- temp;
    array
;;

(* used to perform the rot instructions on stack *)
let rot array index1 index2 index3 =

    let temp1 = array.(index1) in

        array.(index1) <- array.(index2);
        array.(index2) <- array.(index3);
        array.(index3) <- temp1;
        array
;;

(* used to perform the irot instructions on stack *)
let irot array index1 index2 index3 =

    let temp1 = array.(index1) in
    let temp2 = array.(index2) in

        array.(index1) <- array.(index3);
        array.(index2) <- temp1;
        array.(index3) <- temp2;
        array
;;

(* used to perform the cons instruction on stack and mem *)
let cons mem stack mr sr =

    mem.(mr) <- stack.(sr);
    mem.(mr + 1) <- stack.(sr - 1);
    mem
;;

(* used to perform split instructions on stack *)
let split stack mem sr =

    stack.(sr + 1) <- mem.(stack.(sr));
    stack.(sr) <- mem.(stack.(sr) + 1);
    stack
;;

(* used to perform app instructions on stack *)
let app stack sr cr =

    stack.(sr) <- stack.(sr - 1);
    stack.(sr - 1) <- (cr + 1);
    stack
;;

(* the main chunk of code that is a recursive function that updates *)
(* its arguments with each recursive call *)
(* first we output our instruction and contents, then we get the current instruction, *)
(* then we execute the proper instructions corresponding to whatever our current instruction is *)
let rec performCam camCode stack mem cr mr sr =

    printf "\nInstruction: %s\n" (List.nth camCode cr);
    printf "cr = %d\tmr = %d\tsr = %d\n" cr mr sr;
    printf "stack : [ ";
    Array.iter (printf "%d ") stack;
    printf "]\n";
    printf "mem   : [ ";
    Array.iter (printf "%d ") mem;
    printf "]\n";

    let workingCode = List.nth camCode cr in

    let string = (String.sub workingCode 0 (String.index workingCode ' ')) in

    if ((String.compare string "load") = 0) then

        if ((String.get workingCode 5) = '(') then

            performCam camCode (addToArray stack sr (min_int)) mem (cr + 1) mr sr

        else

            performCam camCode (addToArray stack sr (int_of_string (String.sub workingCode 5 ((String.index_from workingCode 5 ' ') - 5)))) mem (cr + 1) mr sr

    else if ((String.compare string "quote") = 0) then

        performCam camCode (addToArray stack (sr + 1) (int_of_string (String.sub workingCode 6 ((String.index_from workingCode 6 ' ') - 6)))) mem (cr + 1) mr (sr + 1)

    else if ((String.compare string "cur") = 0) then

        performCam camCode (addToArray stack (sr + 1) (int_of_string (String.sub workingCode 4 ((String.index_from workingCode 4 ' ') - 4)))) mem (cr + 1) mr (sr + 1)

    else if ((String.compare string "dupl") = 0) then

        performCam camCode (addToArray stack (sr + 1) (stack.(sr))) mem (cr + 1) mr (sr + 1)

    else if ((String.compare string "swap") = 0) then

        performCam camCode (swapArray stack sr (sr-1)) mem (cr + 1) mr sr

    else if ((String.compare string "rot") = 0) then

        performCam camCode (rot stack (sr) (sr - 1) (sr - 2)) mem (cr + 1) mr sr

    else if ((String.compare string "irot") = 0) then

        performCam camCode (irot stack (sr) (sr - 1) (sr - 2)) mem (cr + 1) mr sr

    else if ((String.compare string "plus") = 0) then

        performCam camCode (addToArray stack (sr-1) (stack.(sr) + stack.(sr - 1))) mem (cr + 1) mr (sr - 1)

    else if ((String.compare string "mult") = 0) then

        performCam camCode (addToArray stack (sr-1) (stack.(sr) * stack.(sr - 1))) mem (cr + 1) mr (sr - 1)

    (* the conditions involving 10000 are here as a marker for an element being an address rather than a value *)
    else if ((String.compare string "fst") = 0) then

        if (((stack.(sr)) / 1000000) > 0) then

            if (((mem.((stack.(sr)) - 1000000)) / 1000000) > 0) then

                performCam camCode (addToArray stack (sr) (mem.(mem.(stack.(sr) - 1000000)) - 1000000)) mem (cr + 1) mr (sr)

            else

                performCam camCode (addToArray stack (sr) (mem.(stack.(sr) - 1000000))) mem (cr + 1) mr (sr)
        else

            if ((mem.(stack.(sr)) / 1000000) > 0) then

                performCam camCode (addToArray stack (sr) (mem.(mem.(stack.(sr))) - 1000000)) mem (cr + 1) mr (sr)

            else

                performCam camCode (addToArray stack (sr) (mem.(stack.(sr)))) mem (cr + 1) mr (sr)

    (* the conditions involving 1000000 are here as a marker for an element being an address rather than a value *)
    else if ((String.compare string "snd") = 0) then

        if ((((stack.(sr)) / 1000000)) > 0) then

            if ((mem.((stack.(sr) - 1000000) + 1) / 1000000) > 0) then

                performCam camCode (addToArray stack (sr) (mem.(mem.((stack.(sr) - 1000000) + 1) - 1000000))) mem (cr + 1) mr (sr)

            else

                performCam camCode (addToArray stack (sr) (mem.((stack.(sr) + 1) - 1000000))) mem (cr + 1) mr (sr)
        else

            if ((mem.((stack.(sr) + 1) / 1000000)) > 0) then

                performCam camCode (addToArray stack (sr) (mem.(mem.(stack.(sr) + 1)))) mem (cr + 1) mr (sr)
            else

                performCam camCode (addToArray stack (sr) (mem.(stack.(sr) + 1))) mem (cr + 1) mr (sr)

    else if ((String.compare string "setfst") = 0) then

        performCam camCode  stack (addToArray mem (stack.(sr - 1)) (stack.(sr))) (cr + 1) mr (sr - 1)

    else if ((String.compare string "setsnd") = 0) then

        performCam camCode  stack (addToArray mem (stack.(sr - 1) + 1) (stack.(sr))) (cr + 1) mr (sr - 1)

    else if ((String.compare string "cons") = 0) then

        let temp = stack in

        performCam camCode (addToArray stack (sr - 1) (mr + 1000000)) (cons mem temp (mr) (sr)) (cr + 1) (mr + 2) (sr - 1)

    else if ((String.compare string "split") = 0) then

        performCam camCode (split stack mem sr) mem (cr + 1) mr (sr + 1)

    else if ((String.compare string "branch") = 0) then

        if (stack.(sr) != 0) then

            performCam camCode (addToArray stack sr (int_of_string (String.sub workingCode 7 ((String.index_from workingCode 7 ' ') - 7)))) mem (cr + 1) mr sr

        else

            performCam camCode (addToArray stack sr (int_of_string (String.sub workingCode ((String.index_from workingCode 7 ' ') + 1) ((String.index_from workingCode ((String.index_from workingCode 7 ' ') + 1) ' ') - (String.index_from workingCode 7 ' ') + 1)))) mem (cr + 1) mr sr

    else if ((String.compare string "app") = 0) then

        performCam camCode (app stack sr cr) mem (stack.(sr)) mr sr

    else if ((String.compare string "return") = 0) then

        let temp = stack.(sr - 1) in

        performCam camCode (addToArray stack (sr - 1) (stack.(sr))) mem (temp) mr (sr - 1)

    else if ((String.compare string "stop") = 0) then

        printf "stop\tcr = %d\tmr = %d\tsr = %d\t\n" (cr + 1) mr sr

;;

performCam camCode stack mem cr mr sr;;
