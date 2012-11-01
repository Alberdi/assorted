(*Copyright (c) 2012, Marcelino Alberdi Pereira <marcelino.alberdi@gmail.com>

Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is
hereby granted, provided that the above copyright notice and this permission notice appear in all copies.*)

open Batteries_uni

(* (left, current, right) *)
let parse_brainfuck_operation (l, c, r) = function
  (* Moves to the cell in the right. *)
  | '>' -> (match r with
         | [] -> (c::l, '\000', [])
         | h::t -> (c::l, h, t))
  (* Moves to the cell in the left. *)
  | '<' -> (match l with
           [] -> ([], '\000', c::r)
         | h::t -> (t, h, c::r))
  (* Increments the value of the current cell. *)
  | '+' -> (l, Char.chr (Char.code c+1), r)
  (* Decrements the value of the current cell. *)
  | '-' -> (l, Char.chr (Char.code c-1), r)
  (* Outputs the value of the current cell. *)
  | '.' -> IO.write stdout c; (l, c, r)
  (* Gets input for the value of the current cell. *)
  | ',' -> (l, IO.read stdin, r)
  (* Any other character is a noop. *)
  | _ -> (l, c, r);;

let rec prev_bracket n count = function
  | h::t -> (match h with
            | ']' -> prev_bracket (h::n) (count+1) t
            | '[' -> if count = 0
                       then (t, h::n)
                       else prev_bracket (h::n) (count-1) t
            | _ -> prev_bracket (h::n) count t
            )
    | _ -> raise (Failure "Unmatched closing bracket");;

let rec next_bracket p count = function
  | h::t -> (match h with
            | '[' -> next_bracket (h::p) (count+1) t
            | ']' -> if count = 0
                       then (h::p, t)
                       else next_bracket (h::p) (count-1) t
            | _ -> next_bracket (h::p) count t
            )
    | _ -> raise (Failure "Unmatched opening bracket");;

(* (left, current, right) (previous, next) *)
let rec parse_brainfuck (l, c, r) (p, n) = match n with
  | h::t -> (match h with
          | '[' -> if c = '\000'
                     (* Jump to next matching ]. *)
                     then let x = next_bracket (h::p) 0 t in parse_brainfuck (l, c, r) x
                     (* Continue with the next instruction. *)
                     else parse_brainfuck (l, c, r) (h::p, t)
          | ']' -> if c = '\000' 
                     (* Continue with the next instruction. *)
                     then parse_brainfuck (l, c, r) (h::p, t)
                     (* Jump back to previous matching [. *)
                     else let x = prev_bracket (h::t) 0 p in parse_brainfuck (l, c, r) x
          (* Process any other character. *)
          | _ -> parse_brainfuck (parse_brainfuck_operation (l, c, r) h) (h::p, t)
          )
  | _ -> ();;

let () =
  if Array.length Sys.argv >= 2
    then parse_brainfuck ([], '\000', []) ([], BatString.to_list (BatIO.read_all (open_in Sys.argv.(1))))
    else print_endline "Usage: yabfi filename"

