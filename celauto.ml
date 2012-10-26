(*
 * Copyright 2008 Marcelino Alberdi <marcelino.alberdi@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

type state = E | F;;

let char_of_state = function
    E -> ' '
  | F -> '@';;

let create_rule n =
  let rec aux n l = match n with
      0 -> raise (Failure "Bad rule")
    | 1 -> l
    | _ -> aux (n/2) ((n mod 2)::l)
  in let l = List.map (fun x -> if x = 1 then F else E) (aux (n+256) [])
     and pattern = [F,F,F; F,F,E; F,E,F; F,E,E; E,F,F; E,F,E; E,E,F; E,E,E]
  in List.combine pattern l;;

let ruler rule line n =
  let rec aux rule next = function
    | a::b::c::t -> aux rule ((List.assoc (a,b,c) rule)::next) (b::c::t)
    | a::b::t -> aux rule ((List.assoc (a,b,E) rule)::next) (b::t)
    | a::t -> List.tl (List.rev next)
    | _ -> raise (Failure "Bad line")
  in let rec iter rule lines = function
      1 -> List.rev lines
    | i -> iter rule ((aux rule [] (E::E::(List.hd lines)))::lines) (i-1)
  in iter rule [line] n;;

let rec print_line = function
    [] -> print_endline ""
  | a::t -> let _ = print_char (char_of_state a) in print_line t;;

let rec print_result = function
    [] -> ()
  | a::t -> let _ = print_line a in print_result t;;

let parse_states s =
  let rec aux s i l =
    let empty_states = [' '; '.'; '0'; 'E']
    in let p = if List.mem s.[i] empty_states then E else F
    in if i < (String.length s)-1 then aux s (i+1) (p::l) else List.rev (p::l)
  in aux s 0 [];;

let main () =
  let rule = create_rule (int_of_string(Sys.argv.(1)))
  and states = parse_states Sys.argv.(2)
  and i = int_of_string(Sys.argv.(3))
  in print_result (ruler rule states i);;

try
  main ()
with _ ->  print_endline "Usage: celauto rule \"states\" iterations"
