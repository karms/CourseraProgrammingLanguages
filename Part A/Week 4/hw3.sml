use "hw3provided.sml";
(*

the bindings for datatype and exception definitions:

val g = fn : (unit -> int) -> (string -> int) -> pattern -> int
val only_capitals = fn : string list -> string list
val longest_string1 = fn : string list -> string
val longest_string2 = fn : string list -> string
val longest_string_helper = fn : (int * int -> bool) -> string list -> string
val longest_string3 = fn : string list -> string
val longest_string4 = fn : string list -> string
val longest_capitalized = fn : string list -> string
val rev_string = fn : string -> string

*)

fun starts_with_capital s = 
  let 
    val c = String.sub (s, 0)
  in 
    ord(c) >= ord(#"A") andalso ord(c) <= ord(#"Z")
  end 
  

fun only_capitals cs = 
  case cs of
      [] => []
    | s :: cs' => 
        if starts_with_capital s
        then s :: only_capitals cs'
        else only_capitals cs'

fun longest_string1 cs =
  let fun test (match, current) = 
    if String.size match > String.size current
	then match
	else current
  in foldl test "" cs end

fun longest_string2 cs =
  let fun test (match, current) = 
    if String.size match >= String.size current
	then match
	else current
  in foldl test "" cs end

fun longest_string_helper f cs match =
let fun test (match, current) = 
    if f (String.size match) (String.size current)
	then match
	else current
  in foldl test "" cs end

fun fix3 f a b c = f a c b
val longest_string3 = fix3 longest_string_helper (fn a => fn b => a > b) ""

val longest_string4 = fix3 longest_string_helper (fn a => fn b => a >= b) ""

val rev_string = String.implode o List.rev o String.explode


(* 

val first_answer = fn : (’a -> ’b option) -> ’a list -> ’b
val all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option
val count_wildcards = fn : pattern -> int
val count_wild_and_variable_lengths = fn : pattern -> int
val count_some_var = fn : string * pattern -> int
val check_pat = fn : pattern -> bool
val match = fn : valu * pattern -> (string * valu) list option
val first_match = fn : valu -> pattern list -> (string * valu) list option 

*)
