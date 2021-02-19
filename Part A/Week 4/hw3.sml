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

infix |>
fun x |> f = f x

val only_capitals = List.filter (fn str => String.sub (str, 0) |> Char.isUpper)

fun longest_string1 cs =
	let fun test (match, current) = 
		if String.size match > String.size current
		then match
		else current
	in foldl test "" cs end

val longest_string2 =
	foldl (fn (match, current) =>
			if (String.size match) >= (String.size current)
			then match
			else current
		) ""

fun longest_string_helper f =
	foldl (fn (match, current) => 
		if f (String.size match, String.size current)
		then match
		else current
	) ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = longest_string1 o only_capitals

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

fun first_answer f xs = 
		case xs of
    		[] => raise NoAnswer
		  | x::xs' => case f x of
						 NONE => first_answer f xs'
					   | SOME found => found


fun all_answers f xs = 
	let fun walk (lst, acc) = 
		case lst of
    		[] => SOME acc
		  | x::xs' => case f x of
						 NONE => NONE
					   | SOME found => walk (xs', found @ acc)
	in 
		walk (xs, [])
	end

val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = 
	g (fn x => 1) (fn x => String.size x)

fun count_some_var (s, p) = 
	g (fn x => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p = 
	let 
		fun all_strings p =
			p
		
		fun exists p =
			p

	in

	end