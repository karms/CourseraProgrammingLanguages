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
	case xs 
	of [] => raise NoAnswer
	 | x::xs' => 
	 	case f x 
		of NONE => first_answer f xs'
		 | SOME found => found


fun all_answers f xs = 
	let fun walk (lst, acc) = 
		case lst 
		of [] => SOME acc
		 | x::xs' =>
			case f x 
				of NONE => NONE
				 | SOME found => walk (xs', found @ acc)
	in 
		walk (xs, [])
	end

val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = 
	g (fn x => 1) (fn x => String.size x)

fun count_some_var (s, p) = 
	g (fn x => 0) (fn x => if x = s then 1 else 0) p

val check_pat = 
	let 
		fun grab_variables all =
			case all 
			of ConstructorP(_, P) => grab_variables P
			 | TupleP xs => List.concat (map grab_variables xs)
			 | Variable v => [v]
			 | _ => []

		fun has_duplicates lst =
			case lst
			of [] => false
			| (x::xs) => List.exists (fn y => x = y) xs orelse has_duplicates xs

	in
		not o has_duplicates o grab_variables
	end

fun match v =
  case v
    of (_, Wildcard) => SOME []
     | (v, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (Const v, ConstP v') => if v = v' then SOME [] else NONE
     | (Tuple vs, TupleP ps) =>
         if length(vs) = length(ps)
         then all_answers match (ListPair.zip(vs, ps))
         else NONE
     | (Constructor(s2, v), ConstructorP(s1, p)) => 
         if s1 = s2
         then match(v, p)
         else NONE
     | _ => NONE

fun first_match v patterns =
  SOME(first_answer (fn p => match(v, p)) patterns)
  handle NoAnswer => NONE
