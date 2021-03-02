(*
 * RegExpSyntax.ml
 *
 * This file is part of the OCamlFlat library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Very simple parser for regular expressions.
 *
 * TODO: Register the errors using the Error module. Turn into a client
 * of the Scanner module.
 *)

module type RegExpSyntaxSig =
sig
	type t =
		| Plus of t * t
		| Seq of t * t
		| Star of t
		| Symb of char
		| Empty
		| Zero
 
	val parse : string -> t
	val toString : t -> string
	val show : t -> unit
end

module RegExpSyntax : RegExpSyntaxSig =
struct
		(* Grammar:
				E -> E + E | E E | E* | c | (E) | ()

			Grammar with priorities:
				E -> T | T + E
				T -> F | F T
				F -> A | A*
				A -> P | c
				P -> (E) | ()
		*)
	type t =
		| Plus of t * t
		| Seq of t * t
		| Star of t
		| Symb of char
		| Empty
		| Zero
	;;

	let inputString = ref ""
	let inputStringLength = ref 0
	let inputStringPosition = ref 0

	let skip () =
		inputStringPosition := !inputStringPosition + 1

	let rec curr () =
		if !inputStringPosition >= !inputStringLength then
			' '
		else if String.get !inputString !inputStringPosition = ' ' then
			( skip(); curr() )
		else
			String.get !inputString !inputStringPosition

	let rec parse_exp () =
		let t = parse_term () in
			match curr() with
				| '+' -> skip(); Plus (t, parse_exp ())
				| _ -> t

	and parse_term () =
		let f = parse_factor () in
			match curr() with
				| '+' | ')' | ' ' -> f
				| _ -> Seq (f, parse_term ())

	and parse_factor () =
		let a = parse_atom () in
			match curr() with
				| '*' -> skip(); (Star a)
				| _ -> a

	and parse_atom () =
		match curr() with
			| '~' -> skip(); Empty
			| '!' -> skip(); Zero
			| '(' -> skip(); parse_parentised ()
			| '+' | '*' -> failwith "Invalid use of wildcard\n"
			| ' ' -> failwith "Premature end of expression\n"
			| c -> skip(); (Symb c)

	and parse_parentised () =
		let e = parse_exp () in (
			match curr() with
				| ')' -> skip(); e
				| _ -> failwith "Right-parenthesis expected\n"
		)

	let parse s =
		inputString := s;
		inputStringLength := String.length s;
		inputStringPosition := 0;
		parse_exp ()

	let rec toStringN n re =
		match re with
			| Plus(l, r) ->
					(if n > 0 then "(" else "") ^
					toStringN 0 l ^ "+" ^ toStringN 0 r
					^ (if n > 0 then ")" else "")
			| Seq(l, r) ->
					(if n > 1 then "(" else "") ^
					toStringN 1 l ^ toStringN 1 r
					^ (if n > 1 then ")" else "")
			| Star(r) ->
					toStringN 2 r ^ "*"
			| Symb(c) -> String.make 1 c
			| Empty -> "~"
			| Zero -> "!"

	let toString re =
		toStringN 0 re

	let show re =
		Util.println [toString re]
end

module RegExpSyntaxTests = struct
	let active = false

	let test0 () =
		let re = RegExpSyntax.parse "ab+~*" in
			RegExpSyntax.show re

	let test1 () =
		let re = RegExpSyntax.parse "~((a+b)*(cd)*)*" in
			RegExpSyntax.show re

	let runAll =
		if active then (
			Util.header "RegExpSyntaxTests";
			test0 ();
			test1 ()
	)
end
