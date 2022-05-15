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
 * jul/2021 (amd) - Now this module is client of the Scanner module and
 *                  the erros are registered using the Error module.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: A very simple parser for regular expressions.
 *)

open BasicTypes
open Scanner

module type RegExpSyntaxSig =
sig
	type t =
		| Plus of t * t
		| Seq of t * t
		| Star of t
		| Symb of symbol
		| Empty
		| Zero
 
	val parse : string -> t
	val toString : t -> string
	val show : t -> unit
end

module RegExpSyntax : RegExpSyntaxSig =
struct
	(*	Grammar:
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
		| Symb of symbol
		| Empty
		| Zero

	let rec parseExp () =
		let t = parseTerm () in
			match curr() with
				| '+' -> skip(); Plus (t, parseExp ())
				| _ -> t

	and parseTerm () =
		let f = parseFactor () in
			match curr() with
				| '+' | ')' | ' ' -> f
				| _ -> Seq (f, parseTerm ())

	and parseFactor () =
		let a = parseAtom () in
			match curr() with
				| '*' -> skip(); (Star a)
				| _ -> a

	and parseAtom () =
		match curr() with
			| '~' -> skip(); Empty
			| '!' -> skip(); Zero
			| '(' -> skip(); parseParentised ()
			| '+' | '*' -> invalid "Invalid use of wildcard\n"
			| ' ' -> invalid "Premature end of expression\n"
			| c -> skip(); (Symb (char2symb c))

	and parseParentised () =
		let e = parseExp () in (
			match curr() with
				| ')' -> skip(); e
				| _ -> invalid "Right-parenthesis expected\n"
		)

	let parse s =
		Scanner.start "RegExpSyntax" s;
		try
			parseExp ()
		with Not_found ->
			Zero

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
			| Symb(c) -> symb2str c
			| Empty -> "~"
			| Zero -> "!"

	let toString re =
		toStringN 0 re

	let show re =
		Util.println [toString re]
end

module RegExpSyntaxTests =
struct
	let active = false

	let test0 () =
		let re = RegExpSyntax.parse "ab+~*" in
			RegExpSyntax.show re

	let test1 () =
		let re = RegExpSyntax.parse "~((a+b)*(cd)*)*" in
			RegExpSyntax.show re

	let runAll =
		if Util.testing active "RegExpSyntax" then begin
			test0 ();
			test1 ()
		end
end
