(*
 * CFGSyntax.ml
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
 * dec/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: A very simple parser for CFG syntax.
 *)

module type CFGSyntaxSig =
sig
	type rule = { head : char; body : char list; }
	type rules = rule Set.t

	val parse : string Set.t -> rule Set.t
	val toStringList : rule Set.t -> string list
	val (-->) : char -> string -> rule
	val show : rule Set.t -> unit
end

module CFGSyntax: CFGSyntaxSig =
struct
	type rule = { head : char; body : char list; }
	type rules = rule Set.t

	open Scanner

	let isWhite c =
		List.mem c [' '; '\t']

	let rec parseHead () =
		match curr() with
			| ' ' -> invalid "Premature end of expression\n"
			| c -> skip() ; c

	let rec parseNeck () =
		match curr() with
			| ' ' -> invalid "Premature end of expression\n"
			| '-' -> skip();
					if curr() = '>' then skip()
					else invalid "Bad neck\n"
			| _ -> invalid "Bad neck\n"

	let rec parseBody () =
		match curr() with
			| ' ' -> [[]]
			| '|' -> skip(); []::parseBody ()
			| '~' -> skip(); parseBody ()
			| c -> skip();
					match parseBody () with
						| [] -> invalid "never happens"
						| x::xs -> (c::x)::xs

	let parseLine line =
		if String.trim line = "" then
			Set.empty
		else (
			Scanner.start "CFGSyntax" line;
			try
				let h = parseHead () in
				let _ = parseNeck () in
				let bs = Set.make (parseBody ()) in
					Set.map (fun b -> {head=h; body=b}) bs
			with Not_found ->
				Set.empty
		)

	let parse rs =
		Set.flatMap parseLine rs

	let toString1 r =
		let h = r.head in
		let b = if r.body = [] then [epsilon] else r.body in
		let full = [h; ' '; '-'; '>' ; ' '] @ b in
			String.concat "" (List.map (String.make 1) full)

	let toString rs =
		let rl = Set.toList rs in
		String.concat "\n" (List.map toString1 rl)

	let toStringList rs =
		let rl = Set.toList rs in
			List.map toString1 rl
	
	let (-->) h b : rule =
		{ head = h; body = Util.str2word b } ;;

	let show rs =
		Util.println [toString rs]
end

module CFGSyntaxTests =
struct
	let active = false

	let test0 () =
		let cfg = Set.make [ "S -> aTb | ~"; "T -> aSb" ] in
		let rules = CFGSyntax.parse cfg in
			CFGSyntax.show rules

	let test1 () =
		let cfg = Set.make ["S -> aSb | ~"] in
		let rules = CFGSyntax.parse cfg in
			CFGSyntax.show rules

	let runAll =
		if Util.testing(active) then (
			Util.header "CFGSyntaxTests";
			Util.header "test0";
			test0 ();
			Util.header "test1";
			test1 ();
			Util.header ""
	)
end
