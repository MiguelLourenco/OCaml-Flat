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
 
open BasicTypes
open Scanner

module type CFGSyntaxSig =
sig
	type rule = { head : symbol; body : symbol list; }
	type rules = rule Set.t

	val parse : string Set.t -> rule Set.t
	val parseLine : string -> rule Set.t
	val toStringList : rule Set.t -> string list
	val (-->) : symbol -> string -> rule
	val show : rule Set.t -> unit
end

module CFGSyntax: CFGSyntaxSig =
struct
	type rule = { head : symbol; body : symbol list; }
	type rules = rule Set.t

	let isWhite c =
		List.mem c [' '; '\t']
	
	let rec parseHead () : symbol =
		match curr() with
			| ' ' -> invalid "Premature end of expression\n"
			| c -> skip() ; char2symb c

	let rec parseNeck (): unit =
		match curr() with
			| ' ' -> invalid "Premature end of expression\n"
			| '-' -> skip();
					if curr() = '>' then skip()
					else invalid "Bad neck\n"
			| _ -> invalid "Bad neck\n"

	let rec parseBody (): symbolList list =
		match curr() with
			| ' ' -> [[]]
			| '|' -> skip(); []::parseBody ()
			| '~' -> skip(); parseBody ()
			| c -> skip();
					match parseBody () with
						| [] -> invalid "never happens"
						| x::xs -> ((char2symb c)::x)::xs

	let parseLine line: rule Set.t =
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

	let parse rs: rule Set.t =
		Set.flatMap parseLine rs

	let toString1 r: string =
		let h = r.head in
		let b = if r.body = [] then [epsilon] else r.body in
		let full = [symb2str h; " -> "] @ (List.map symb2str b) in
			String.concat "" full

	let toString rs: string =
		let rl = Set.toList rs in
		String.concat "\n" (List.map toString1 rl)

	let toStringList rs: string list =
		let rl = Set.toList rs in
			List.map toString1 rl
	
	let (-->) h b : rule =
		{ head = h; body = str2word b } ;;

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
		if Util.testing active "CFGSyntax" then begin
			Util.header "test0";
			test0 ();
			Util.header "test1";
			test1 ();
			Util.header ""
		end
end
