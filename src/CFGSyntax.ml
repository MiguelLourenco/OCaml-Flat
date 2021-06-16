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
 * jan/2021 (amd) - Module in an independent file.
 * dec/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Very simple parser for regular expressions.
 *
 * TODO: Register the errors using the Error module. Turn into a client
 * of the Scanner module.
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

	let inputString = ref ""
	let inputStringLength = ref 0
	let inputStringPosition = ref 0

	let isWhite c =
		List.mem c [' '; '\t']

	let skip () =
		inputStringPosition := !inputStringPosition + 1

	let rec curr () =
		if !inputStringPosition >= !inputStringLength then
			' '
		else if isWhite (String.get !inputString !inputStringPosition) then
			( skip(); curr() )
		else
			String.get !inputString !inputStringPosition

	let rec parseHead () =
		match curr() with
			| ' ' -> failwith "Premature end of expression\n"
			| c -> skip() ; c

	let rec parseNeck () =
		match curr() with
			| ' ' -> failwith "Premature end of expression\n"
			| '-' -> skip();
					if curr() = '>' then skip()
					else failwith "Bad neck\n"
			| _ -> failwith "Bad neck\n"

	let rec parseBody () =
		match curr() with
			| ' ' -> [[]]
			| '|' -> skip(); []::parseBody ()
			| '~' -> skip(); parseBody ()
			| c -> skip();
					match parseBody () with
						| [] -> failwith "never happens"
						| x::xs -> (c::x)::xs

	let parseLine line =
		if String.trim line = "" then Set.empty
		else (
			inputString := line;
			inputStringLength := String.length line;
			inputStringPosition := 0;
			let h = parseHead () in
			let () = parseNeck () in
			let bs = Set.make (parseBody ()) in
				Set.map (fun b -> {head=h; body=b}) bs
		)

	let parse rs =
		Set.flatMap parseLine rs


	let toString1 r =
		let full = [r.head; ' '; '-'; '>' ; ' '] @ r.body in
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
		if active then (
			Util.header "CFGSyntaxTests";
			Util.header "test0";
			test0 ();
			Util.header "test1";
			test1 ();
			Util.header ""
	)
end
