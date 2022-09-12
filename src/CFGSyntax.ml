(*
 * CFGSyntax.ml
 *
 * This file is part of the OCamlFLAT library
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
 * sep/2022 (amd) - New submodules CFGConversions and CFGForLearnOCaml
 * jul/2021 (amd) - Now this module is client of the Scanner module and
 *                  the erros are registered using the Error module.
 * jan/2021 (amd) - Module moved to an independent file.
 * dec/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Support types and functions for CFGs including a parser for CFGs.
 *)
 
open BasicTypes

module CFGTypes =
struct
	type rule =
		{ head : symbol; body : word; }
	type rules =
		rule Set.t
	type tx = {
		alphabet : symbolX list;
		variables : variableX list;
		initial : variableX;
		rules : string list
	}
	type contextFreeGrammar =
		tx
	type t = {
		alphabet : symbols;
		variables : variables;
		initial : variable;
		rules : rules
	}
	type cfg =
		t
	type cfgTree =
		  Leaf of symbol
		| Root of symbol * cfgTree list
end

module type CFGSyntaxSig =
sig
	open CFGTypes

	val parse : string Set.t -> rule Set.t
	val parseLine : string -> rule Set.t
	val toStringList : rule Set.t -> string list
	val (-->) : symbol -> string -> rule
	val show : rule Set.t -> unit
end

module CFGSyntax: CFGSyntaxSig =
struct
	open Scanner
	open CFGTypes

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

	let rec parseBody (): word list =
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

module CFGConversions =
struct
	open CFGTypes

	let internalize (cfg: tx): t = {
		alphabet = symbolsX2symbols cfg.alphabet;
		variables = symbolsX2symbols cfg.variables;
		initial = symbX2symb cfg.initial;
		rules = CFGSyntax.parse (Set.make cfg.rules)
	}

	let externalize (cfg: t): tx = {
		alphabet = symbols2symbolsX cfg.alphabet;
		variables = symbols2symbolsX cfg.variables;
		initial = symb2symbX cfg.initial;
		rules = CFGSyntax.toStringList cfg.rules
	}

	let fromJSon j =
		if JSon.isNull j || not (JSon.hasField j "kind") then {
			alphabet = Set.empty;
			variables = Set.make [draftVar];
			initial = draftVar;
			rules = Set.empty;
		}
		else {
			alphabet = JSon.fieldSymbolSet j "alphabet";
			variables = JSon.fieldSymbolSet j "variables";
			initial = JSon.fieldSymbol j "initial";
			rules = CFGSyntax.parse (JSon.fieldStringSet j "rules");
		}

	let rule2str {head=h; body=b} =
		let bb = if b = [] then [epsilon] else b in
			(symb2str h) ^ " -> " ^ (word2str bb)

	let toJSon (id: JSon.t) (rep: t): JSon.t =
		let body =
			JSon.makeAssoc [
				("alphabet", JSon.makeSymbolSet rep.alphabet);
				("variables", JSon.makeSymbolSet rep.variables);
				("initial", JSon.makeSymbol rep.initial);
				("rules", JSon.makeStringSet (Set.map rule2str rep.rules))
			]
		in JSon.append id body
end

module CFGForLearnOCaml =
struct
	open CFGTypes

	let moduleName =
		"ContextFreeGrammar"

	let xTypeName =
		"contextFreeGrammar"

	let solution (name: string) (repx: tx): string =
		Printf.sprintf {zzz|
		%s{
			alphabet = %s;
			variables = %s;
			initial = %s;
			rules = %s
		}
		|zzz}	(* please, do not change this line *)
			(FinAutForLearnOCaml.displayHeader name xTypeName)
			(symbolsX2display repx.alphabet)
			(symbolsX2display repx.variables)
			(symbX2display repx.initial)
			(strings2display repx.rules)

	let prelude : string =
		Printf.sprintf {zzz|
			type symbol = %s
			type variable = %s
			type rule = string
			type contextFreeGrammar = {
				alphabet : symbol list;
				variables : variable list;
				initial : variable;
				rules : rule list
			}
		|zzz}	(* please, do not change this line *)
				symbolTypeName symbolTypeName

		let example : JSon.t =
			JSon.parse {| {
				kind : "context free grammar",
				description : "this is an example",
				name : "cfg_simple",
				alphabet : ["0", "1"],
				variables : ["S", "X"],
				initial : "S",
				rules : [ "S -> 1S0 | X", "X -> 0X1 | ~" ]
			}
			|}	(* please, do not change this line *)
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
