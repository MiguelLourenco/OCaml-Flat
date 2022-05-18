(*
 * ContextFreeGrammar.ml
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
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Improved Learn-OCaml support and error handling.
 * jun/2021 (amd) - Added checks for '~' in the #validate method.
 * may/2021 (amd) - Added support for an extern representation.
 * jan/2021 (amd) - Module in an independent file and some cleanup.
 * feb/2020 (jg) - Main functionalities.
 * dec/2019 (amd) - Initial skeleton, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Context-free grammar functionality.
 *
 * TODO: More cleanup.
 *)

open BasicTypes

module type ContextFreeGrammarSig =
sig
	type cfgTree = Leaf of char | Root of char * cfgTree list
	type tx = {
		alphabet : symbol list;
		variables : variable list;
		initial : variable;
		rules : string list
	}
	type t = {
		alphabet : symbols;
		variables : variables;
		initial : variable;
		rules : CFGSyntax.rules
	}
	val modelDesignation : string
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation: t
				method representationx : tx
				method validate : unit

				method tracing: unit
				method isRegular: bool
				method accept: word -> bool
				method acceptWithTracing: word -> unit
				method generate: int -> words

				method checkProperty : string -> bool
				method checkExercise: Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties
											
			(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t
			end
end

module ContextFreeGrammar : ContextFreeGrammarSig =
struct
	type cfgTree = Leaf of char | Root of char * cfgTree list

	open CFGSyntax

	type tx = {
		alphabet : symbol list;
		variables : variable list;
		initial : variable;
		rules : string list
	}

	type t = {
		alphabet : symbols;
		variables : variables;
		initial : variable;
		rules : CFGSyntax.rules
	}

	let modelDesignation = "context free grammar"

	let internalize (cfg: tx): t = {
		alphabet = Set.make cfg.alphabet;
		variables = Set.make cfg.variables;
		initial = cfg.initial;
		rules = CFGSyntax.parse (Set.make cfg.rules)
	}

	let externalize (cfg: t): tx = {
		alphabet = Set.toList cfg.alphabet;
		variables = Set.toList cfg.variables;
		initial = cfg.initial;
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

	let toJSon (rep: t): JSon.t =
		JSon.makeAssoc [
			("alphabet", JSon.makeSymbolSet rep.alphabet);
			("variables", JSon.makeSymbolSet rep.variables);
			("initial", JSon.makeSymbol rep.initial);
			("rules", JSon.makeStringSet (Set.map rule2str rep.rules))
		]

	(*------Auxiliary functions---------*)

	(* given a head, returns the set of all its bodies according to the cfg's rules *)
	let bodiesOfHead h rl =
		let rls = Set.filter (fun r -> r.head = h) rl in
			Set.map (fun r -> r.body) rls


	(* given 2 sets of words, to each word of the left set, appends each word of the right set *)
	let concatWords lws rws =
		if lws = Set.empty then rws
		else if rws = Set.empty then lws
		else
			let pairs = Set.combinations lws rws in
				Set.map (fun (x,y) -> x@y) pairs

	(* tests if the number of symbols in the given word exceeds the given lenght *)
	let exceedsMaxLen w l alph =
		let cleanWord = List.filter (fun c -> Set.belongs c alph) w in
			(List.length cleanWord) > l



	let subX h rws rl =
		let bs = bodiesOfHead h rl in
			concatWords bs rws


	(* applies the cfg's rules to the given word *)
	let rec subVar w vs rs =
		match w with
			| [] -> Set.make [[]]
			| x::xs -> if (Set.belongs x vs) then subX x (subVar xs vs rs) rs
				else concatWords (Set.make [[x]]) (subVar xs vs rs)


	(* removes the empty symbol from all non-empty words *)
	let removeEpsi w = List.filter (fun c -> c <> epsilon) w


	(* filters out all words that have variables and cleans any unnecessary epsilon *)
	let cleanNonWords ws vs =
		let hasVar w = List.exists (fun c -> Set.belongs c vs) w in
		let ws = Set.filter (fun w -> not (hasVar w)) ws in
			Set.map (fun w -> removeEpsi w) ws

	let displayHeader (name: string) (xTypeName: string) =
		if name = "" then
			""
		else
			("let " ^ name ^ ": " ^ xTypeName ^ " =\n\t\t")

	let toDisplayString (name: string) (xTypeName: string) (repx: tx): string =
		Printf.sprintf {zzz|
		%s{
			alphabet = %s;
			variables = %s;
			initial = %s;
			rules = %s
		}
		|zzz}
			(displayHeader name xTypeName)
			(Util.symbolList2DisplayString repx.alphabet)
			(Util.symbolList2DisplayString repx.variables)
			(Util.symbol2DisplayString repx.initial)
			(Util.stringList2DisplayString repx.rules)

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super

			val representation: t =
				match arg with
					| Arg.Representation r -> r
					| Arg.RepresentationX r -> internalize r
					| _ -> fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation =
				representation

			method representationx: tx =
				externalize representation

			method toJSon: JSon.t =
				JSon.append (super#toJSon) (toJSon representation)

			method validate: unit = (

				(* the alphabet must not contain epsilon ('~') *)
				let isValidAlphabet = not (Set.belongs epsilon representation.alphabet) in

				(* the variables must not contain epsilon ('~') *)
				let isValidVariables = not (Set.belongs epsilon representation.variables) in

				let isIntersectionValid = (Set.inter representation.variables representation.alphabet) = Set.empty in

				let isInitialValid = Set.belongs representation.initial representation.variables in

				let areRuleHeadsValid =
					let hs = Set.map (fun r -> r.head) representation.rules in
						Set.subset hs representation.variables
				in

				let areRuleBodiesValid =
					let bs = Set.map (fun r -> Set.make r.body) representation.rules in
					let allValidSymbs = Set.add epsilon (Set.union representation.alphabet representation.variables) in
					let res = Set.exists (fun b -> not (Set.subset b allValidSymbs)) bs in
						not res
				in
				
				if not isValidAlphabet then
					Error.error self#id.Entity.name
						"The alphabet contains epsilon '~', and it should not" ()
				;

				if not isValidVariables then
					Error.error self#id.Entity.name
						"The variables contain epsilon '~', and it should not" ()
				;

				if not isIntersectionValid then
					Error.error self#id.Entity.name
						"The intersection between the alphabet and the variables is not empty" ()
				;

				if not isInitialValid then
					Error.error (symb2str representation.initial)
						"Symbol initial does not belong to the set of all variables" ()
				;

				if not areRuleHeadsValid then
					Error.error self#id.Entity.name
						"Some rule heads do not belong to the set of all variables" ()
				;

				if not areRuleBodiesValid then
					Error.error self#id.Entity.name
						"Some rule bodies have invalid characters" ()
				)

			method tracing: unit = ()

			(* This method checks if the grammar is regular
			*
			* @returns bool -> true if regular, false otherwise
			*)
			method isRegular : bool =

				let vs = representation.variables in
				let alp = representation.alphabet in

				let bs = Set.map (fun r -> r.body) representation.rules in

				let isRightLinear bs =
					let isRightLinearX b =
						match b with
							| [a] -> (Set.belongs a alp) || a = epsilon
							| [a; v] -> (Set.belongs a alp) && (Set.belongs v vs)
							| _ -> false
					in
						Set.for_all (fun b -> isRightLinearX b) bs
				in

				let isLeftLinear bs =
					let rec isLeftLinearX b =
						match b with
							| [a] -> (Set.belongs a alp) || a = epsilon
							| [v; a] -> (Set.belongs v vs) && (Set.belongs a alp)
							| _ -> false
					in
						Set.for_all (fun b -> isLeftLinearX b) bs
				in

					isRightLinear bs || isLeftLinear bs


			(* This method checks if the given word is accepted by the grammar
			*
			* @param testWord -> word to be tested
			*
			* @returns bool -> true if it accepts the word, false otherwise
			*)
			method accept (testWord:word) : bool =

				(* any word with a symbol not from the cfg alphabet will not be accepted
				if not (Set.subset (Set.make testWord) representation.alphabet) then false else
				*)

				let vs = representation.variables in


				(* for word wa, get subword to the left of its first variable *)
				let rec getPrefix wa =
					match wa with
						| [] -> []
						| x::xs -> if Set.belongs x vs then [] else x::(getPrefix xs)
				in

				(* for word wa, get subword to the rigth of its last variable *)
				let getSuffix wa =
					let rec getSuffixX wa sfx =
						match wa with
							| [] -> sfx
							| x::xs -> let auxSfx = sfx@[x] in
										if Set.belongs x vs then getSuffixX xs []
											else getSuffixX xs auxSfx
					in
						getSuffixX wa []
				in

				let rec firstNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n > 0 then x::(firstNElements xs (n-1)) else []
				in

				let rec lastNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n < (List.length w) then lastNElements xs n else w
				in

				(* a word can be discarded if its prefix does not match the leftmostmost part of word w *)
				let keepByPrefix genW testW =
					let pgw = getPrefix genW in
					let ptw = firstNElements testW (List.length pgw) in
						pgw = [] || pgw = ptw
				in


				(* a word can be discarded if its suffix does not match the rightmost part of word w *)
				let keepBySufix genW testW =
					let sgw = getSuffix genW in
					let stw = lastNElements testW (List.length sgw) in
						sgw = [] || sgw = stw
				in

				(* the word inst discarded only if it cant be discarded by neither its prefix nor its suffix *)
				let toKeep w tw = (w = [] && tw = []) || (keepByPrefix w tw && keepBySufix w tw) in


				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in
				let l = List.length testWord in

				let nextGeneration ws =
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
					let rws = Set.filter (fun w -> not (exceedsMaxLen w l alph)) subsWs in
					let rws = Set.map (fun w -> removeEpsi w) rws in
						Set.filter (fun w -> toKeep w testWord ) rws

				in

				let start = Set.make [[representation.initial]] in

				let res = Set.historicalFixedPoint nextGeneration start in
					Set.exists (fun x -> x = testWord ) res




			method acceptWithTracing (testWord:word) =



				let vs = representation.variables in


				(* for word wa, get subword to the left of its first variable *)
				let rec getPrefix wa =
					match wa with
						| [] -> []
						| x::xs -> if Set.belongs x vs then [] else x::(getPrefix xs)
				in

				(* for word wa, get subword to the rigth of its last variable *)
				let getSuffix wa =
					let rec getSuffixX wa sfx =
						match wa with
							| [] -> sfx
							| x::xs -> let auxSfx = sfx@[x] in
										if Set.belongs x vs then getSuffixX xs []
											else getSuffixX xs auxSfx
					in
						getSuffixX wa []
				in

				let rec firstNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n > 0 then x::(firstNElements xs (n-1)) else []
				in

				let rec lastNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n < (List.length w) then lastNElements xs n else w
				in

				(* a word can be discarded if its prefix does not match the leftmostmost part of word w *)
				let keepByPrefix genW testW =
					let pgw = getPrefix genW in
					let ptw = firstNElements testW (List.length pgw) in
						pgw = [] || pgw = ptw
				in


				(* a word can be discarded if its suffix does not match the rightmost part of word w *)
				let keepBySufix genW testW =
					let sgw = getSuffix genW in
					let stw = lastNElements testW (List.length sgw) in
						sgw = [] || sgw = stw
				in

				(* the word inst discarded only if it cant be discarded by neither its prefix nor its suffix *)
				let toKeep w tw = (w = [] && tw = []) || (keepByPrefix w tw && keepBySufix w tw) in


				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in
				let l = List.length testWord in

				let nextGeneration ws =
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
					let rws = Set.filter (fun w -> not (exceedsMaxLen w l alph)) subsWs in
					let rws = Set.map (fun w -> removeEpsi w) rws in
						Set.filter (fun w -> toKeep w testWord ) rws

				in

				let start = Set.make [[representation.initial]] in

				let res = Set.historicalFixedPointTracing nextGeneration start in


				let trimRes l =
					match l with
					| [] -> []
					| x::xs -> if Set.belongs testWord x then xs
								else l
				in

				let res2 = List.rev (trimRes (List.rev res)) in


				let printWset ws =
					Util.print ["["];
					Set.iter (fun w -> Util.print [word2str w; ";"]) ws;
					Util.println ["]"];
				in

					List.iter (fun ws -> printWset ws) res2




			(* This method generates all words up the the given lenght that belong to the grammars language
			*
			* @ param lenght -> the max lenght of generated words
			*
			* @returns words -> the set of generated words
			*)
			method generate (length:int) : words =

				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in


				let nextGeneration ws =
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
						Set.filter (fun w -> not (exceedsMaxLen w length alph)) subsWs
				in

				let start = Set.make [[representation.initial]] in

				let res = Set.historicalFixedPoint nextGeneration start in

					cleanNonWords res vs

			method checkProperty (prop: string) =
				match prop with
					| "regular" -> self#isRegular
					| "context free grammar" -> true
					| _ -> super#checkProperty prop

		(* Learn-OCaml support *)
			method moduleName =
				"ContextFreeGrammar"

			method xTypeName =
				"contextFreeGrammar"

			method xTypeDeclString : string = {|
				type symbol = char
				type variable = char
				type rule = string
				type contextFreeGrammar = {
					alphabet : symbol list;
					variables : variable list;
					initial : variable;
					rules : rule list
				}
				|}

			method toDisplayString (name: string): string =
				toDisplayString name self#xTypeName self#representationx

			method example : JSon.t =
				JSon.parse {| {
					kind : "context free grammar",
					description : "this is an example",
					name : "cfg_simple",
					alphabet : ["0", "1"],
					variables : ["S", "X"],
					initial : "S",
					rules : [ "S -> 1S0 | X", "X -> 0X1 | ~" ]
				} |}
		end
end

module ContextFreeGrammarTests: sig end =
struct
	let active = false
	
	let test0 () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let j = m#toJSon in
			JSon.show j

	let test1 () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_balanced") in
		let e = new Exercise.exercise (Arg.Predef "exer_balanced") in
		let result = m#checkExercise e in
			if result then Util.println ["it works"] else Util.println ["it does not work"]

	let testRegular () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let ws = m#isRegular in
			if ws then Util.println ["is regular"] else Util.println ["is not regular"]


	let testAcc () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let ws = m#accept [] in
			if ws then Util.println ["Word was accepted"]
			else Util.println ["Word was not accepted"]


	let testTrace () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
			m#acceptWithTracing (word "01")

	let testGen () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let ws = m#generate 4 in
			Util.printWords ws

	let runAll =
		if Util.testing active "ContextFreeGrammar" then begin
			test0 ();
			test1 ();
			testRegular ();
			testAcc ();
			testTrace ();
			testGen ()
		end
end

