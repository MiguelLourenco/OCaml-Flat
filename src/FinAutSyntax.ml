(*
 * FinAutSyntax.ml
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
 * sep/2022 (amd) - New module
 *)

(*
 * Description: Support types and functions for FAs.
 *)

open BasicTypes

module FinAutTypes =
struct
	type tx = {
		alphabet : symbolX list;
		states : state list;
		initialState : state;
		transitions : transition3X list;
		acceptStates : state list
	}
	type finiteAutomaton =
		tx
	type t = {
		alphabet : symbols;
		states : states;
		initialState : state;
		transitions : transitions3;
		acceptStates : states
	}
	type fa =
		t
end

module FinAutConversions =
struct
	open FinAutTypes

	let internalize (fa: tx): t = {
		alphabet = symbolsX2symbols fa.alphabet;
		states = Set.make fa.states;
		initialState = fa.initialState;
		transitions = transsX2transs3 fa.transitions;
		acceptStates = Set.make fa.acceptStates
	}

	let externalize (fa: t): tx = {
		alphabet = symbols2symbolsX fa.alphabet;
		states = Set.toList fa.states;
		initialState = fa.initialState;
		transitions = transs2transsX3 fa.transitions;
		acceptStates = Set.toList fa.acceptStates
	}

	let fromJSon (j: JSon.t): t =
		if JSon.isNull j || not (JSon.hasField j "kind") then {
			alphabet = Set.empty;
			states = Set.make [draftState];
			initialState = draftState;
			transitions = Set.empty;
			acceptStates = Set.empty
		}
		else {
			alphabet = JSon.fieldSymbolSet j "alphabet";
			states = JSon.fieldStateSet j "states";
			initialState = JSon.fieldState j "initialState";
			transitions = JSon.fieldTriplesSet j "transitions";
			acceptStates = JSon.fieldStateSet j "acceptStates"
		}

	let toJSon (id: JSon.t) (rep: t): JSon.t =
		let body =
			JSon.makeAssoc [
				("alphabet", JSon.makeSymbolSet rep.alphabet);
				("states", JSon.makeStateSet rep.states);
				("initialState", JSon.makeState rep.initialState);
				("transitions", JSon.makeTriplesSet rep.transitions);
				("acceptStates", JSon.makeStateSet rep.acceptStates)
			]
		in JSon.append id body
end

module FinAutForLearnOCaml =
struct
	open FinAutTypes

	let moduleName =
		"FiniteAutomaton"

	let xTypeName =
		"finiteAutomaton"

	let displayHeader (name: string) (xTypeName: string) =
		if name = "" then ""
		else ("let " ^ name ^ ": " ^ xTypeName ^ " =\n\t\t")

	let solution (name: string) (repx: tx): string =
		Printf.sprintf {zzz|
		%s{
			alphabet = %s;
			states = %s;
			initialState = %s;
			transitions = %s;
			acceptStates = %s
		}
		|zzz}	(* please, do not change this line *)
			(displayHeader name xTypeName)
			(symbolsX2display repx.alphabet)
			(statesX2display repx.states)
			(state2display repx.initialState)
			(transsX2display3 repx.transitions)
			(statesX2display repx.acceptStates)


	let prelude : string =
		Printf.sprintf {zzz|
		type symbol = %s
		type state = string
		type finiteAutomaton = {
			alphabet : symbol list;
			states : state list;
			initialState : state;
			transitions : (state * symbol * state) list;
			acceptStates : state list
		}
		|zzz}	(* please, do not change this line *)
			symbolTypeName

	let example : JSon.t =
		JSon.parse {|
		{
			kind : "finite automaton",
			description : "this is an example",
			name : "example",
			alphabet: ["w", "z"],
			states : ["START", "X", "Z"],
			initialState : "START",
			transitions : [
				["START", "w", "X"], ["X", "z", "X"]
			],
			acceptStates : ["Z"]
		}
		|}	(* please, do not change this line *)
end
