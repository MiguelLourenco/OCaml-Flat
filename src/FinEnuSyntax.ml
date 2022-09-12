(*
 * FinEnuSyntax.ml
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
 * Description: Support types and functions for FEs.
 *)

open BasicTypes

module FinEnuTypes =
struct
	type tx =
		string list
	type finiteEnumeration =
		tx
	type t =
		words
	type fe =
		t
end

module FinEnuConversions =
struct
	open FinEnuTypes

	let internalize (fe: tx): t =
		Set.make (strs2words fe)

	let externalize (fe: t): tx =
		words2strs (Set.toList fe)

	let fromJSon j =
		if JSon.isNull j || not (JSon.hasField j "kind") then
			Set.empty
		else
			let strings = JSon.fieldStringSet j "words" in
			let words = Set.map str2word strings in
				words
	
	let toJSon (id: JSon.t) (rep: t): JSon.t =
		let body =
			JSon.makeAssoc [
				("words", JSon.makeStringSet (Set.map word2str rep))
			]
		in JSon.append id body
end

module FinEnuForLearnOCaml =
struct
	open FinEnuTypes

	let moduleName =
		"FiniteEnumeration"

	let xTypeName =
		"finiteEnumeration"

	let solution (name: string) (repx: tx): string =
		Printf.sprintf {zzz|
		%s	%s
		|zzz}	(* please, do not change this line *)
			(FinAutForLearnOCaml.displayHeader name xTypeName)
			(strings2display repx)

	let prelude : string = {| {
		type symbol = char
		type state = string
		type transition = state * symbol * state

		type finiteAutomaton = {
			alphabet : symbol list;
			states : state list;
			initialState : state;
			transitions : transition list;
			acceptStates : state list
		}
		|}	(* please, do not change this line *)

	let example : JSon.t =
		JSon.parse {| {
			kind : "finite enumeration",
			description : "this is an example",
			name : "example",
			words : ["Red", "Yellow", "Blue"]
		}
		|}	(* please, do not change this line *)

end
