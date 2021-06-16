(*
 * FiniteEnumeration.ml
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
 * may/2021 (amd) - Added support for an extern representation.
 * mar/2021 (amd) - New module
 *)

(*
 * Description: Finite language, directly defined as a set of words.
 *)


module FiniteEnumeration =
struct

	type tx = string list

	type t = words

	let modelDesignation = "finite enumeration"

	let internalize (fe: tx): t =
		Set.make (Util.strings2words fe)

	let externalize (fe: t): tx =
		Util.words2strings (Set.toList fe)

	let fromJSon j =
		let strings = JSon.field_string_set j "words" in
		let words = Set.map Util.str2word strings in
			words
	
	let toJSon (rep: t): JSon.t =
		let open JSon in
		let strings = Util.words2strings (Set.toList rep) in
		JAssoc [
			("words", JList (List.map (fun s -> JString s) strings))
		]

	let displayHeader (name: string) (moduleName: string) =
		if name = "" then
			""
		else
			("let " ^ name ^ ": " ^ moduleName ^ ".tx =\n\t\t")

	let toDisplayString (name: string) (moduleName: string) (repx: tx): string =
		Printf.sprintf {zzz|
		%s	%s
		|zzz}
			(displayHeader name moduleName)
			(Util.stringList2DisplayString repx)

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super
		
			val representation: t =
				match arg with
					| Arg.Representation r -> r
					| Arg.RepresentationX r -> internalize r
					| _ -> fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation: t =
				representation

			method representationx: tx =
				externalize representation

			method toJSon: JSon.t =
				JSon.append (super#toJSon) (toJSon representation)

			method validate: unit = ()

			method example : JSon.t = 
				JSon.from_string {| {
					kind : "finite enumeration",
					description : "this is an example",
					name : "example",
					words : ["Red", "Yellow", "Blue"]
				} |}

			method tracing : unit = ()

			method accept (w: word): bool =
				Set.belongs w representation

			method generate (length: int): words =
				Set.filter (fun w -> List.length w == length) representation

			method checkProperty (prop: string) =
				match prop with
					| "finite enumeration" -> true
					| _ -> super#checkProperty prop

			method moduleName =
				"FiniteEnumeration"

			method toDisplayString (name: string): string =
				toDisplayString name self#moduleName self#representationx

		end
		
		let register solution =
			let model = new model (Arg.RepresentationX solution) in
				(fun x -> model#accept (Util.str2word x))
end

module FiniteEnumerationTests : sig end =
struct
	let active = false
	
	let fe_colors = {| {
		kind : "finite enumeration",
		description : "this is an example",
		name : "colors",
		words : ["Red", "Yellow", "Blue"]
	} |}
	
	let exer_colors = {| {
		kind : "exercice",
		description : "this is an example",
		name : "exer_colors",
		problem : "Colors of length 3",
		inside : ["Red"],
		outside : ["","Yellow","Blue"],
		properties : ["true"]
	} |}

	let test0 () =
		let fe = new FiniteEnumeration.model (Arg.Text fe_colors) in
		let e = new Exercise.exercise (Arg.Text exer_colors) in
		let (ins,outs,props) = fe#checkExerciseFailures e in	
			Util.printWords (Set.toList ins);
			Util.printWords (Set.toList outs);
			Util.printStrings (Set.toList props)
		
	let runAll =
		if active then (
			Util.header "FiniteEnumerationTests";
			test0 ();

		)
end

