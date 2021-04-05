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
 * mar/2021 (amd) - New module
 *)

(*
 * Description: Finite languagem directly defined as a set of words.
 *)


module FiniteEnumeration =
struct

	type t = words

	let modelDesignation = "finite enumeration"

	class model (arg: t Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super
		
			val representation: t =
				let j = Arg.fromAlternatives arg in
					if j = JSon.JNull then
						Arg.getRepresentation arg
					else
						let strings = JSon.field_string_set j "words" in
						let words = Set.map Util.str2word strings in
							words

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation =
				representation

			method toJSon: JSon.t =
				let open JSon in
				let rep = representation in
				let strings = Util.words2strings (Set.toList rep) in
				JAssoc [
					("kind", JString self#kind);
					("description", JString self#description);
					("name", JString self#name);
					("words", JList (List.map (fun s -> JString s) strings))
				]

			method validate: unit = ()

			method tracing : unit = ()

			method accept (w: word): bool =
				Set.belongs w representation

			method generate (length: int): words =
				Set.filter (fun w -> List.length w == length) representation

			method checkProperty (prop: string) =
				match prop with
					| _ -> super#checkProperty prop
		end

end

module FiniteEnumerationTests : sig end =
struct
	let active = true
	
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

