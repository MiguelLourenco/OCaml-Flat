(*
 * FiniteEnumeration.ml
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
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Improved Learn-OCaml support and error handling.
 * may/2021 (amd) - Added support for an extern representation.
 * mar/2021 (amd) - New module
 *)

(*
 * Description: Finite language, directly defined as a set of words.
 *)

open BasicTypes

module FiniteEnumeration =
struct
	open FinEnuTypes

	let modelDesignation = "finite enumeration"

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super
		
			val representation: t =
				match arg with
					| Arg.Representation r -> r
					| Arg.RepresentationX r -> FinEnuConversions.internalize r
					| _ -> FinEnuConversions.fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation: t =
				representation

			method representationx: tx =
				FinEnuConversions.externalize representation

			method toJSon: JSon.t =
				FinEnuConversions.toJSon (super#toJSon) representation

			method validate: unit = ()

			method tracing : unit = ()

			method accept (w: word): bool =
				Set.belongs w representation

			method generate (length: int): words =
				Set.filter (fun w -> List.length w == length) representation

			method checkProperty (prop: string) =
				match prop with
					| "finite enumeration" -> true
					| _ -> super#checkProperty prop

		(* Learn-OCaml support *)
			method moduleName = FinEnuForLearnOCaml.moduleName
			method xTypeName = FinEnuForLearnOCaml.xTypeName
			method xTypeDeclString : string = FinEnuForLearnOCaml.prelude
			method toDisplayString (name: string): string =
				FinEnuForLearnOCaml.solution name self#representationx
			method example : JSon.t = FinEnuForLearnOCaml.example
		end
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
		kind : "exercise",
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
			Util.printWords ins;
			Util.printWords outs;
			Util.printStrings props
		
	let runAll =
		if Util.testing active "FiniteEnumeration" then begin
			test0 ();
		end
end

