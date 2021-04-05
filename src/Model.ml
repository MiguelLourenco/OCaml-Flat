(*
 * Model.ml
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
 * mar/2021 (amd) - Added support for semantic constrains (properties) in the exercices.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Abstract FLAT model.
 *
 * TODO: Probably add a new method "canonical" to generate a
 * normalized/simplified version of the FLAT model.
 *)

module type ModelSig =
sig
	class virtual model :
		'r Arg.alternatives -> string ->
			object
				method kind : string
				method description : string
				method name : string
				method errors : string list
				method handleErrors : unit
				method virtual validate : unit
				method virtual toJSon: JSon.t

				method virtual accept : word -> bool
				method virtual generate : int -> words
				method virtual tracing : unit
				
				method checkProperty : string -> bool
				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties
			end
end

module Model : ModelSig
 =
struct
open Exercise

	class virtual model (arg: 'r Arg.alternatives) (expectedKind: string) =
		object(self) inherit Entity.entity arg expectedKind
			method virtual validate: unit
			method virtual accept: word -> bool
			method virtual generate: int -> words
			method virtual tracing: unit
			
			method checkProperty (prop: string) =
				match prop with
					| "fail" | "false" -> false
					| "true" | "none" -> true
					| _ ->
						let mesg = "checkProperty: unknown property ("
										^ prop ^ ")" in
							failwith mesg
			
			method checkExercise (exercise: exercise) =
				let rep = exercise#representation in
					   Set.for_all self#accept rep.inside
					&& Set.for_all (fun w -> not (self#accept w)) rep.outside
					&& Set.for_all self#checkProperty rep.properties
						
			method checkExerciseFailures (exercise: exercise) =
				let rep = exercise#representation in
				(
					Set.filter (fun w -> not (self#accept w)) rep.inside,
					Set.filter self#accept rep.outside,
					Set.filter (fun w -> not (self#checkProperty w)) rep.properties
				)

	end

end
