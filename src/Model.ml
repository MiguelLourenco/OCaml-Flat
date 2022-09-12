(*
 * Model.ml
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
 * jul/2021 (amd) - Improved Learn-OCaml support.
 * mar/2021 (amd) - Added support for semantic constrains (properties) in
 *                  the exercises, in this class and in all its subclasses.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Abstract FLAT model.
 *
 * TODO: Probably add a new method "canonical" to generate a
 * normalized/simplified version of the FLAT model.
 *)

open BasicTypes

module type ModelSig =
sig
	class virtual model :
		('r,'x) Arg.alternatives -> string ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method virtual validate : unit

				method virtual accept : word -> bool
				method virtual generate : int -> words
				method virtual tracing : unit
				
				method checkProperty : string -> bool
				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties

			(* Learn-OCaml support *)
				method virtual moduleName : string
				method virtual xTypeName : string
				method virtual xTypeDeclString : string
				method virtual toDisplayString : string -> string
				method virtual example : JSon.t
			end
end

module Model : ModelSig
 =
struct
	class virtual model (arg: ('r,'x) Arg.alternatives) (expectedKind: string) =
		object(self) inherit Entity.entity arg expectedKind
		
			method virtual validate: unit
			method virtual accept: word -> bool
			method virtual generate: int -> words
			method virtual tracing: unit
			
			method checkProperty (prop: string) =
				match prop with
					| "fail" | "false" -> false
					| "true" -> true
					| _ ->
						let mesg = "checkProperty: unknown property ("
										^ prop ^ ")" in
							failwith mesg
			
			method checkExercise (exercise: Exercise.exercise) =
				let rep = exercise#representation in
					   Set.for_all self#accept rep.inside
					&& Set.for_all (fun w -> not (self#accept w)) rep.outside
					&& Set.for_all self#checkProperty rep.properties
						
			method checkExerciseFailures (exercise: Exercise.exercise) =
				let rep = exercise#representation in
				(
					Set.filter (fun w -> not (self#accept w)) rep.inside,
					Set.filter self#accept rep.outside,
					Set.filter (fun w -> not (self#checkProperty w)) rep.properties
				)

		(* Learn-OCaml support *)
			method virtual moduleName: string
			method virtual xTypeName: string
			method virtual xTypeDeclString : string
			method virtual toDisplayString: string -> string
			method virtual example : JSon.t
	end

end
