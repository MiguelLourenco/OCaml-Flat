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
 * set/2022 (amd) - Full restructuration.
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

module ModelBasics =
struct
end

module ModelExercises =
struct
	let checkProperty (prop: string) =
		match prop with
			| "fail" | "false" -> false
			| "true" -> true
			| _ ->
				let mesg = "checkProperty: unknown property ("
								^ prop ^ ")" in
					failwith mesg

	let checkExercise (ex: ExerTypes.t) accept checkProperty =
		   Set.for_all accept ex.inside
		&& Set.for_all (fun w -> not (accept w)) ex.outside
		&& Set.for_all checkProperty ex.properties

	let checkExerciseFailures (ex: ExerTypes.t) ac cp = (
		Set.filter (fun w -> not (ac w)) ex.inside,
		Set.filter ac ex.outside,
		Set.filter (fun w -> not (cp w)) ex.properties
	)
end

module ModelSupport =
struct
	include ModelBasics
	include ModelExercises
end

module ModelPrivate =
struct
	(* semi-algoritm *)
	let accept (m: 'm) (w: word)
				(initial: 'm -> word -> 'c)
				(next: 'm -> 'c -> 'c set)
				(isAccepting: 'm -> 'c -> bool): bool =

		let rec acceptX (configs: 'c set) (seen: 'c set): bool =
			let newConfigs = Set.diff configs seen in				(* select the new *)
			let seen = Set.unionUnsafe newConfigs seen in			(* build new seen *)
			if Set.isEmpty newConfigs then false				(* no new configs - rejected *)
			else if Set.exists (isAccepting m) newConfigs then true	(* found accepted configuration *)
			else
				let nextConfigs = Set.flatMap (next m) newConfigs in
					acceptX nextConfigs seen
		in	
		let _ = RuntimeControl.start 0 in
		let initialConfigs = Set.make [initial m w] in
			acceptX initialConfigs Set.empty

	(* semi-algoritm *)
	(* invariant - for_all c in seen: c <= len *)
	let generate (m: 'm) (len: int)
				(initial: 'm -> word -> 'c)
				(next2: 'm -> 'c -> 'c set)
				(isAccepting: 'm -> 'c -> bool)
				(getWord: 'c -> word): words =

		let strict = len < 0 in
		let len = abs len in
		let lenWord c = List.length (getWord c) in
		let isNew seen c = lenWord c <= len && not (Set.belongs c seen) in
		let isExpanding c = lenWord c < len || not (isAccepting m c) in
		let finalSelection =
			if strict then (fun c -> isAccepting m c && lenWord c = len) 
					else (fun c -> isAccepting m c) 
		in
		let rec generateX (configs: 'c set) (seen: 'c set): 'c set = (* configs -> toExpand *)
			let newConfigs = Set.filter (isNew seen) configs in		(* select the new *)
			let seen = Set.unionUnsafe newConfigs seen in			(* build new seen *)
			let toExpand = Set.filter isExpanding newConfigs in		(* select to expand *)
				if Set.isEmpty toExpand || RuntimeControl.giveUp (Set.size seen) then
					seen
				else
					let nextConfigs = Set.flatMap (next2 m) toExpand in
						generateX nextConfigs seen
		in

		let _ = RuntimeControl.start 0 in
		let initialConfigs = Set.make [initial m (word "")] in
		let collected = generateX initialConfigs Set.empty in
		let selected = Set.filter finalSelection collected in
			Set.map getWord selected
end

module Model =
struct
	include ModelSupport

	let accept = ModelPrivate.accept
	let generate = ModelPrivate.generate

	class virtual model (arg: ('r,'x) Arg.alternatives) (expectedKind: string) =
		object(self) inherit Entity.entity arg expectedKind

			method virtual accept: word -> bool
			method virtual generate: int -> words
			
		(* Exercices support *)
			method checkProperty (prop: string) = checkProperty prop
			method checkExercise (exercise: Exercise.exercise) =
				checkExercise exercise#representation self#accept self#checkProperty
			method checkExerciseFailures (exercise: Exercise.exercise) =
				checkExerciseFailures exercise#representation self#accept self#checkProperty

		(* Learn-OCaml support *)
			method virtual moduleName: string
			method virtual xTypeName: string
			method virtual xTypeDeclString : string
			method virtual toDisplayString: string -> string
			method virtual example : JSon.t
	end

end
