(*
 * Exercise.ml
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
 * jul/2021 (amd) - Improved error handling.
 * mar/2021 (amd) - Added semantic constrains (properties) to the exercises.
 * jan/2021 (amd) - Module in an independent file.
 * set/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml"
 *)

(*
 * Description: Support to pedagogical exercises. The solutions are validated
 * using unit tests.
 *)

open BasicTypes

module ExerTypes =
struct
	type tx =
		unit
	type t = {
		problem : string;
		inside : words;
		outside : words;
		properties : properties
	}
end

module type ExerciseSig =
sig
	open ExerTypes

	class exercise :
		(t,t) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon : JSon.t
				method representation : t
				method validate : unit
				
				method tracing : unit
				
				method moduleName : string
			end
end

module Exercise : ExerciseSig =
struct
	open ExerTypes
	
	let fromJSon j =
		if JSon.isNull j || not (JSon.hasField j "kind") then {
			problem = "_";
			inside = Set.empty;
			outside = Set.empty;
			properties = Set.empty
		}
		else {
			problem = JSon.fieldString j "problem";
			inside = Set.map str2word (JSon.fieldStringSet j "inside");
			outside = Set.map str2word (JSon.fieldStringSet j "outside");
			properties = JSon.fieldStringSet j "properties"
		}
	
	let toJSon (rep: t): JSon.t =
		JSon.makeAssoc [
			("problem", JSon.makeString rep.problem);
			("inside", JSon.makeStringSet (Set.map word2str rep.inside));
			("outside", JSon.makeStringSet (Set.map word2str rep.outside));
			("properties", JSon.makeStringSet rep.properties)
		]

	class exercise (arg: (t,t) Arg.alternatives ) =
		object(self) inherit Entity.entity arg "exercise" as super

			val representation: t =
				match arg with
					| Arg.Representation r -> r
					| Arg.RepresentationX r -> r
					| _ -> fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation =
				representation

			method validate = ()
			
			method toJSon: JSon.t =
				JSon.append (super#toJSon) (toJSon representation)

			method tracing = ()
			
			method moduleName =
				"Exercice"
	end
end

module ExerciseTests : sig end =
struct
	let active = false

	let test0 () =
		let e = new Exercise.exercise (Arg.Predef "exer_balanced") in
		let je = e#toJSon in
			JSon.show je

	let runAll =
		if Util.testing active "Exercices" then begin
			test0 ()
		end
end


