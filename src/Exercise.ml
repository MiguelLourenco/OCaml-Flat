(*
 * Exercise.ml
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
 * jan/2021 (amd) - Module in an independent file.
 * set/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml"
 *)

(*
 * Description: Support to pedagogical exercises. The solutions are validated
 * using unit tests.
 *
 * TODO: Improve with support for the validation of extra properties,
 * beyond unit testing.
 *)

module type ExerciseSig =
sig
	type t = { problem : string; inside : words; outside : words; }
	class exercise :
		t Arg.alternatives ->
			object
				method description : string
				method errors : string list
				method handleErrors : unit
				method kind : string
				method name : string
				method representation : t
				method toJSon : JSon.t
				method tracing : unit
				method validate : unit
			end
end

module Exercise : ExerciseSig =
struct

	type t = {
		problem : string;
		inside : words;
		outside : words
	}

	class exercise (arg: 'r Arg.alternatives ) =
		object(self) inherit Entity.entity arg "exercice"

			val representation: t =
				let j = Arg.fromAlternatives arg in
					if j = JSon.JNull then
						Arg.getRepresentation arg
					else
						let problem = JSon.field_string j "problem" in
						let inside = JSon.field_string_set j "inside" in
						let outside = JSon.field_string_set j "outside" in
						{
							problem = problem;
							inside = Set.map (fun s -> Util.str2word s) inside;
							outside = Set.map (fun s -> Util.str2word s) outside
						}

		initializer self#handleErrors	(* placement is crucial - after representation *)

		method representation =
				representation
		method validate = ()
		method toJSon: JSon.t =
			let open JSon in
			let rep = representation in
				JAssoc [
					("kind", JString self#kind);
					("description", JString self#description);
					("name", JString self#name);
					("problem", JString rep.problem );
					("inside", JList (List.map
									(fun w -> JString (Util.word2str w))
									(Set.toList rep.inside)));
					("outside", JList (List.map
									(fun w -> JString (Util.word2str w))
									(Set.toList rep.outside)))
				]
		method tracing = ()
	end
end

module ExerciseTests : sig end =
struct
let active = false

	let test0 () =
		let e = new Exercise.exercise (Arg.Predef "exer_re2fa") in
			let j = e#toJSon in
				JSon.show j

	let runAll =
		if active then (
			Util.header "ExercicesTests";
			test0 ()
		)
end


