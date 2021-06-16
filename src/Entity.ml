(*
 * Entity.ml
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
 * may/2021 (amd) - Added support for an extern representation.
 * may/2021 (amd) - Centralized the handling of kind/description/name.
 * feb/2021 (amd) - Added the alternative Predef.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: An entity is a named instance of a concept. As now, the entities
 * are the exercises and the FLAT models. The type "alternatives" is to allow
 * the constructor to be used with several kind of parameter forms.
 *)

module Arg =
struct	
	type ('r, 'x) alternatives =
		| JSon of JSon.t
		| Text of string
		| File of string
		| Predef of string
		| Representation of 'r
		| RepresentationX of 'x

	let fromAlternatives alt =
		match alt with
			| JSon j -> j
			| Text str -> JSon.from_string str
			| File str -> JSon.from_file str
			| Predef str -> JSon.from_string (Examples.example str)
			| _ -> JSon.JNull
end

module Entity =
struct

	type t = {
		kind : string;
		description : string;
		name : string
	}

	let fromJSon (j: JSon.t): t =
		let open JSon in {
			kind = field_string j "kind";
			description = field_string j "description";
			name = field_string j "name"
		}
		
	let toJSon (rep: t): JSon.t =
		let open JSon in
		JAssoc [
			("kind", JString rep.kind);
			("description", JString rep.description);
			("name", JString rep.name)
		]
	
	let dummyId (k: string): t = {
		kind = k;
		description = "_";
		name = "_"
	}
		
	class virtual entity (arg: ('r,'x) Arg.alternatives) (expectedKind: string) =
		object(self)
			
			val errors = Error.start ()
		
			val id: t =
				match arg with
				| Arg.Representation r -> dummyId expectedKind
				| Arg.RepresentationX r -> dummyId expectedKind
				| _ -> fromJSon (Arg.fromAlternatives arg)
		
			method id: t =
				id
			
			method errors : string list =
				!errors
			
			method virtual validate: unit
			
			method toJSon =
				toJSon id
			
			method handleErrors = (
				if id.kind <> expectedKind then
					Error.error id.kind "Wrong kind" ();
				self#validate;
				if Configuration.diagnosticsOn () then
					Error.show expectedKind id.name;
				Error.stop ()
			)
			
			method moduleName =
				"Entity"
	end
end

module EntityTests : sig end =
struct
	let active = false

	let test0 () =
		()

	let runAll =
		if active then
			Util.header "EntityTests";
			test0 ()
end

