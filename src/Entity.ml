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
	type 'r alternatives =
		| JSon of JSon.t
		| Text of string
		| File of string
		| Predef of string
		| Representation of 'r

	let fromAlternatives alt =
		match alt with
			| JSon j -> j
			| Text str -> JSon.from_string str
			| File str -> JSon.from_file str
			| Predef str -> JSon.from_string (Examples.example str)
			| Representation r -> JSon.JNull

	let getRepresentation alt =
		match alt with
			| Representation r -> r
			| _ -> failwith "getRepresentation"
end

module Entity =
struct
	class virtual entity (arg: 'r Arg.alternatives) (expectedKind: string) =
		let errors = Error.start () in
		let r = Arg.fromAlternatives arg in
		let j = if r <> JSon.JNull then r else JSon.makeDummyIdentification(expectedKind) in
		let (kind, description, name) = JSon.identification j in
		object(self)
			method kind: string = kind
			method description: string = description
			method name: string = name
			method errors : string list = !errors
			method virtual validate: unit
			method virtual toJSon: JSon.t
			method handleErrors = (
				if self#kind <> expectedKind then
					Error.error self#kind "Wrong kind" ();
				self#validate;
				if Configuration.diagnosticsOn () then
					Error.show expectedKind self#name;
				Error.stop ()
			)
	end
end

