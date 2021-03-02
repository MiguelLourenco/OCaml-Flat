(*
 * Error.ml
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
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Supports a log of errors. Probably, a text-based application
 * will use the error log differently from a graphical-based application.
 * The errors are handled in a imperative style to simplify the signature of
 * many functions - this modules implements a kind of error log monad.
 *)

module type ErrorSig =
sig
	val start : unit -> string list ref
	val stop : unit -> unit
	val onlyFirst : unit -> unit
	val error : string -> string -> 'a -> 'a
	val show : string -> string -> unit
end

module Error : ErrorSig =
struct
	type t = string list ref

	let sink: t = ref []
	let err: t ref = ref sink
	let onlyOne: bool ref = ref false

	let start () =	(* setup a log of errors *)
		let r = ref [] in
			err := r;
			r		(* returns a new error log *)

	let stop () = (* disables logging *)
		sink := [];
		err := sink

	let onlyFirst () =
		onlyOne := true

	let error (culprit: string) (str: string) (res: 'a): 'a =
		let r = !err in
		if !r = [] || not !onlyOne then
			r := !r@[culprit ^ ": " ^ str];
		res

	let show (expectedKind: string) (name: string): unit =
		let r = !err in
			if !r = [] then ()
			else (
				print_string (expectedKind^" "^name^ " has errors:\n");
				List.iter (fun m -> print_string ("	"^m^"\n")) !r
			)
end
