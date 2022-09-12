(*
 * Error.ml
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
 * jul/2021 (amd) - Simplified module.
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
	val start : unit -> unit
	val error : string -> string -> 'a -> 'a
	val get: unit -> string list
	val show : string -> string -> unit
end

module Error : ErrorSig =
struct

	let showImmediately =
		false (* for debugging *)

	let errors: string list ref =
		ref []

	let start () =
		errors := []

	let makeMesg (culprit: string) (str: string) =
		if culprit = "_" then "" ^ str
		else "\"" ^ culprit ^ "\": " ^ str

	let printMesg (mesg: string) =
		print_string "	";
		print_string mesg;
		print_string "\n"

	let debugMesg (mesg: string) =
		if showImmediately && Configuration.diagnosticsOn () then
			printMesg ("==> "^mesg)

	let error (culprit: string) (str: string) (res: 'a): 'a =
		let mesg = makeMesg culprit str in
			errors := !errors @ [mesg];
			debugMesg mesg;
			res

	let get (): string list =
		!errors

	let show (expectedKind: string) (name: string): unit =
		if !errors <> [] && Configuration.diagnosticsOn () then begin
			print_string (expectedKind^" \""^name^ "\" has errors:\n");
			List.iter printMesg (!errors)
		end
end
