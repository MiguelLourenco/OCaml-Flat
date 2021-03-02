(*
 * Scanner.ml
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
 * jan/2021 (amd) - Initial version, for the JSon parser.
 *)

(*
 * Description: Simple lexical analyzer that assumes that the tokens are
 * the individual non-blank characters. The function getToken is available
 * to handle the rare cases where we need a multi-char token. This module is
 * suitable for the three parsers defined in the OCamlFlat library.
 * The tokens are handled in a imperative style to simplify the signature of
 * of the client parsing functions.
 *)

module type ScannerSig =
sig
	val start : string -> string -> string list ref
	val skip : unit -> unit
	val curr : unit -> char
	val getToken : (char -> bool) -> string
	val expecting : string -> char -> 'a -> 'a
	val stop : unit -> unit
end

module Scanner : ScannerSig =
struct
	let parserName = ref ""
	let inputString = ref ""
	let inputStringLength = ref 0
	let inputStringPosition = ref 0

	let start name s =
		parserName := name;
		inputString := s;
		inputStringLength := String.length s;
		inputStringPosition := 0;
		let log = Error.start () in
			Error.onlyFirst ();
			log

	let isInside () =
		!inputStringPosition < !inputStringLength

	let getThis () =
		String.get !inputString !inputStringPosition

	let skip () =
		inputStringPosition := !inputStringPosition + 1

	let skipWhile good =
		while isInside () && good (getThis ()) do skip() done

	let curr () =
		skipWhile (fun c -> c = ' ' || c = '\t' || c = '\n');
		if isInside () then
			getThis ()
		else
			' '

	let getToken good =
		let start = !inputStringPosition in
			skipWhile good;
			String.sub !inputString start (!inputStringPosition - start)

	let expecting exp got z =
		let g = if got = ' ' then "'EOF'" else "'" ^ Char.escaped got ^ "'" in
		let mesg ="Expecting " ^ exp ^ ", got " ^ g in
			Error.error !parserName mesg z

	let stop () =
		Error.show !parserName "input";
		Error.stop ()

end
