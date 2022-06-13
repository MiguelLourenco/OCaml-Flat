(*
 * BasicTypes.ml
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
 * apr/2022 (amd) - The type 'symbol' is now opaque type an can be internally
 *						represented using a char or a string. Required several changes
 *						all over the code of several modules.
 * mar/2021 (amd) - New types 'property', 'properties'.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Some types and constants used across all the modules.
 *)
module type BasicTypesSig =
sig
	type 'a set = 'a Set.t
	
	(* SYMBOLS *)
	type symbol (* opaque *)
	type symbols = symbol set
	type variable = symbol
	type variables = variable set
	
	val char2symb : char -> symbol
	val str2symb : string -> symbol
	val symb2str : symbol -> string
	val symb : string -> symbol
	val epsilon : symbol
	val dollar : symbol
	val draftVar : variable
	val symbols : string -> symbols
	
	(* WORDS *)
	type word = symbol list
	type words = word set
	val str2word : string -> word
	val word : string -> word
	val word2str : word -> string
	val strings2words : string list -> word list
	val words2strings : word list -> string list
	
	(* STATES *)
	(* type state *) (* opaque *)
	type state = string
	type states = state set
	val state2str : state -> string
	val str2state : string -> state
	val state : string -> state
	val draftState: state
	
	(* PROPERTIES *)	
	type property = string
	type properties = property set

	(* OTHER *)	
	type direction = L | R
end

module BasicTypes : BasicTypesSig =
struct
	type 'a set = 'a Set.t

	(*
	type symbol = string
	let char2symb c: symbol = Char.escaped c
	let str2symb s: symbol = s
	let symb2str s: string = s
	*)

	(* SYMBOLS *)
	
	type symbol = char
	let char2symb c: symbol = c
	let str2symb s: symbol = if String.length s = 1 then String.get s 0 else '?'
	let symb2str s: string = Char.escaped s	
	
	let symb s: symbol  = str2symb s	
	
	type symbols = symbol set
	
	let epsilon: symbol = symb "~" (* used for representing the empty transitions *)
	let dollar: symbol = symb "$"

	type variable = symbol
	type variables = variable set

	let draftVar: variable = symb "_"

	(* WORDS *)

	type word = symbol list
	type words = word set
	
	(* let str2word s =      //only ocaml >= 4.06
		List.init (String.length s) (String.get s) ; *)
	let str2word (s:string): word =
		let n = String.length s in
		let rec iterStr i =
			if i < n then (char2symb s.[i])::iterStr (i+1)
			else []
		in
			iterStr 0

	let word s = str2word s

	let word2str (w:word): string =
		let strs = List.map symb2str w in
			String.concat "" strs

	let strings2words ss =
		List.map str2word ss
		
	let words2strings ws =
		List.map word2str ws

	let symbols s = Set.make (word s)

	(* STATES *)
	
	type state = string
	type states = state set

	let state2str s: string = s

	let str2state s: state = s
	let state s = str2state s

	let draftState: state = state "_"

	(* PROPERTIES *)	
	
	type property = string
	type properties = property set

	(* OTHER *)	
	type direction = L | R
end
