(*
 * BasicTypes.ml
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

#define SYMBOL_IS_CHAR 1

module type BasicTypesSig =
sig
	type 'a set = 'a Set.t
	
	type symbol (* opaque *)
#if SYMBOL_IS_CHAR > 0
	type symbolX = char
#else
	type symbolX = string
#endif
	type symbols = symbol set
	type variable = symbol
	type variableX = symbolX
	type variables = variable set
	
	val char2symb : char -> symbol
	val symb2char : symbol -> char
	val str2symb : string -> symbol
	val symb2str : symbol -> string
	val symb2symbX : symbol -> symbolX
	val symbX2symb : symbolX -> symbol
	val symb : string -> symbol

	val symbols2symbolsX : symbols -> symbolX list
	val symbolsX2symbols : symbolX list -> symbols

	val epsilon : symbol
	val dollar : symbol
	val draftVar : variable
	val symbolTypeName : string
	val empty: symbol

	(* WORDS *)
	type word = symbol list
	type words = word set
	val str2word : string -> word
	val word : string -> word
	val word2str : word -> string
	val strs2words : string list -> word list
	val words2strs : word list -> string list
	val symbols : string -> symbols
	
	(* STATES *)
	(* type state *) (* opaque *)
	type state = string
	type states = state set
	val state2str : state -> string
	val str2state : string -> state
	val state : string -> state
	val draftState: state

	(* TRANSITIONS3 *)	
	type transition3 = state * symbol * state
	type transition3X = state * symbolX * state
	type transitions3 = transition3 set

	val transsX2transs3 : transition3X list -> transitions3
	val transs2transsX3 : transitions3 -> transition3X list

	(* PROPERTIES *)	
	type property = string
	type properties = property set

	(* OTHER *)	
	type direction = L | R

	val direction2string: direction -> string
	val string2direction: string -> direction

	val char2direction: char -> direction

	val stringIsDirection: char -> bool

	(* DISPLAY *)	
	val str2display : string -> string
	val symb2display : symbol ->  string
	val symbX2display : symbolX ->  string
	val state2display : state -> string

	val symbols2display : symbols ->  string
	val symbolsX2display : symbolX list ->  string
	val statesX2display : state list ->  string
	val strings2display : string list ->  string
	val transsX2display3 : transition3X list -> string
end

module BasicTypes : BasicTypesSig =
struct
	type 'a set = 'a Set.t
	
#if SYMBOL_IS_CHAR > 0
	type symbol = char
	type symbolX = char
	let symbolTypeName = "char"
#else
	type symbol = string
	type symbolX = string
	let symbolTypeName = "string"
#endif
	type symbols = symbol set
	type variable = symbol
	type variableX = symbolX
	type variables = variable set
	
#if SYMBOL_IS_CHAR > 0
	let symbDisplayQuote = "'" 
	let char2symb c: symbol = c
	let symb2char s: char = s
	let str2symb s: symbol = if String.length s > 0 then String.get s 0 else '?'
	let symb2str s: string = Char.escaped s	
#else
	let symbDisplayQuote = "\"" 
	let char2symb c: symbol = Char.escaped c
	let symb2char s: char = if String.length s > 0 then String.get s 0 else '?'
	let str2symb s: symbol = s
	let symb2str s: string = s
#endif

	let symb2symbX (s: symbol): symbolX = s
	let symbX2symb (x: symbolX): symbol = x
	let symb (s: string): symbol  = str2symb s

	let symbols2symbolsX ss: symbolX list =
		List.map symb2symbX (Set.toList ss)
	let symbolsX2symbols ss: symbols =
		Set.make (List.map symbX2symb ss)
	

	let epsilon: symbol = symb "~" (* used for representing the empty transitions *)
	let dollar: symbol = symb "$"
	let draftVar: variable = symb "_"
	let nothing: symbol = symb "_"
	let empty: symbol = symb "B"

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

	let strs2words ss =
		List.map str2word ss
		
	let words2strs ws =
		List.map word2str ws
		
	let symbols s =
		Set.make (word s)

	(* STATES *)
	
	type state = string
	type states = state set

	let state2str s: string = s

	let str2state s: state = s
	let state s = str2state s

	let draftState: state = state "_"

	(* TRANSITIONS3 *)
		
	type transition3 = state * symbol * state
	type transition3X = state * symbolX * state
	type transitions3 = transition3 set

	let transX2trans3 (a,b,c): transition3 =
		(a, symbX2symb b, c)
	let transs2transsX3 s: transition3X list =
		List.map transX2trans3 (Set.toList s)
	let transsX2transs3 l: transitions3 =
		Set.make (List.map transX2trans3 l)

	(* PROPERTIES *)	
	
	type property = string
	type properties = property set

	(* OTHER *)
	
	type direction = L | R
	type 'c trail = 'c set list
	type 'c path = 'c list

	let direction2string dir : string = if dir = L then "L" else "R"
	let string2direction dirS : direction = if dirS = "L" then L else R 

	let char2direction dirC : direction = if dirC = 'L' then L else R 

	let stringIsDirection dirC: bool = if dirC = 'L' || dirC = 'R' then true else false

	(* DISPLAY *)

	let str2display s: string = "\"" ^ s ^ "\""
	let state2display s: string = "\"" ^ (state2str s) ^ "\""
	let symb2display s: string = symbDisplayQuote ^ (symb2str s) ^ symbDisplayQuote
	let symbX2display s: string = symb2display s

	let list2display f l =
		let l = List.map f l in
		let core = String.concat "; " l in
			"[" ^ core ^ "]"	
	let symbolsX2display l: string = list2display symb2display l
	let symbols2display s: string = list2display symbX2display (Set.toList s)
	let statesX2display l = list2display state2display l
	let strings2display l = list2display str2display l
	let transsX2display3 l =
		let t2d (a,b,c) =
			Printf.sprintf "(%s, %s, %s)" (state2display a) (symbX2display b) (state2display c)
		in list2display t2d l
end
