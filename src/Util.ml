(*
 * Util.ml
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
 * may/2021 (amd) - Lots of miscellaneous new stuff.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Miscellaneous utility functions. 
 *
 * TODO: Check if this is the right place for some of this functions.
 *)
 
open BasicTypes

module type UtilSig =
sig
	val stripChars : string -> string -> string
	val stripHead : string -> string

	val flatMap:  ('a -> 'b list) -> 'a list -> 'b list
	val concatAll : 'a list -> 'a list list -> 'a list list
	val distrib2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
	val indexOf : 'a -> 'a list -> int
	val fixedPoint : ('a -> 'a) -> 'a -> 'a
	
	val loadFile : string -> string
	val print : string list -> unit
	val println : string list -> unit
	val header : string -> unit
	val printAlphabet : symbols -> unit
	val printStates : states -> unit
	val printTransition : string -> symbol -> string -> unit
	val printWords : words -> unit
	val printStrings : string set -> unit
	val show : string -> unit

	val handleHomeDir : string -> string
	val testing : bool -> string -> bool
end

module Util : UtilSig =
struct
	let stripChars s cs =
		let len = String.length s in
		let j = ref 0 in
		let res = Bytes.create len in
			for i = 0 to len-1 do
				if not (String.contains cs s.[i]) then begin
					Bytes.set res !j s.[i];
					j := !j + 1
				end
			done;
			Bytes.to_string (Bytes.sub res 0 !j)

	let stripHead s =
		let len = String.length s in
		let n = ref 0 in
		let skip = ref (-1) in
		let j = ref 0 in
		let res = Bytes.create len in
			for i = 1 to len-1 do
				if !skip < 0 then begin
					if s.[i] = '\t' then
						n := !n + 1
					else
						skip := 0
				end;
				if !skip >= 0 then begin
					if !skip > 0 && s.[i] = '\t' then
						skip := !skip - 1
					else begin
						if s.[i] = '\n' then
							skip := !n
						else ();
						Bytes.set res !j s.[i];
						j := !j + 1
					end
				end
			done;
			Bytes.to_string (Bytes.sub res 0 !j)
		
	let flatMap f l =
		List.flatten (List.map f l)

	let addAll symb =
		List.map (fun l -> symb::l)

	let concatAll w =
		List.map (fun l -> w@l)

	let distrib2 f (a,b) =
		f a b

	let indexOf e l =
		let rec index e l n =
			match l with
				[] -> -1
				|x::xs -> if e = x then n else index e xs (n+1)
		in
		index e l 0

	let rec fixedPoint (f: 'a -> 'a) (x: 'a): 'a =
		let next = f x in
			if x = next then x
			else fixedPoint f next
			
			
	let loadFile (filename: string): string =
		try
			let ic = open_in filename in
			let n = in_channel_length ic in
			let s = Bytes.create n in
				really_input ic s 0 n;
				close_in ic;
				Bytes.to_string s
		with
			Sys_error str ->
				Error.error "file" str ""


	let rec print (l: string list) =
		match l with
		| [] -> ()
		| x::xs -> print_string x; print xs

	let println (l: string list) =
		print l ;
		print_newline()

	let header (str: string) =
		println ["------------------------------------------------"] ;
		println [str]

	let printAlphabet (a: symbols) =
		Set.iter (fun x -> print [symb2str x; ", "]) a;
		println []

	let printStates (st:states) =
		Set.iter (fun x -> print [state2str x; ", "]) st;
		println []

	let printTransition (a:string) (b:symbol) (c:string) =
		println ["("; a; ", "; symb2str b; ", "; c; ")"]

	let printWord (w:word) =
		println ["'"; word2str w; "'"]

	let printWords (s: words) =
		Set.iter printWord s
		
	let printString (s: string) =
		println ["'"; s; "'"]

	let printStrings (s: string set) =
		Set.iter printString s
		
	let show s =
		print_string ("|" ^ s ^ "|\n")
		
	let handleHomeDir s =
		match String.length s with
		| 0 ->
			""
		| 1 ->
			if s = "~" then Sys.getenv("HOME") else s
		| n ->
			if s.[0] = '~' then
				if s.[1] = '/' then
					Sys.getenv("HOME") ^ String.sub s 1 (n - 1)
				else
					"/home/" ^ String.sub s 1 (n - 1)
			else s

	let testing active moduleName =
		let forceActive = false in
		let regularActive = (active && try ignore (Sys.getenv("TESTING")); true with _ -> false) in
		let active = forceActive || regularActive in
			if active then
				header ("### Testing " ^ moduleName ^ " ###");
			active
end

module UtilTests =
struct
	let active = false

	let test0 () =
		Util.println [Util.loadFile "examples/fa_abc.json"]

	let test1 () =
		let a = word2str [symb "e";symb "r";symb "t"] in
		let b = word2str [symb "4";symb "5";symb "y"] in
			Util.println [a; b]

	let runAll : unit =
		if Util.testing active "Util" then begin
			test0 ();
			test1 ()
		end
end
