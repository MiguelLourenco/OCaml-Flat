(*
 * Util.ml
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
 * Description: Miscellaneous utility functions. 
 *
 * TODO: Check if this is the right place for some of this functions.
 *)

module type UtilSig =
sig
	val ch2str : char -> string
	val str2word : string -> char list
	val word2str : char list -> string
	val strings2words : string list -> char list list
	val words2strings : char list list -> string list
	val stripChars : string -> string -> string
	val stripHead : string -> int -> string
	
	val flatMap:  ('a -> 'b list) -> 'a list -> 'b list
	val concatAll : 'a list -> 'a list list -> 'a list list
	val distrib2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
	val indexOf : 'a -> 'a list -> int
	val fixedPoint : ('a -> 'a) -> 'a -> 'a
	
	val load_file : string -> string
	val print : string list -> unit
	val println : string list -> unit
	val header : string -> unit
	val printAlphabet : char list -> unit
	val printStates : string list -> unit
	val printTransition : string -> char -> string -> unit
	val printWords : char list list -> unit
	val printStrings : string list -> unit
end

module Util : UtilSig =
struct
	let ch2str c =
		Char.escaped c

	(* let str2word s =      //only ocaml >= 4.06
		List.init (String.length s) (String.get s) ; *)
	let str2word s =
		let n = String.length s in
		let rec iterStr i =
			if i < n then s.[i]::iterStr (i+1)
			else []
		in
			iterStr 0

	let word2str w =
		let buf = Buffer.create 16 in
		let () = List.iter (Buffer.add_char buf) w in
			Buffer.contents buf

	let strings2words ss =
		List.map str2word ss
		
	let words2strings ws =
		List.map word2str ws

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

	let stripHead s n =
		let len = String.length s in
		let j = ref 0 in
		let skip = ref n in
		let res = Bytes.create len in
			for i = 1 to len-1 do
				if !skip > 0 && s.[i] = '\t' then
					skip := !skip - 1
				else begin
					if s.[i] = '\n' then
						skip := n
					else ();
					Bytes.set res !j s.[i];
					j := !j + 1
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
			
			
	let load_file (filename: string): string =
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
		println ["----------"] ;
		println [str]

	let printAlphabet (alf:char list) =
		List.iter (fun x -> print [ch2str x; ", "]) alf;
		println []

	let printStates (st:string list) =
		List.iter (fun x -> print [x; ", "]) st;
		println []

	let printTransition (a:string) (b:char) (c:string) =
		println ["("; a; ", "; ch2str b; ", "; c; ")"]

	let printWord (w:char list) =
		println ["'"; word2str w; "'"]

	let printWords (l: char list list) =
		List.iter printWord l
		
	let printString (s: string) =
		println ["'"; s; "'"]

	let printStrings (l: string list) =
		List.iter printString l
end

module UtilTests =
struct
	let active = false

	let test0 () =
		Util.println [Util.load_file "examples/fa_abc.json"]

	let test1 () =
		let a = Util.word2str ['e';'r';'t'] in
		let b = Util.word2str ['4';'5';'y'] in
			Util.println [a; b]

	let runAll : unit =
		if active then (
			Util.header "UtilTests";
			test0 ();
			test1 ()
		)
end
