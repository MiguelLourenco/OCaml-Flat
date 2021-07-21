(*
 * JSon.ml
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
 * may/2021 (amd) - Added a second parser, for OCaml values syntax. The output
 *                  is regular JSon.
 * jan/2021 (amd) - Added a very simple recursive descent parser for JSon.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Very simple JSon parser, plus some JSon handling functions.
 *)

module type JSonSig =
sig
	type t =
		| JNull
		| JString of string
		| JAssoc of (string * t) list
		| JList of t list

	val parse : string -> t
	val parseOon : string -> t
	val fromFile : string -> t

	val toStringN : int -> t -> string
	val toString : t -> string
	val show : t -> unit
	val remove : t -> string list -> t

	val hasField : t -> string -> bool
	val fieldString : t -> string -> string
	val fieldStringList : t -> string -> string list
	val fieldStringSet : t -> string -> string Set.t
	val fieldCharList : t -> string -> char list
	val fieldCharSet : t -> string -> char Set.t
	val fieldTriplesList : t -> string -> (string * char * string) list
	val fieldTriplesSet : t -> string -> (string * char * string) Set.t
	val append: t -> t -> t
end

module JSon : JSonSig =
struct
	type t =
		| JNull
		| JString of string
		| JAssoc of (string * t) list
		| JList of t list

	module JSonParsing = (* JSon syntax *)
	struct
		open Scanner

		let parseString () =
			skip();	(* skip quotation mark *)
			let tk = getToken (fun c -> c <> '"') in
				match curr () with
					| '"' -> skip(); tk
					| err -> expecting "closing '\"'" err

		let parseWord () =
			getToken (fun c -> 'a' <= c && c <= 'z'
						|| 'A' <= c && c <= 'Z'
						|| '0' <= c && c <= '9'
						|| c = '_')

		let parseLabel () =
			match curr() with
				| '"' -> parseString ()
				| 'a'..'z' -> parseWord ()
				| err -> expecting "'STRING' or '}'" err

		let checkEOF () =
			match curr() with
				| ' ' -> ()
				| err -> expecting "'EOF'" err


		let rec parsePair () =
			let label = parseLabel () in
				match curr() with
					| ':' -> skip(); (label, parseJSon ())
					| err -> expecting "':'" err

		and parseAssocCont () =
			let p = parsePair () in
				match curr() with
					| ',' -> skip(); p::parseAssocCont ()
					| '}' -> skip(); [p]
					| err -> expecting "',' or '}'" err

		and parseAssoc () =
			skip();	(* skip { *)
			match curr() with
				| '}' -> skip(); []
				| ' ' -> expecting "'}' or 'STRING'" ' '
				| _ -> parseAssocCont ()

		and parseListCont () =
			let j = parseJSon () in
				match curr() with
					| ',' -> skip(); j::parseListCont ()
					| ']' -> skip(); [j]
					| err -> expecting "',' or ']'" err

		and parseList () =
			skip();	(* skip [ *)
			match curr() with
				| ']' -> skip(); []
				| ' ' -> expecting "']' or 'JSON'" ' '
				| _ -> parseListCont ()

		and parseJSon s =
			match curr() with
				| '"' -> JString (parseString ())
				| '[' -> JList (parseList ())
				| '{' -> JAssoc (parseAssoc ())
				| err -> expecting "'JSON'" err

		let parse s =
			Scanner.start "JSon" s;
			try
				let j = parseJSon () in
					checkEOF (); j
			with Not_found ->
				JNull
	end

	module OCamlValueParsing = (* OCaml value syntax *)
	struct
		open Scanner

		let parseString (delim) =
			skip();	(* skip quotation mark *)
			let tk = getToken (fun c -> c <> delim) in
				match curr () with
					| x when x = delim -> skip(); tk
					| err -> expecting "closing '\"'" err

		let parseWord () =
			getToken (fun c -> 'a' <= c && c <= 'z'
						|| 'A' <= c && c <= 'Z'
						|| '0' <= c && c <= '9'
						|| c = '_')

		let parseLabel () =
			match curr() with
				| '"' -> parseString ('"')
				| 'a'..'z' -> parseWord ()
				| err -> expecting "'STRING' or '}'" err

		let checkEOF () =
			match curr() with
				| ' ' -> ()
				| err -> expecting "'EOF'" err


		let rec parsePair () =
			let label = parseLabel () in
				match curr() with
					| '=' -> skip(); (label, parseOon ())
					| err -> expecting "'='" err

		and parseAssocCont () =
			let p = parsePair () in
				match curr() with
					| ';' -> skip(); p::parseAssocCont ()
					| '}' -> skip(); [p]
					| err -> expecting "';' or '}'" err

		and parseAssoc () =
			skip();	(* skip { *)
			match curr() with
				| '}' -> skip(); []
				| ' ' -> expecting "'}' or 'STRING'" ' '
				| _ -> parseAssocCont ()

		and parseListCont () =
			let j = parseOon () in
				match curr() with
					| ';' -> skip(); j::parseListCont ()
					| ']' -> skip(); [j]
					| err -> expecting "';' or ']'" err

		and parseList () =
			skip();	(* skip [ *)
			match curr() with
				| ']' -> skip(); []
				| ' ' -> expecting "']' or 'Oon'" ' '
				| _ -> parseListCont ()

		and parseTupleCont () =
			let j = parseOon () in
				match curr() with
					| ',' -> skip(); j::parseTupleCont ()
					| ')' -> skip(); [j]
					| err -> expecting "',' or ')'" err

		and parseTuple () =
			skip();	(* skip [ *)
			match curr() with
				| ')' -> skip(); []
				| ' ' -> expecting "')' or 'Oon'" ' '
				| _ -> parseTupleCont ()

		and parseOon s =
			match curr() with
				| '"' -> JString (parseString ('"'))
				| '\'' -> JString (parseString ('\''))
				| '[' -> JList (parseList ())
				| '(' -> JList (parseTuple ())
				| '{' -> JAssoc (parseAssoc ())
				| err -> expecting "'OON'" err
				
		let parse s =
			Scanner.start "OON" s;
			try
				let j = parseOon () in
					checkEOF (); j
			with Not_found ->
				JNull
	end

	let parse s =
		JSonParsing.parse s

	let parseOon s =
		OCamlValueParsing.parse s

	let fromFile filename =
		parse (Util.loadFile filename)

(* PRETTY PRINT *)
	let tab n =
		String.make n '\t'

	let isComplex j =
		match j with
		| JList l -> true
		| JAssoc l -> true
		| _ -> false

	let rec textual (tab1: int) (tab2: int) (j: t) : string =
		tab tab1
		^
		match j with
		| JNull ->
			"null"
		| JString s ->
				"\"" ^ s ^ "\""
		| JList l when List.exists isComplex l ->
				let elems = List.map (textual (tab2+1) (tab2+1)) l in (
						"[\n"
						^ String.concat (",\n") elems ^ "\n"
						^ tab tab2 ^ "]"
					)
		| JList l ->
				let elems = List.map (textual 0 0) l in
					("[" ^ String.concat ", " elems ^ "]")
		| JAssoc [] ->
				"{}"
		| JAssoc l ->
				let field (s,j) = tab (tab2+1) ^ s ^ " : " ^ textual 0 (tab2+1) j in
					let elems = List.map field l in (
						"{\n"
						^ String.concat ",\n" elems ^ "\n"
						^ tab tab2 ^ "}"
					)

	let rec textualOCaml (tab1: int) (tab2: int) (j: t) : string =
		tab tab1
		^
		match j with
		| JNull ->
			"null"
		| JString s ->
				"\"" ^ s ^ "\""
		| JList l when List.exists isComplex l ->
				let elems = List.map (textual (tab2+1) (tab2+1)) l in (
						"[\n"
						^ String.concat (",\n") elems ^ "\n"
						^ tab tab2 ^ "]"
					)
		| JList l ->
				let elems = List.map (textual 0 0) l in
					("[" ^ String.concat ", " elems ^ "]")
		| JAssoc [] ->
				"{}"
		| JAssoc l ->
				let field (s,j) = tab (tab2+1) ^ s ^ " : " ^ textual 0 (tab2+1) j in
					let elems = List.map field l in (
						"{\n"
						^ String.concat ",\n" elems ^ "\n"
						^ tab tab2 ^ "}"
					)

	let toStringN n j =
		textual 0 n j

	let toString j =
		toStringN 0 j

	let show (j: t) =
		Util.println [toString j]

	let remove (j: t) r =
		match j with
		| JAssoc l ->
			JAssoc (List.filter (fun (a,_) -> not (List.mem a r)) l)
		| _ ->
			j


(* MEMBERSHIP *)
	let hasField j name =
		match j with
		| JAssoc obj -> (
				try
					ignore (List.assoc name obj); true
				with Not_found -> false
			)
		| _ ->
			false

	let getField name j =
		match j with
		| JAssoc obj -> (
				try
					List.assoc name obj
				with Not_found -> JNull
			)
		| _ ->
			JNull

(* MORE *)

	let error = Error.error

	let fieldString (j: t) (field: string) =
		match j |> getField field with
		| JNull -> error field "Missing field" "#"
		| JString s -> s
		| _ -> error field "Expected string" "#"

	let asString (j: t) (field: string) =
		match j with
		| JString s -> s
		| _ -> error field "Expected string" "#"

	let fieldStringList (j: t) (field: string) =
		match j |> getField field with
		| JNull -> error field "Missing field" []
		| JList l -> List.map (fun j -> asString j field) l
		| _ -> error field "Expected string list" []

	let fieldStringSet (j: t) (field: string) =
		Set.validate (fieldStringList j field) field

	let asChar (j: t) (field: string) =
		match j with
		| JString s when String.length s = 1 -> String.get s 0
		| _ -> error field "Expected char" '#'

	let fieldCharList (j: t) (field: string) =
		match j |> getField field with
		| JNull -> error field "Missing field" []
		| JList l -> List.map (fun j -> asChar j field) l
		| _ -> error field "Expected char list" []

	let fieldCharSet (j: t) (field: string) =
		Set.validate (fieldCharList j field) field

	let asStringCharString (j: t) (field: string) =
		match j with
		| JList [a; b; c] -> (asString a field, asChar b field, asString c field)
		| _ -> error field "Malformed triple" ("#",'#',"#")

	let fieldTriplesList (j: t) (field: string) =
		match j |> getField field with
		| JNull -> error field "Missing field" []
		| JList l -> List.map (fun j -> asStringCharString j field) l
		| _ -> []

	let fieldTriplesSet (j: t) (field: string) =
		Set.validate (fieldTriplesList j field) field
	
	let append j1 j2 =
		match j1, j2 with
		| JAssoc l1, JAssoc l2 -> JAssoc (l1 @ l2)
		| _, _ -> failwith "JSon.append: not Assoc"

end

module JSonTests =
struct
	let active = false

	let jsonSample = {| {
		name: {
			first: "aa",
			last: "22",
			fullname: "33"
		},
		age: "33",
		hobbies: [ "44", "55" ]
	} |}
	
	let jsonSample2 = {| "aa" |}
	
	let test0 () =
		let json = JSon.parse jsonSample in
		let json2 = JSon.parse jsonSample2 in
			JSon.show json; JSon.show json2
	
	let oonSample = {| {
		alphabet = ['a';'b'];
		states = ["START"; "33"];
		initialState = "START";
		transitions = [("START", 'a', "START"); ("START", 'a', "START")];
		acceptStates = ["START"]
	} |}
	
	let oonSample2 = {| "z*" |}

	let oonSample3 = {| ("START", ["ee"], "yu") |}

	let test1 () =
		let oon = JSon.parseOon oonSample in
		let oon2 = JSon.parseOon oonSample2 in
		let oon3 = JSon.parseOon oonSample3 in
			JSon.show oon; JSon.show oon2; JSon.show oon3

	let test () =
		let oon2 = JSon.parseOon oonSample2 in
			JSon.show oon2

	let runAll =
		if Util.testing(active) then (
			Util.header "JSonTests";
			test ()
		)

end
