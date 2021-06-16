(*
 * RegularExpression.ml
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
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * may/2021 (amd) - Added support for an extern representation.
 * jan/2021 (amd) - Module in an independent file and some cleanup.
 * dec/2019 (jg) - Main functionalities.
 * jun/2019 (amd) - Initial skeleton, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Regular expressions functionality.
 *
 * TODO: More cleanup. Improve the regular expression simplifier.
 *)

module type RegularExpressionSig =
sig	
	type tx = string
	
	type t = RegExpSyntax.t

	type reTree =
	| Fail
	| Tree of word * t * reTree list

	val modelDesignation : string
	val register : tx -> (string -> bool)
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation : t
				method representationx : tx
				method validate : unit
				method example : JSon.t

				method accept : word -> bool
				method allTrees : word -> unit
				method generate : int -> words
				method tracing : unit

				method alphabet : symbols
				method quasiLanguage : words

				method simplify : model

				method checkProperty : string -> bool
				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties
											
				method moduleName : string
				method toDisplayString : string -> string
			end
end

module RegularExpression : RegularExpressionSig =
struct
	open RegExpSyntax

	type tx = string

	type t = RegExpSyntax.t
	
	type reTree =
	| Fail
	| Tree of word * t * reTree list

	let modelDesignation = "regular expression"

	let internalize (re: tx): t =
		RegExpSyntax.parse re

	let externalize (re: t): tx =
		RegExpSyntax.toString re

	let fromJSon j =
		let re = JSon.field_string j "re" in
			RegExpSyntax.parse re

	let toJSon (rep: t): JSon.t =
		let open JSon in
		JAssoc [
			("re", JString (RegExpSyntax.toString rep));
		]

	(* auxiliary functions *)
	let seqConcat aset bset = Set.flatMap (fun s1 -> Set.map (fun s2 -> s1@s2) bset) aset

	let displayHeader (name: string) (moduleName: string) =
		if name = "" then
			""
		else
			("let " ^ name ^ ": " ^ moduleName ^ ".tx =\n\t\t")

	let toDisplayString (name: string) (moduleName: string) (repx: tx): string =
		Printf.sprintf {zzz|
		%s	%s
		|zzz}
			(displayHeader name moduleName)
			(Util.string2DisplayString repx)

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super

			val representation: t =
				match arg with
					| Arg.Representation r -> r
					| Arg.RepresentationX r -> internalize r
					| _ -> fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method toJSon: JSon.t =
				JSon.append (super#toJSon) (toJSon representation)

			method representation: t =
				representation

			method representationx: tx =
				externalize representation

			method validate = (

				(*
				let representation = RegExpSyntax.parse "(((xx+ut)+(aaa+dss+ghf)+(xx+uu))ee)+bgc*+(jgg+bgcd)" in

				let rec lang rep =
					match rep with
						| RegExpSyntax.Plus(l, r) -> Util.print "pls: "; Util.print (RegExpSyntax.toString l); Util.print ", ";
						Util.print (RegExpSyntax.toString r); Util.println ""; Set.union (lang l) (lang r)
						| RegExpSyntax.Seq(l, r) -> Util.print "seq: "; Util.print (RegExpSyntax.toString l); Util.print ", ";
						Util.print (RegExpSyntax.toString r); Util.println ""; Set.union (lang l) (lang r)
						| RegExpSyntax.Star(r) -> Util.print "str: "; Util.print (RegExpSyntax.toString r); Util.println ""; (lang r)
						| RegExpSyntax.Symb(c) -> Set.make [c]
						| RegExpSyntax.Empty -> Set.empty
						| RegExpSyntax.Zero -> Set.empty
				in
				let a = lang representation in
				()
				*)
			)

			method example : JSon.t = 
				JSon.from_string {| {
					kind : "regular expression",
					description : "this is a simple example",
					name : "example",
					re : "a*+(a+bc)*"
				} |}

			method tracing: unit = ()


			(**
			* This method generates the alphabet of all symbols in the expression
			*
			* @returns symbols -> the set of all symbols in the expression's alphabet
			*
			*)
			method alphabet: symbols =

				let rec alf rep =
					match rep with
						| Plus(l, r) -> Set.union (alf l) (alf r)
						| Seq(l, r) -> Set.union (alf l) (alf r)
						| Star(r) -> alf r
						| Symb(c) -> Set.make [c]
						| Empty -> Set.empty
						| Zero -> Set.empty
				in
					alf representation


			(**
			* This method generates the language of the regular expression for when klenne is always zero
			*
			* @returns words -> set of generated words
			*
			*)
			method quasiLanguage: words =

				let rec lang rep =
					match rep with
						| Plus(l, r) -> Set.union (lang l) (lang r)
						| Seq(l, r) -> seqConcat (lang l) (lang r)
						| Star(r) -> Set.make [[]]
						| Symb(c) -> Set.make [[c]]
						| Empty -> Set.empty
						| Zero -> Set.empty
				in
					lang representation




			(**
			* This method tests if a given word is accepted by the regular expression
			*
			* @param w:word -> word to be tested for acceptance
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			*)
			method accept (w: word): bool =

				let partition w =
					let rec partX w pword =
						match w with
							[] -> Set.empty
							| x::xs -> let fwp = pword@[x] in
											Set.add (fwp, xs) (partX xs fwp) in
					Set.add ([],w) (partX w []) in

				let rec acc rep w =
					match rep with
						| Plus(l, r) -> (acc l w) || (acc r w)
						| Seq(l, r) -> let wpl = partition w in
											Set.exists (fun (wp1,wp2) -> (acc l wp1) && (acc r wp2)) wpl
						| Star(re) -> w = [] ||
									(let wpl = Set.remove ([],w) (partition w) in
											Set.exists (fun (wp1,wp2) -> (acc re wp1) && (acc (Star re) wp2)) wpl)
						| Symb(c) -> w = [c]
						| Empty -> w = []
						| Zero -> false
				in

					acc representation w



			(**
			* This method returns the derivation tree for the word acceptance
			*
			* @param w:word -> word to be tested for acceptance
			*
			* @returns reTree list -> list of derivation trees
			*
			*)
			method allTrees w : unit =


				let partition w =
					let rec partX w pword =
						match w with
							[] -> Set.empty
							| x::xs -> let fwp = pword@[x] in
										Set.add (fwp, xs) (partX xs fwp)
					in
					Set.add ([],w) (partX w [])
				in

				let rec acc w rep =
					match rep with
						| Plus(l, r) ->
							let l1 = acc w l in
							let r1 = acc w r in
								List.map (fun t -> Tree (w, rep, [t])) (l1 @ r1)

						| Seq(l, r) ->
							let wps = partition w in
							let wpl = Set.toList wps in
							List.flatten ( List.map (fun (wp1, wp2) ->
								let tl = acc wp1 l in
								let tr = acc wp2 r in
									List.flatten (List.map (fun x -> List.map
										(fun y -> Tree (w, rep, [x; y])) tr)tl)
							) wpl)

						| Star(re) ->
							if w = [] then
								[Tree (['~'], rep, [])]
							else
								(let wps = Set.remove ([],w) (partition w) in
								let wpl = Set.toList wps in
								List.flatten (List.map (fun (wp1, wp2) ->
									let tl = acc wp1 re in
									let tr = acc wp2 (Star re) in
									List.flatten (List.map (fun x -> List.map
										(fun y -> Tree (w, rep, [x; y])) tr) tl)) wpl))

						| Symb(c) ->
							if w = [c] then
								[Tree (w, rep, [])]
							else
								[Tree (w, rep, [Fail])]

						| Empty ->
							if w = [] then
								[Tree (['~'], rep, [])]
							else
								[Tree (w, rep, [Fail])]

						| Zero -> [Tree (w, rep, [Fail])]

				in

				let ac = acc w self#representation in



				let rec isNotFail t =
					match t with
						Fail -> false
						| Tree ([], re, []) -> true
						| Tree (w, re, []) -> true
						| Tree ([], re, x::xs) -> (isNotFail x) && (isNotFail (Tree ([], re, xs)))
						| Tree (w, re, x::xs) -> (isNotFail x) && (isNotFail (Tree (w, re, xs)))
				in

				let ts = List.filter (fun t -> isNotFail t) ac in


				let printTreeX w re n =
					let s = String.make (3*n) ' ' in
					Util.println [s; Util.word2str w; " -> "; RegExpSyntax.toString re]
				in

				let rec printTree t n =
					match t with
						Fail -> Util.println ["Fail"]
						| Tree ([], re, []) -> Util.println ["TREH "]
						| Tree (w, re, []) -> printTreeX w re n
						| Tree ([], re, x::xs) -> printTreeX [] re n; printTree x (n+1); List.iter (fun t -> printTree t (n+1)) xs
						| Tree (w, re, x::xs) -> printTreeX w re n; printTree x (n+1); List.iter (fun t -> printTree t (n+1)) xs
				in

					List.iter (fun t -> printTree t 0) ts



			(**
			* This method generates all words up to the given length that are generated by the regular expression
			*
			* @param length:int -> maximum length of all generated words
			*
			* @returns words -> set of generated words
			*)
			method generate (length: int): words =

				let rec lang rep ln =
					match rep with
						| Plus(l, r) ->
								Set.union (lang l ln) (lang r ln)
						| Seq(l, r) ->
								let left = lang l ln in
								let rigth w = lang r (ln - (List.length w)) in
								let conc w = Util.concatAll w (Set.toList (rigth w)) in
									Set.flatMap (fun lw -> Set.make (conc lw)) left
						| Star r ->
								let exp = lang r ln in
									Set.star exp ln

							(* alternate version of star, leave 4 now

							let rec starX ws sz =
								if sz <= 0 then Set.make [[]]
								else
									let ws = Set.filter (fun x -> sz >= (List.length x)) ws in
									let newLn w = sz - (List.length w) in
									let tail w ws = Set.toList (starX ws (newLn w)) in
									let conc w ws = Util.concatAll w (tail w ws) in
									let track w ws = Set.add w (Set.make (conc w ws)) in
										Set.flatMap (fun w -> if w = [] then Set.make [[]] else track w ws) ws in
							let exp = lang r ln in
								Set.add [] (starX exp ln)*)

						| Symb(c) -> if ln > 0 then Set.make [[c]] else Set.empty
						| Empty -> Set.make [[]]
						| Zero -> Set.empty
				in
					lang representation length


			(**
			* This method simplifies the regular expression
			*
			* @returns RegularExpression.model -> the new simplified, equivalent expression
			*)
			method simplify : model =

				(* various base case simplification rules to apply to the given expressions *)
				let rec simpX re =
					match re with
						(* plus *)
						(* a* + empty *)
						| Plus(Star(l), Empty) -> Star(l)
						| Plus(Empty, Star(r)) -> Star(r)
						(* a* + zero *)
						| Plus(Zero, r) -> r
						| Plus(l, Zero) -> l
						(* ~ + aa* *)
						| Plus(Empty, Seq(l, Star(r))) when l = r -> Star(r)
						| Plus(Empty, Seq(Star(l), r)) when l = r -> Star(l)
						| Plus(Seq(l, Star(r)), Empty) when l = r -> Star(r)
						| Plus(Seq(Star(l), r), Empty) when l = r -> Star(l)
						(* a* + a + empty *)
						| Plus(Star(l), Plus(Empty, r)) when l = r -> Star(l)
						| Plus(Star(l), Plus(r, Empty)) when l = r -> Star(l)
						| Plus(Plus(Empty, l), Star(r)) when l = r -> Star(r)
						| Plus(Plus(l, Empty), Star(r)) when l = r -> Star(r)
						(* a* + a *)
						| Plus(Star(l), r) when l = r -> Star(l)
						| Plus(l, Star(r)) when l = r -> Star(r)
						(* a + b = a||b when a = b *)
						| Plus(l, r) when l = r -> l
						(* seq *)
						| Seq(Empty, Empty) -> Empty
						| Seq(Zero, Zero) -> Zero
						| Seq(Empty, r) -> r
						| Seq(l, Empty) -> l
						| Seq(Zero, r) -> Zero
						| Seq(l, Zero) -> Zero
						(* (~+a)a* *)
						| Seq(Plus(Empty, l),Star(r)) when l = r -> Star(r)
						| Seq(Plus(l, Empty),Star(r)) when l = r -> Star(r)
						| Seq(Star(l),Plus(Empty, r)) when l = r -> Star(l)
						| Seq(Star(l),Plus(r, Empty)) when l = r -> Star(l)
						| Seq(Star(l),Star(r)) when l = r -> Star(l)
						(* star *)
						| Star(Star(r)) -> Star(r)
						| Star(Plus(Empty, r)) -> Star(r)
						| Star(Plus(r, Empty)) -> Star(r)
						| Star(Empty) -> Empty
						| Star(Zero) -> Empty
						| Star(r) -> re
						(* symb *)
						| Symb(c) -> Symb c
						(* empty *)
						| Empty -> Empty
						(* zero *)
						| Zero -> Zero
						| _ -> re
				in

				(* applies various base case simplifications to the various sub-expressions of regular expression re *)
				let rec simplify re =

					match re with
						| Plus(l,r) -> simpX (Plus(simplify l, simplify r))
						| Seq(l,r) -> simpX (Seq(simplify l, simplify r))
						| Star(re) -> simpX (Star(simplify re))
						| Symb(c) -> Symb c
						| Empty -> Empty
						| Zero -> Zero
				in

				let sre = simplify representation in

				new model (Arg.Representation (sre))

			method checkProperty (prop: string) =
				match prop with
					| "regular expression" -> true
					| _ -> super#checkProperty prop

			method moduleName =
				"RegularExpression"

			method toDisplayString (name: string): string =
				toDisplayString name self#moduleName self#representationx

		end
		
		let register solution =
			let model = new model (Arg.RepresentationX solution) in
				(fun x -> model#accept (Util.str2word x))
end


module RegularExpressionTests: sig end =
struct
	let active = false

	let test0 () =
		let m = new RegularExpression.model (Arg.Predef "re_abc") in
			let j = m#toJSon in
				JSon.show j

	let test1 () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			let j = re#toJSon in
				JSon.show j

	let testAlphabet () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			Util.println ["alphabet: "]; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println []

	let testAlphabet2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			Util.println ["alphabet: "]; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println []

	let testAlphabet3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
			Util.println ["alphabet: "]; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println []

	let testAlphabet4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
			Util.println ["alphabet: "]; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println []

	let testQuasiLang () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			let ws = re#quasiLanguage in
			Util.printWords (Set.toList ws)

	let testQuasiLang2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			let ws = re#quasiLanguage in
			Util.printWords (Set.toList ws)

	let testQuasiLang3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
			let ws = re#quasiLanguage in
			Util.printWords (Set.toList ws)

	let testQuasiLang4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
			let ws = re#quasiLanguage in
			Util.printWords (Set.toList ws)

	let check f w =
		let msg = 
			if f w then "word was accepted" else "word was not accepted"
		in Util.println [msg]

	let testAccept () =
		let m = new RegularExpression.model (Arg.Predef "re_abc") in
			check m#accept ['a';'a']

	let testAccept2 () =
		let m = new RegularExpression.model (Arg.Predef "re_simple") in
			check m#accept ['a';'a']

	let testAccept3 () =
		let m = new RegularExpression.model (Arg.Predef "re_complex") in
			check m#accept ['a';'a']

	let testAccept4 () =
		let m = new RegularExpression.model (Arg.Predef "re_convoluted") in
			check m#accept ['a';'a']

	let testGenerate () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (re#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (re#generate 1));
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (re#generate 2));
			Util.println ["generated words size 3:"]; Util.printWords (Set.toList (re#generate 3));
			Util.println ["generated words size 4:"]; Util.printWords (Set.toList (re#generate 4));
			Util.println []

	let testGenerate2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (re#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (re#generate 1));
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (re#generate 2));
			Util.println ["generated words size 3:"]; Util.printWords (Set.toList (re#generate 3));
			Util.println ["generated words size 4:"]; Util.printWords (Set.toList (re#generate 4));
			Util.println []

	let testGenerate3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (re#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (re#generate 1));
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (re#generate 2));
			Util.println ["generated words size 3:"]; Util.printWords (Set.toList (re#generate 3));
			Util.println ["generated words size 4:"]; Util.printWords (Set.toList (re#generate 4));
			Util.println []

	let testGenerate4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (re#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (re#generate 1));
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (re#generate 2));
			Util.println ["generated words size 3:"]; Util.printWords (Set.toList (re#generate 3));
			Util.println ["generated words size 4:"]; Util.printWords (Set.toList (re#generate 4));
			Util.println []

	let testSimplify2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let res = re#simplify in
			JSon.show res#toJSon

	let testEnum () =
		let e = new Exercise.exercise (Arg.Predef "exer_re2fa") in
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let result = re#checkExercise e in
			if result then Util.print ["it works"] else Util.print ["it does not work"]

	let testTrace () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			re#allTrees ['a';'c';'b';'a';'c';'b']
	
	let re_more = {| {
			kind : "regular expression",
			description : "this is an example",
			name : "re_more",
			re : "**"
	} |}
				
	let testMore () =
		let re = new RegularExpression.model (Arg.Text re_more) in
			re#allTrees ['a';'a']

	let runAll =
		if active then (
			Util.header "RegularExpressionTests";
			testMore ()
		)
end

