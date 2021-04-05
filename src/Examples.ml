(*
 * Examples.ml
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
 * dec/2020 (amd) - Collected the examples in a single module.
 * sep/2019 (jg) - Initial version, each example in an individual file.
 *)

(*
 * Description: A set of good predefined examples.
 *
 * TODO: Check if these examples are really good and improve.
 *)

module type ExamplesSig =
sig
	val examples : string list
	val example : string -> string
	val jsonExample : string -> JSon.t
	val see : string -> unit
end

module Examples : ExamplesSig =
struct
	(* Entity definitions *)

	let dfa_1 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "dfa_1",
			alphabet: ["a", "b"],
			states : ["START", "A", "B", "C"],
			initialState : "START",
			transitions : [
					["START", "a", "A"], ["A", "b", "B"], ["B", "a", "C"], ["C", "b", "B"],
					["C", "a", "A"]
				],
			acceptStates : ["START", "B", "C"]
		} |}

	let dfa_2 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "dfa_2",
			alphabet: ["0", "1"],
			states : ["START", "1", "2", "3"],
			initialState : "START",
			transitions : [
				["START", "1", "1"], ["1", "1", "START"], ["1", "0", "2"], ["2", "0", "1"],
				["2", "1", "3"], ["3", "1", "2"], ["3", "0", "START"], ["START", "0", "3"]
			],
			acceptStates : ["1"]
			} |}

	let dfa_astar = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "dfa_astar",
			alphabet: ["a"],
			states : ["START"],
			initialState : "START",
			transitions : [
				["START", "a", "START"]
			],
			acceptStates : ["START"]
			} |}

	let fa_abc = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "fa_abc",
			alphabet : ["a", "b", "c", "d"],
			states : ["START", "A", "AB", "SUCCESS"],
			initialState : "START",
			transitions : [
					["START","a","A"], ["START","b","START"], ["START","c","START"], ["START","d","START"],
					["A","a","A"], ["A","b","AB"], ["A","c","START"], ["A","d","START"],
					["AB","a","A"], ["AB","b","START"], ["AB","c","SUCCESS"], ["AB","d","START"],
					["SUCCESS","a","SUCCESS"], ["SUCCESS","b","SUCCESS"], ["SUCCESS","c","SUCCESS"], ["SUCCESS","d","SUCCESS"]
				],
			acceptStates : ["SUCCESS"]
		} |}

	let nfa_1 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "nfa_1",
			alphabet: ["a", "b"],
			states : ["START", "A", "B"],
			initialState : "START",
			transitions : [
					["START", "a", "A"], ["A", "b", "B"], ["A", "b", "START"], ["B", "a", "START"]
				],
			acceptStates : ["START"]
			} |}

	let nfa_2 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "nfa_2",
			alphabet : ["a", "b", "c", "d", "e"],
			states : ["START", "A", "AB", "SUCCESS", "UNREACHABLE", "UNPRODUCTIVE"],
			initialState : "START",
			transitions : [
					["START","a","A"], ["START","b","START"], ["START","c","START"], ["START","d","START"],
					["A","a","A"], ["A","b","AB"], ["A","c","START"], ["A","d","START"],
					["AB","a","A"], ["AB","b","START"], ["AB","c","SUCCESS"], ["AB","d","START"],
					["SUCCESS","a","SUCCESS"], ["SUCCESS","b","SUCCESS"], ["SUCCESS","c","SUCCESS"], ["SUCCESS","d","SUCCESS"], ["A","a","AB"], ["UNREACHABLE", "a", "SUCCESS"],
					["SUCCESS", "e", "UNPRODUCTIVE"], ["UNPRODUCTIVE", "a", "UNPRODUCTIVE"]
				],
			acceptStates : ["SUCCESS"]
		} |}

	let re_abc = {| {
			kind : "regular expression",
			description : "this is an example",
			name : "re_abc",
			re : "((a+b)*(cd)*)*"
		} |}

	let re_complex = {| {
			kind : "regular expression",
			description : "this is a complex example",
			name : "re_complex",
			re : "(a+(b(c+d)+ea))*f*g"
		} |}

	let re_convoluted = {| {
			kind : "regular expression",
			description : "this is a convoluted example",
			name : "re_convoluted",
			re : "((((a+b)*(cd)*)*+(e(f+gh*i)*jk)*+lmn)op+q)"
		} |}

	let re_simple = {| {
			kind : "regular expression",
			description : "this is a simple example",
			name : "re_simple",
			re : "a+a*+bc*"
		} |}


	let cfg_simple = {| {
			kind : "context free grammar",
			description : "this is an example",
			name : "cfg_simple",
			alphabet : ["0", "1"],
			variables : ["S", "P"],
			initial : "S",
			rules : [	"S -> 1S0 | P",
						"P -> 0P1 | ~" ]
		} |}

	let exer_astar = {| {
			kind : "exercice",
			description : "Exercise astar",
			name : "exer_astar",
			problem : "Convert the regular expression a* to finite automaton.",
			inside : ["","a","aa","aaa","aaaaaaa"],
			outside : ["d","b","ava"],
			properties : ["none"]
		} |}

	let exer_abcd = {| {
			kind : "exercice",
			description : "this is an example",
			name : "exer_abcd",
			problem : "Convert the regular expression (a+b)*(c+d) to finite automaton.",
			inside : ["abc","c","ab","b","abac"],
			outside : ["","aba","bab","abba","baab","abcd"],
			properties : ["qqq"]
		} |}

	let exer_ab = {| {
			kind : "exercice",
			description : "this is an example",
			name : "exer_ab",
			problem : "Convert the regular expression ab*+ba* to finite automaton.",
			inside : ["a","ab","abb","abbbbbbbb","b","ba","baa","baaaaaa"],
			outside : ["","aba","bab","abba","baab","c"],
			properties : ["qqq"]
		} |}

	let exer_re2fa = {| {
			kind : "exercice",
			description : "this is an example",
			name : "exer_re2fa",
			problem : "Converta o autómato finito com alfabeto: [x, y, z], estados: [S, T, V], estado inicial: S, transições [[S, x, S], [S, y, T], [S, z, V], [T, x, T], [T, z, T], [T, y, V], [V, x, T]], e estados finais: [V] em expressão regular.",
			inside : ["z", "xz", "yy", "yzy", "xyy", "zxxy"],
			outside : ["x","y","xy", "xyz", "yyx", "xzxz", "xyxz"],
			properties : ["qqq"]
		} |}

	let exer_readwrite = {| {
			kind : "exercice",
			description : "this is an example",
			name : "exer_readwrite",
			problem : "open,close,read,write",
			inside : ["","orc","owc","orwc","owwrwrrc","ocorwc"],
			outside : ["or","oo","o","w","r","c","orw","owrrww","corwc"],
			properties : ["qqq"]
		} |}



	(* Examples table *)

	let oflatExamplesTable = [
		("dfa_1", dfa_1);
		("dfa_2", dfa_2);
		("dfa_astar", dfa_astar);
		("fa_abc", fa_abc);
		("nfa_1", nfa_1);
		("nfa_2", nfa_2);

		("re_abc", re_abc);
		("re_complex", re_complex);
		("re_convoluted", re_convoluted);
		("re_simple", re_simple);

		("cfg_simple", cfg_simple);

		("exer_astar", exer_astar);
		("exer_abcd", exer_abcd);
		("exer_ab", exer_ab);
		("exer_re2fa", exer_re2fa);
		("exer_readwrite", exer_readwrite)
	]

	let examples =
		List.map fst oflatExamplesTable

	let example name =
		List.assoc name oflatExamplesTable

	let jsonExample name =
		JSon.from_string (example name)

	let see name =
		Util.println [example name]

end
