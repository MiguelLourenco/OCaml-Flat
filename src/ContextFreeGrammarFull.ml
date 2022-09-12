#ifdef ALL
 
(*
 * ContextFreeGrammarFull.ml
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
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
????
 *)

(*
 * Description: Context-free grammar functionality.
 *
 * TODO: More cleanup.
 *)

open BasicTypes
open CFGTypes  

module type ContextFreeGrammarFullSig =
sig
	type syntaxTable = LL1Grammar.syntaxTable
	type acceptTable = LL1Grammar.acceptTable
	type recognized = LL1Grammar.recognized
	type acceptStep = LL1Grammar.acceptStep
	type transformation = LL1Grammar.transformation

	class model :
		(t,tx) Arg.alternatives ->
			object
			(*ContextFreeGrammar Basic*)
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation: t
				method representationx : tx
				method validate : unit

				method tracing: unit
				method isRegular: bool
				method first: word -> symbol Set.t
				method follow: symbol -> symbol Set.t
				method lookahead: rule -> symbol Set.t
				method accept: word -> bool
				method acceptWithTracing: word -> unit
				method generate: int -> words

			(* LL1Grammar *)
				method isSimplified : bool
				method rdparserOpts : string list
				method toggleSimplified : unit

				method isLL1: bool
				method isLeftRecursive: bool
				method createParsingTable: ((variable * symbol) * word) Set.t
				method hasParsingTableConflict : bool 
				method acceptZ: word -> acceptStep list
				method productiveSymbols: symbol Set.t
				method accessibleSymbols: symbol Set.t
				method productiveRewrite: ContextFreeGrammar.model
				method accessibleRewrite: ContextFreeGrammar.model
				method clean: transformation list
				method isFullyProductive: bool
				method isFullyAccessible: bool
				method isClean: bool
				method removeLeftRecursion: transformation
				method removeDirectLeftRecursion: ContextFreeGrammar.model
				method leftFactoring: transformation
				method isLeftFactoring: bool
				method leftCorner: symbol -> symbol Set.t
				method hasEmptyProductions: bool
				method removeEmptyProductions: transformation
				method hasUnitProductions: bool
				method removeUnitProductions: transformation
				method generateRecursiveDescendentParser: string -> string
				method transformToLL1: transformation list

			(* LRGrammar *)
				method isLR1 : bool

			(* Exercices support *)
				method checkProperty : string -> bool
				method checkExercise: Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties
											
			(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t
			end
end

module ContextFreeGrammarFull : ContextFreeGrammarFullSig =
struct
	type syntaxTable = LL1Grammar.syntaxTable
	type acceptTable = LL1Grammar.acceptTable
	type recognized = LL1Grammar.recognized
	type acceptStep = LL1Grammar.acceptStep
	type transformation = LL1Grammar.transformation

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit LRGrammar.model arg as super

     end


end

#endif
