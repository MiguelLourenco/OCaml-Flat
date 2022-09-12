#ifdef ALL

(*
 * PushdownAutomaton.ml
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
 *  Written by Carlos Freitas (cf)
 *)

(*
 * ChangeLog:
 *
 * ???/2022 (cf) - ???.
 * may/2022 (amd) - Initial skeleton.
 *)

(*
 * Description: Pushdown automata functionality.
 *)

open BasicTypes

module type PushdownAutomatonSig = sig

	type transition = state * symbol * symbol * state * symbol list
	type transitions = transition set
	type tx = {
		inputAlphabet: symbol list;
		stackAlphabet: symbol list;
		states: state list;
		initialState: state;
		initialStackSymbol: symbol;
		transitions: transition list;
		acceptStates: state list;
		criteria: bool
	}
	type t = {
		inputAlphabet : symbols;
		stackAlphabet : symbols;
		states : states;
		initialState : state;
		initialStackSymbol : symbol;
		transitions : transitions;
		acceptStates : states;
		criteria: bool; (* true = acceptStates | false = emptyStack *) 
	}
	val modelDesignation : string
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
				
				method tracing : unit

				method accept : word -> bool
				method generate : int -> words	
			
			(* Exercices support *)
				method checkProperty : string -> bool
				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties

			(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t			end
end

module PushdownAutomaton : PushdownAutomatonSig =
struct

	type transition =
		  state			(* state *)	
		* symbol		(* current symbol on top of the stack *)
		* symbol		(* consumed input symbol *)
		* state			(* next state *)
		* symbol list	(* new top of stack*)

	type transitions = transition set

	type tx = {
		inputAlphabet: symbol list;
		stackAlphabet: symbol list;
		states: state list;
		initialState: state;
		initialStackSymbol: symbol;
		transitions: transition list;
		acceptStates: state list;
		criteria: bool
	}
	
	type t = {
		inputAlphabet: symbols;		(* Input Alphabet *)
		stackAlphabet: symbols;		(* Stack Alphabet *)
		states: states;				(* States *)
		initialState: state;		(* Initial state *)
		initialStackSymbol: symbol;	(* Initial Symbol on the Stack *)
		transitions: transitions;	(* Transition relation *)
		acceptStates: states;		(* Accept states *)
		criteria: bool				(* true = acceptStates | false = emptyStack *)
	}

	let modelDesignation = "pushdown automaton"

	let internalize (pda: tx): t = {
		inputAlphabet = Set.make pda.inputAlphabet;
		stackAlphabet = Set.make pda.stackAlphabet;
		states = Set.make pda.states;
		initialState = pda.initialState;
		initialStackSymbol = pda.initialStackSymbol;
		transitions = Set.make pda.transitions;
		acceptStates = Set.make pda.acceptStates;
		criteria = pda.criteria
	}

	let externalize (pda: t): tx = {
		inputAlphabet = Set.toList pda.inputAlphabet;
		stackAlphabet = Set.toList pda.stackAlphabet;
		states = Set.toList pda.states;
		initialState = pda.initialState;
		initialStackSymbol = pda.initialStackSymbol;
		transitions = Set.toList pda.transitions;
		acceptStates = Set.toList pda.acceptStates;
		criteria = pda.criteria
	}

	let fromJSon (j: JSon.t): t =
		if JSon.isNull j || not (JSon.hasField j "kind") then {			
			inputAlphabet = Set.empty;
			stackAlphabet = Set.make [draftVar];
			states = Set.make [draftState];
			initialState = draftState;
			initialStackSymbol = draftVar;
			transitions = Set.empty;
			acceptStates = Set.empty;
			criteria = false
		}
		else {
			inputAlphabet = JSon.fieldSymbolSet j "inputAlphabet";
			stackAlphabet = JSon.fieldSymbolSet j "stackAlphabet";
			states = JSon.fieldStateSet j "states";
			initialState = JSon.fieldState j "initialState";
			initialStackSymbol = JSon.fieldSymbol j "initialStackSymbol";
			transitions = JSon.fieldQuintupletsSet j "transitions";
			acceptStates = JSon.fieldStateSet j "acceptStates";
			criteria = JSon.fieldBool j "criteria"
		}

	let toJSon (rep: t): JSon.t =
		JSon.makeAssoc [
			("inputAlphabet", JSon.makeSymbolSet rep.inputAlphabet);
			("stackAlphabet", JSon.makeSymbolSet rep.stackAlphabet);
			("states", JSon.makeStateSet rep.states);
			("initialState", JSon.makeState rep.initialState);
			("initialStackSymbol", JSon.makeSymbol rep.initialStackSymbol);
			("transitions", JSon.makeQuintupletsSet rep.transitions);
			("acceptStates", JSon.makeStateSet rep.acceptStates);
			("criteria", JSon.makeBool rep.criteria)
		]
	
			
	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super

			val representation: t =
				match arg with
				| Arg.Representation r -> r
				| Arg.RepresentationX r -> internalize r
				| _ -> fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation: t =
				representation

			method representationx: tx =
				externalize representation

			method toJSon: JSon.t =
				JSon.append (super#toJSon) (toJSon representation)

			method validate: unit = ()

			method tracing : unit = ()
						
			method accept(w: word): bool =
				false

			method generate (length: int): words =
				Set.empty	
		
		(* Learn-OCaml support *)
		(* incomplete *)
			method moduleName =
				"PushdownAutomaton"

			method xTypeName =
				"pushdownAutomaton"
				
			method xTypeDeclString : string = ""

			method toDisplayString (name: string): string = ""

			method example : JSon.t =
				JSon.parse {|
				{
					kind : "pushdown automaton"
				}
			|}
	end
end
	
module PushdownAutomatonTests : sig end =
struct
	let active = false

	let pda_astar = {| {
			kind : "pushdown automaton",
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
	
	let test0 () = (* not a pushdown automaton - will change*)
		let pda = new PushdownAutomaton.model (Arg.Text pda_astar) in
			let j = pda#toJSon in
				JSon.show j

	let runAll =
		if Util.testing active "PushdownAutomaton" then begin
			Util.header "PushdownAutomatonTests starting...";
			test0 ()
		end
end

#endif
