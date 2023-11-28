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
open Util

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

	let aNbNExample = {| {
		kind : "pushdown automaton",
		description : "this is an example for anbn",
		name : "anbn_astar",
		inputAlphabet: ["a", "b", "_"],
		stackAlphabet: ["a", "z"],
		states: ["p", "q"],
		initialState: "p",
		initialStackSymbol: "z",
		transitions: [
			["p", "a", "a", "p", "aa"],
			["p", "z", "a", "p", "za"],
			["p", "a", "b", "q", "B"],
			["q", "a", "b", "q", "B"],
			["q", "z", "_", "q", "B"]
		],
		acceptStates: [],
		criteria: "false"
		} |}

	let generateTransitionsToPD st alphEntr alphPD =
		let allAlph = Set.add dollar (Set.union alphEntr alphPD) in
			Set.map (fun symb -> (st,symb,st,symb,R)) allAlph 

	let generateTransitionsFromPD st alphEntr alphPD =
		let allAlph = Set.add dollar (Set.union alphEntr alphPD) in
			Set.map (fun symb -> (st,symb,st,symb,L)) allAlph 

	let insertSymbolsPD alphEntr (rep: PushdownAutomaton.t) =
		let alphPD = rep.stackAlphabet in
		let st1 = state (IdGenerator.gen("q")) in
		let st2 = state (IdGenerator.gen("q")) in
		let st3 = state (IdGenerator.gen("q")) in
		let newSts = Set.add st1 ( Set.add st2 ( Set.add st3 Set.empty)) in
		let newTrs1 = Set.union (generateTransitionsToPD st1 alphEntr alphPD) (generateTransitionsFromPD st3 alphEntr alphPD) in
		let newTrs2 = Set.add (st1,empty,st2,symb "$",R) (Set.add (st2,empty,st3,rep.initialStackSymbol,R) ( Set.add (st3,empty,rep.initialState,empty,R)  newTrs1 )) in
			(Set.union rep.states newSts) , newTrs2

	let rec fillStackTransition lastSt prevSt trs wordL =
		match wordL with
		| [] -> trs
		| x::y ->	let newState = if (Set.isEmpty (Set.make y)) then lastSt else IdGenerator.gen("q") in
							let dir = if (Set.isEmpty (Set.make y)) then L else R in
								fillStackTransition lastSt newState (Set.add (prevSt,empty, newState, x, dir) trs) y 

	let convertNormalTransition trs alphEntr alphPD =
		let (startState,unstackedSymbol,readSymbol,nextState,writeSymbolL) = trs in

		let st1 = state (IdGenerator.gen("q")) in
		let st2 = state (IdGenerator.gen("q")) in
		let st3 = state (IdGenerator.gen("q")) in

		let ftrs = (startState,readSymbol,st1,empty,R) in
		let trsTPD = Set.add ftrs (generateTransitionsToPD st1 alphEntr alphPD) in
		let trsRTOP = Set.add (st1,empty,st2,empty,L) trsTPD in

		let firstDirection = if ((List.length writeSymbolL) = 1) then L else R in
		let lastSt = if ((List.length writeSymbolL) = 1) then st3 else state (IdGenerator.gen("q")) in

		let replaceTop = Set.add (st2,unstackedSymbol,st3, (List.hd writeSymbolL), firstDirection) trsRTOP in
		let additionalSymbolTrs = Set.union replaceTop (fillStackTransition lastSt st3 Set.empty (List.tl writeSymbolL)) in
		let trsFPD = Set.union additionalSymbolTrs (generateTransitionsFromPD lastSt alphEntr alphPD) in
		let trsLast = Set.add (lastSt,empty,nextState,empty,R) trsFPD in
			Set.add lastSt (Set.add st3 (Set.add st2 (Set.add st1 Set.empty))), trsLast

	let convertAcceptTransition trs alphEntr alphPD initialStackSymb =
		let (startState,unstackedSymbol,readSymbol,nextState,writeSymbolL) = trs in

		let st1 = state (IdGenerator.gen("q")) in
		let st2 = state (IdGenerator.gen("q")) in
		let st3 = state (IdGenerator.gen("q")) in
		
		let ftrs = Set.add (startState,dollar,st1,dollar,R) Set.empty in
		let checkInitSS = Set.add (st1,initialStackSymb,st2,empty,R) ftrs in
		let lastCheck = Set.add (st2,empty,st3,empty,R) checkInitSS in
			Set.add st3 (Set.add st2 (Set.add st1 Set.empty)), lastCheck

	let convertTransitionX trs alphEntr alphPD initialStackSymb = 
		let (_,_,readSymbol,_,_) = trs in
			if readSymbol == draftVar then convertAcceptTransition trs alphEntr alphPD initialStackSymb
			else convertNormalTransition trs alphEntr alphPD

	let rec convertTransitions newSts newTrs alphEntr (rep: PushdownAutomaton.t) trs = 
		let alphPD = rep.stackAlphabet in
		let initialStackSymb = rep.initialStackSymbol in
		if (Set.size trs) = 0 then newSts, newTrs
		else 
			let (nSts,nTrs) = convertTransitionX (Set.hd trs) alphEntr alphPD initialStackSymb in
				convertTransitions (Set.union nSts newSts) (Set.union nTrs newTrs) alphEntr rep (Set.tl trs)


(*Se parar por pilha vazia 'e ncess'ario criar um estado final*)

	let getFinalStates trs =
		Set.map (fun (_,_,_,d,_) -> d) (Set.filter (fun (_,_,c,_,_) -> c = dollar) trs)

	let pda2tm (pda: PushdownAutomaton.model) =
		let rep: PushdownAutomaton.t = pda#representation in
		let pdaAlphabet = Set.remove draftVar rep.inputAlphabet in
		let (initialStates, initialTransitions) = insertSymbolsPD pdaAlphabet rep in
		let (convertedTransitionStates,convertedTransitions) = convertTransitions Set.empty Set.empty pdaAlphabet rep rep.transitions in
		let allAlphabet = Set.add dollar ( Set.union pdaAlphabet rep.stackAlphabet) in
		let allStates = Set.union initialStates convertedTransitionStates in
		let allTransitions = Set.union initialTransitions convertedTransitions in
		let allAcceptStates = Set.union rep.acceptStates (getFinalStates rep.transitions) in
		let tm: TurMachTypes.t = {
						entryAlphabet = rep.inputAlphabet;
						tapeAlphabet = allAlphabet;
						empty = empty;
						states = allStates;
						initialState = state "q00";
						transitions = allTransitions;
						acceptStates = allAcceptStates;
						criteria = true;
						markers = Set.empty
					}	in
		new TuringMachine.model (Arg.Representation tm)
	
	let test0 () = (* not a pushdown automaton - will change*)
		let pda = new PushdownAutomaton.model (Arg.Text pda_astar) in
			let j = pda#toJSon in
				JSon.show j

	let test1 () =
		let pda = new PushdownAutomaton.model (Arg.Text aNbNExample) in
		let tm = pda2tm pda in
		let j = tm#toJSon in
			JSon.show j

	let runAll =
		if Util.testing active "PushdownAutomaton" then begin
			Util.header "PushdownAutomatonTests starting...";
			test1 ()
		end
end

#endif
