#ifdef ALL

(*
 * TuringMachine.ml
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
 *  Written by Miguel Lourenço (ml)
 *)

(*
 * ChangeLog:
 *
 * ???/2022 (ml) - ???.
 * jun/2022 (amd) - Initial skeleton.
 *)

(*
 * Description: Turing machine functionality.
 *)

open BasicTypes

module TurMachTypes =
struct
	type transition = state * symbol * state * symbol * direction
	type transitions = transition set
	type tx = {
		alphabet : symbol list;
		states : state list;
		initialState : state;
		transitions : transition list;
		acceptStates : state list
	}
	type t = {
		alphabet : symbols;
		states : states;
		initialState : state;
		transitions : transitions;
		acceptStates : states
	}
end

open TurMachTypes

module type TuringMachineSig = sig
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

module TuringMachine : TuringMachineSig =
struct
	let modelDesignation = "turing machine"

	let internalize (tm: tx): t = {
		alphabet = Set.make tm.alphabet;
		states = Set.make tm.states;
		initialState = tm.initialState;
		transitions = Set.make tm.transitions;
		acceptStates = Set.make tm.acceptStates
	}

	let externalize (tm: t): tx = {
		alphabet = Set.toList tm.alphabet;
		states = Set.toList tm.states;
		initialState = tm.initialState;
		transitions = Set.toList tm.transitions;
		acceptStates = Set.toList tm.acceptStates
	}

	let fromJSon (j: JSon.t): t =
		if JSon.isNull j || not (JSon.hasField j "kind") then {
			alphabet = Set.empty;
			states = Set.make [draftState];
			initialState = draftState;
			transitions = Set.empty;
			acceptStates = Set.empty
		}
		else {
			alphabet = JSon.fieldSymbolSet j "alphabet";
			states = JSon.fieldStateSet j "states";
			initialState = JSon.fieldState j "initialState";
			transitions = JSon.fieldTMTransitionSet j "transitions";
			acceptStates = JSon.fieldStateSet j "acceptStates"
		}

	let toJSon (rep: t): JSon.t =
		JSon.makeAssoc [
			("alphabet", JSon.makeSymbolSet rep.alphabet);
			("states", JSon.makeStateSet rep.states);
			("initialState", JSon.makeState rep.initialState);
			("transitions", JSon.makeTMTransitionsSet rep.transitions);
			("acceptStates", JSon.makeStateSet rep.acceptStates)
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

			method validate: unit =
				Error.error self#id.Entity.name
					"The alphabet contains epsilon '~', and it should not" ()


			method tracing : unit = ()
						
			method accept(w: word): bool =
				false

			method generate (length: int): words =
				Set.empty	
		
		(* Learn-OCaml support *)
		(* incomplete *)
			method moduleName =
				"TuringMachine"

			method xTypeName =
				"turingMachine"
				
			method xTypeDeclString : string = ""

			method toDisplayString (name: string): string = ""

			method example : JSon.t =
				JSon.parse {|
				{
					kind : "turing machine"
				}
			|}
	end
end
	
module TuringMachineTests : sig end =
struct
	let active = false

	let tm_astar = {| {
			kind : "turing machine",
			description : "this is an example",
			name : "tm_astar",
			alphabet: ["a"],
			states : ["START"],
			initialState : "START0",
			transitions : [
				["START", "a", "START", "b", "L"]
			],
			acceptStates : ["START"]
			} |}
	
	let test0 () = (* not a pushdown automaton - will change*)
		let tm = new TuringMachine.model (Arg.Text tm_astar) in
			let j = tm#toJSon in
				JSon.show j

	let runAll =
		if Util.testing active "TuringMachine" then begin
			Util.header "TuringMachineTests starting...";
			test0 ()
		end
end

#endif
