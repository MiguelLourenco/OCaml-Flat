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
		entryAlphabet: symbol list;
		tapeAlphabet: symbol list;
		empty: symbol;
		states: state list;
		initialState: state;
		transitions: transition list;
		acceptStates: state list;
		criteria: bool;
		markers: symbol list
	}
	
	type t = {
		entryAlphabet: symbols;
		tapeAlphabet: symbols;
		empty: symbol;
		states: states;
		initialState: state;
		transitions: transitions;
		acceptStates: states;
		criteria: bool; (* true = acceptStates | false = stop *)
		markers: symbols
	}
	type configuration = state * symbol list * symbol list
	type configurations = configuration set

	type path = configuration list
	type paths = path list

end

open TurMachTypes

module type TuringMachineSig = sig
	val modelDesignation: string

	val getDefaultMachine: t

	val transitionGet1: transition set -> state set
	val transitionGet2: transition set -> symbol set
	val transitionGet3: transition set -> state set
	val transitionGet4: transition set -> symbol set

	val configurationGet1: configurations -> state set
	val configurationGet2: configurations -> symbol list set
	val configurationGet3: configurations -> symbol list set

	val reverse: symbol list -> symbol list

	val printConfiguration: configuration -> unit
	val printConfigurations: configurations -> unit

	class model:
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors: string list
				method handleErrors: unit
				method toJSon: JSon.t
				method representation: t
				method representationx: tx
				method validate: unit
				
				method tracing: unit

				method acceptOld: word -> bool
				method accept: word -> bool
				method acceptFull: word -> bool * configuration list * configuration set list
				method generate: int -> words	
				method downgradeModelToFiniteAutomaton: FiniteAutomaton.model
				method reachable: state -> states
				method productive: states
				method getUsefulStates: states
				method getUselessStates: states
				method isDeterministic: bool
				method areAllStatesUseful: bool
				method cleanUselessStates: t
				method acceptLB: word -> bool
				method acceptFullLB: word -> bool * configuration list * configuration set list
				method isLB: bool
				method convertToStopCriteria: model
				method hasState: state -> bool
				method hasTransition: transition -> bool
				method isFinal: state -> bool
				method isInitial: state -> bool
				method addState: state -> t
				method addInitialState: state -> t
				method addFinalState: state -> t
				method removeState: state -> t
				method changeStateToInitial: state -> t
				method changeStateToFinal: state -> t
				method changeStateFromFinal: state -> t
				method addTransition: transition -> t
				method removeTransition: transition -> t
				method renameState: state -> state -> t
			
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
				method example : JSon.t			
	end
end

module TuringMachine : TuringMachineSig =
struct

		(*-----------------------Types--------------------------------*)

	type configuration = state * symbol list * symbol list
	type configurations = configuration set

	type path = configuration list
	type paths = path list

		(*-----------------------Export functions--------------------------------*)

	let modelDesignation = "turing machine"

	let getDefaultMachine = {
		entryAlphabet = Set.empty;
		tapeAlphabet = Set.add empty Set.empty;	
		empty = empty;
		states = Set.add "START" Set.empty;
		initialState = "START";
		transitions = Set.empty;
		acceptStates = Set.empty;
		criteria = false;
		markers = Set.empty
	}
	
	let transitionGet1 trns = Set.map ( fun (a,_,_,_,_) -> a ) trns
	let transitionGet2 trns = Set.map ( fun (_,b,_,_,_) -> b ) trns
	let transitionGet3 trns = Set.map ( fun (_,_,c,_,_) -> c ) trns
  let transitionGet4 trns = Set.map ( fun (_,_,_,d,_) -> d ) trns
	let configurationGet1 configs =  Set.map ( fun (a,_,_) -> a ) configs
	let configurationGet2 configs =  Set.map ( fun (_,b,_) -> b ) configs
	let configurationGet3 configs =  Set.map ( fun (_,_,c) -> c ) configs

	let rec reverse l =
		match l with
		| [] -> []
		| x::xs -> (reverse xs)@[x]

	let printConfiguration config =
		let (state, left, right) = config in
		Util.print ["("; state2str state; ", "];
		List.iter (fun x -> Util.print [symb2str x]) left;
		Util.print [", "];
		List.iter (fun x -> Util.print [symb2str x]) right;
		Util.print [")"]

	let printConfigurations configs = 
		Util.println ["printing configs"];
		Set.iter (fun x -> printConfiguration x) configs;
		Util.println [string_of_int (Set.size configs)]

	(*-----------------------Auxiliary functions--------------------------------*)

	let internalize (tm: tx): t = {
		entryAlphabet = Set.make tm.entryAlphabet;
		tapeAlphabet = Set.make tm.tapeAlphabet;	
		empty = tm.empty;	
		states = Set.make tm.states;
		initialState = tm.initialState;
		transitions = Set.make tm.transitions;
		acceptStates = Set.make tm.acceptStates;
		criteria = tm.criteria;
		markers = Set.make tm.markers
	}

	let externalize (tm: t): tx = {
		entryAlphabet = Set.toList tm.entryAlphabet;
		tapeAlphabet = Set.toList tm.tapeAlphabet;	
		empty = tm.empty;
		states = Set.toList tm.states;
		initialState = tm.initialState;
		transitions = Set.toList tm.transitions;
		acceptStates = Set.toList tm.acceptStates;
		criteria = tm.criteria;
		markers = Set.toList tm.markers
	}

	let fromJSon (j: JSon.t): t =
		if JSon.isNull j || not (JSon.hasField j "kind") then {
			entryAlphabet = Set.empty;
			tapeAlphabet = Set.make [empty];	
			empty = empty;
			states = Set.make [draftState];
			initialState = draftState;
			transitions = Set.empty;
			acceptStates = Set.empty;
			criteria = false;
			markers = Set.empty
		}
		else {
			entryAlphabet = JSon.fieldSymbolSet j "entryAlphabet";
			tapeAlphabet = JSon.fieldSymbolSet j "tapeAlphabet";
			empty = JSon.fieldSymbol j "empty";
			states = JSon.fieldStateSet j "states";
			initialState = JSon.fieldState j "initialState";
			transitions = JSon.fieldTMTransitionSet j "transitions";
			acceptStates = JSon.fieldStateSet j "acceptStates";
			criteria = JSon.fieldBool j "criteria";
			markers = JSon.fieldSymbolSet j "markers"
		}

	let toJSon (rep: t): JSon.t =
		JSon.makeAssoc [
			("entryAlphabet", JSon.makeSymbolSet rep.entryAlphabet);
			("tapeAlphabet", JSon.makeSymbolSet rep.tapeAlphabet);
			("empty", JSon.makeSymbol rep.empty);
			("states", JSon.makeStateSet rep.states);
			("initialState", JSon.makeState rep.initialState);
			("transitions", JSon.makeTMTransitionsSet rep.transitions);
			("acceptStates", JSon.makeStateSet rep.acceptStates);
			("criteria", JSon.makeBool rep.criteria);
			("markers", JSon.makeSymbolSet rep.markers)
		]	
	
	let getMarkers rep = (Set.nth rep.markers 0, Set.nth rep.markers 1)
	let transitionsTm2Fa trns = Set.map ( fun (a,b,c,_,_) -> (a,b,c) ) trns
	let transitionGetSymbs trns = Set.union (Set.map (fun (_,b,_,_,_) -> b) trns) (Set.map (fun (_,_,_,d,_) -> d) trns)

	let rec acceptX left right st trs seen limit = 
		if limit = 0 then 
			state "~"
		else
			let newLimit = limit - 1 in
			let config = (reverse left)@[symb (state2str st)]@right in
				if Set.belongs config seen then state "~"
				else
					let newSeen = Set.add config seen in
					match left, right with
					| [], [] -> st
					| [], x::xs -> acceptX [empty] right st trs newSeen newLimit
					| x::xs, [] -> acceptX left [empty] st trs newSeen newLimit
					| x::xs, y::ys -> let getTransi = Set.filter (fun (a,b,_,_,_) -> st = a && y = b) trs in
														if Set.isEmpty getTransi then st
														else 
															let (_,_,nst,nsymb,dir) = Set.nth getTransi 0 in
																if dir = R then
																	let newLeft = nsymb::left in
																		acceptX newLeft ys nst trs newSeen newLimit;
																else
																	let newRight = x::nsymb::right in
																		acceptX xs newRight nst trs	newSeen newLimit

	let acceptOld w rp =
		let bWord = w@[empty] in
			let lastST: state = acceptX [empty] bWord rp.initialState rp.transitions Set.empty 100 in
				if rp.criteria then Set.exists (fun x -> x = lastST) rp.acceptStates
				else if lastST = state "~" then false
				else true

	(* Config de erro *)
	let stopConfig = (state "~", [], [])

	let initialConfigs tm w: configuration set =
		Set.make [(tm.initialState, [empty], w@[empty])]

	let isAcceptingConfig tm (s,l,r): bool =
		if tm.criteria then Set.belongs s tm.acceptStates
		else 
			let nextSymb = if not ((List.length r) = 0) then List.nth r 0 else empty in
			let stateTransitions = Set.filter (fun (a,b,_,_,_) -> a = s && b = nextSymb) tm.transitions in
				Set.isEmpty stateTransitions

	let next (a,b,c,d,e) (s,l,r) = 
		let l = if l = [] then [empty] else l in
		let r = if r = [] then [empty] else r in
		match l, r with
		| l::ls, r::rs -> if a = s && b = r then 
												if e = R then Set.make [(c,d::l::ls,rs)]
												else Set.make [(c,ls,l::d::rs)]
											else Set.empty
		| _, _ -> failwith "next"
	
	let getWord (s,l,r) = word "_"

	let nextConfigs tm (s,l,r): configuration set = 
		Set.flatMap (fun (a,b,c,d,e) -> next (a,b,c,d,e) (s,l,r)) tm.transitions 
	
	let accept w rp = 
		Model.accept rp w initialConfigs nextConfigs isAcceptingConfig

	let acceptFull w rp = 
		Model.acceptFull rp w initialConfigs nextConfigs isAcceptingConfig

	let generate length rp = 
		Model.generateDumb rp rp.entryAlphabet length initialConfigs nextConfigs isAcceptingConfig

	let isLB rep = 

		let hasMarkers = (Set.size rep.markers) = 2 in

		if hasMarkers then
			let (leftMarker, rightMarker) = getMarkers rep in
			let boundedDirection mark rev = Set.exists (fun (_,b,_,d,e) -> b = mark && (d != mark || e != rev)) rep.transitions in

			let bounded = (boundedDirection leftMarker R) && (boundedDirection rightMarker L) in
				bounded
		else
			hasMarkers

	let validate rep id = (

		let validInitSt = Set.belongs rep.initialState rep.states in

		let validAccSts = Set.subset rep.acceptStates rep.states in

		let currentSt = transitionGet1 rep.transitions in
		let readSy = transitionGet2 rep.transitions in
		let newSt = transitionGet3 rep.transitions in
		let writeSy = transitionGet4 rep.transitions in

		let alpha = Set.union (Set.make [empty]) rep.tapeAlphabet in
	
		let validTrns = (Set.subset currentSt rep.states) &&
								(Set.subset newSt rep.states) && 
								(Set.subset readSy alpha) &&
								(Set.subset writeSy alpha) in

		let emptyInAlph = Set.belongs empty rep.tapeAlphabet in

		let emptyIsEmpty = rep.empty = empty in

		let markersSize = ((Set.size rep.markers) = 2 || (Set.size rep.markers) = 0) in 

		if not validInitSt then
			Error.error id.Entity.name
				"The initial state does not belong to the set of all states" ()
		;

		if not validAccSts then
			Error.error id.Entity.name
				"Some accept states do not belong to the set of all states" ()
		;

		if not validTrns then
			Error.error id.Entity.name
				"Some transitions are invalid" ()
		;

		if not emptyInAlph then
			Error.error id.Entity.name
				"Empty symbol isn't in the tape alphabet" ()
		;

		if not emptyIsEmpty then
			Error.error id.Entity.name
				"The empty symbol is not correct, change it to 'B'" ()
		;
		if not markersSize then
			Error.error id.Entity.name
				"Too little or too many markers given" ()
		)

	let downgradeModelToFiniteAutomaton rep = 
		let alphaB = Set.union (Set.make [empty]) rep.tapeAlphabet in
		let fa: FinAutTypes.t = {
				alphabet = alphaB;
				states = rep.states;
				initialState = rep.initialState;
				transitions = transitionsTm2Fa rep.transitions;
				acceptStates = rep.acceptStates
			} in
		new FiniteAutomaton.model (Arg.Representation fa)

	let reachable s rep = 
		let fa = downgradeModelToFiniteAutomaton rep in
			fa#reachable s

	let productive rep =
		if rep.criteria then
			let fa = downgradeModelToFiniteAutomaton rep in
				fa#productive
		else rep.states
		
	let getUsefulStates rep =
		Set.inter (productive rep) (reachable rep.initialState rep)

	let getUselessStates rep =
		Set.diff rep.states (getUsefulStates rep)

	let isDeterministic rep =
		let fa = downgradeModelToFiniteAutomaton rep in
			fa#isDeterministic

	let cleanUselessStates rep =
		let usfSts = getUsefulStates rep in
		let usfTrs = Set.filter (fun (a,_,c,_,_) -> Set.belongs a usfSts && Set.belongs c usfSts) rep.transitions in
		let tapeAlf = Set.add empty (transitionGetSymbs usfTrs) in
		let entryAlf = Set.inter tapeAlf rep.entryAlphabet in
		let newAccSts = Set.inter rep.acceptStates usfSts in
			{
				entryAlphabet = entryAlf;
				tapeAlphabet = tapeAlf;
				empty = rep.empty;
				states = usfSts;
				initialState = rep.initialState;
				transitions = usfTrs;
				acceptStates = newAccSts;
				criteria = rep.criteria;
				markers = rep.markers
			} 

	let areAllStatesUseful rep =
		let fa = downgradeModelToFiniteAutomaton rep in
			fa#areAllStatesUseful

	let acceptLB w rep = 
		if isLB rep then
			let (leftMarker, rightMarker) = getMarkers rep in
			let newWord = [leftMarker]@w@[rightMarker] in
				accept newWord rep
		else
			false

	let acceptFullLB w rep =
		if isLB rep then
			let (leftMarker, rightMarker) = getMarkers rep in
			let newWord = [leftMarker]@w@[rightMarker] in
				acceptFull newWord rep
		else
			(false,[],[])

	let convertToStopCriteriaOld rep =
		let stEnd = state "END" in
		let endState = stEnd in
		let completeStates = Set.union (rep.states) (Set.make [endState]) in

		let newAlph = Set.union (rep.tapeAlphabet) (Set.make [empty]) in
		let nonAcceptStates =  Set.filter (fun x -> not (Set.exists (fun y -> y = x) rep.acceptStates)) rep.states in

		let missingSymbols st = Set.filter (fun x -> not (Set.exists (fun (a,b,_,_,_) -> a = st && b = x) rep.transitions)) newAlph in
		let createTransitions st = Set.map (fun x -> (st,x,endState,x,R)) (missingSymbols st) in
		let newTransList = Set.flatten (Set.map (fun x -> createTransitions x) nonAcceptStates) in
		let fullTransitions = Set.union (rep.transitions) (newTransList) in
			{
				entryAlphabet = rep.entryAlphabet;
				tapeAlphabet = rep.tapeAlphabet;
				empty = rep.empty;
				states = completeStates;
				initialState = rep.initialState;
				transitions = fullTransitions;
				acceptStates = Set.empty;
				criteria = false;
				markers = rep.markers
			}

	let convertToStopCriteria rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = rep.initialState;
			transitions = rep.transitions;
			acceptStates = Set.empty;
			criteria = false;
			markers = rep.markers
		}

	let hasState st rep = 
		Set.belongs st rep.states

	let hasTransition trs rep =
		Set.belongs trs rep.transitions

	let isFinal st rep = 
		Set.belongs st rep.acceptStates

	let isInitial st rep = 
		st = rep.initialState
	
	let addState s rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = Set.add s rep.states;
			initialState = rep.initialState;
			transitions = rep.transitions;
			acceptStates = rep.acceptStates;
			criteria = rep.criteria;
			markers = rep.markers
		}
			
	let addInitialState s rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = Set.add s rep.states;
			initialState = s;
			transitions = rep.transitions;
			acceptStates = rep.acceptStates;
			criteria = rep.criteria;
			markers = rep.markers
		}

	let addFinalState s rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = Set.add s rep.states;
			initialState = rep.initialState;
			transitions = rep.transitions;
			acceptStates = Set.add s rep.acceptStates;
			criteria = true;
			markers = rep.markers
		}

	let removeState s rep =
		if s != rep.initialState then
			{
				entryAlphabet = rep.entryAlphabet;
				tapeAlphabet = rep.tapeAlphabet;
				empty = rep.empty;
				states = Set.remove s rep.states;
				initialState = rep.initialState;
				transitions = Set.filter (fun (a,_,c,_,_) -> a = s || c = s) rep.transitions;
				acceptStates = Set.remove s rep.acceptStates;
				criteria = rep.criteria;
				markers = rep.markers
			}
		else 
			rep

	let changeStateToInitial s rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = s;
			transitions = rep.transitions;
			acceptStates = rep.acceptStates;
			criteria = rep.criteria;
			markers = rep.markers
		}

	let changeStateFromFinal s rep =
		let newAcceptSts = Set.remove s rep.acceptStates in
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = rep.initialState;
			transitions = rep.transitions;
			acceptStates = newAcceptSts;
			criteria = if (Set.size newAcceptSts) = 0 then false else true;
			markers = rep.markers
		}

	let changeStateToFinal s rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = rep.initialState;
			transitions = rep.transitions;
			acceptStates = Set.add s rep.acceptStates;
			criteria = true;
			markers = rep.markers
		}

	let renameState st name rep =
		let initial = if st = rep.initialState then name else rep.initialState in
		let newStates = Set.remove st (Set.add name rep.states) in
		let newTransitions = Set.map (fun (s,a,t,b,c) -> 
			if s = st && t = st then (name,a,name,b,c) 
			else if s = st then (name,a,t,b,c) 
			else if t = st then (s,a,name,b,c) 
			else (s,a,t,b,c)
		) rep.transitions in
		let newAcceptStates = Set.map (fun s -> if s = st then name else s) rep.acceptStates in
			{
				entryAlphabet = rep.entryAlphabet;
				tapeAlphabet = rep.tapeAlphabet;
				empty = rep.empty;
				states = newStates;
				initialState = initial;
				transitions = newTransitions;
				acceptStates = newAcceptStates;
				criteria = true;
				markers = rep.markers
			}

	let addTransition trs rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = rep.initialState;
			transitions = Set.add trs rep.transitions;
			acceptStates = rep.acceptStates;
			criteria = rep.criteria;
			markers = rep.markers
		}

	let removeTransition trs rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = rep.initialState;
			transitions = Set.remove trs rep.transitions;
			acceptStates = rep.acceptStates;
			criteria = rep.criteria;
			markers = rep.markers
		}

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super
			val representation: t =
				match arg with
				| Arg.Representation r -> r
				| Arg.RepresentationX r -> internalize r
				| _ -> fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors

			method representation: t =
				representation

			method representationx: tx =
				externalize representation

			method toJSon: JSon.t =
				JSon.append (super#toJSon) (toJSon representation)

			method validate: unit = 
				validate representation self#id
				
			method tracing : unit = 
				()
						
			method acceptOld(w: word): bool =
				acceptOld w representation

			method accept (w: word): bool =
				accept w representation

			method acceptFull (w:word): bool * 'c list * 'c set list =
				acceptFull w representation

			method generate (length: int): words =
				generate length representation

			method reachable (s:state): states =
				reachable s representation

			method productive : states =
				productive representation
				
			method getUsefulStates: states =
				getUsefulStates representation

			method getUselessStates: states =
				getUselessStates representation

			method isDeterministic: bool =
				isDeterministic representation

			method cleanUselessStates: t =
				cleanUselessStates representation

			method areAllStatesUseful: bool =
				areAllStatesUseful representation

			method acceptLB (w: word) : bool =
				acceptLB w representation

			method acceptFullLB (w: word) : bool * 'c list * 'c set list =
				acceptFullLB w representation

			method isLB : bool = 	
				isLB representation

			method convertToStopCriteria: model =
				let tm = convertToStopCriteria representation in
					new model (Arg.Representation tm)

			method hasState(s: state): bool =
				hasState s representation

			method hasTransition (trs: transition): bool =
				hasTransition trs representation

			method isFinal (st: state): bool =
				isFinal st representation

			method isInitial (st: state): bool =
				isInitial st representation

			method addState (s: state) : t =
				addState s representation

			method addInitialState (s: state) : t =
				addInitialState s representation

			method addFinalState (s: state) : t =
				addFinalState s representation
		
			method removeState (s: state) : t =
				removeState s representation

			method changeStateToInitial (s: state) : t =
				changeStateToInitial s representation

			method changeStateToFinal (s: state) : t =
				changeStateToFinal s representation

			method changeStateFromFinal (s: state) : t =
				changeStateFromFinal s representation
			
			method renameState (s:state) (newS:state): t =
				renameState s newS representation

			method addTransition (trs:transition) : t =
				addTransition trs representation

			method removeTransition (trs:transition) : t =
				removeTransition trs representation

			method downgradeModelToFiniteAutomaton: FiniteAutomaton.model =
				downgradeModelToFiniteAutomaton representation
			
			method checkProperty prop =
				match prop with
					| "deterministic" -> self#isDeterministic
					| "linear bounded" -> self#isLB
					| "acceptance by states" -> representation.criteria
					| "acceptance by stop" -> not representation.criteria
					| "turing machine" -> true
					| _ -> super#checkProperty prop
		
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
	let active = true

	(* usar exemplos 1,2,4,10 *)

	(*
	Primeiro exemplo dos slides do professor - Troca os a's pelos b's
	Este exemplo e:
		- determinista
		- nao entra em loop de configuracao
		- nao corre infinitamente sem repetir configuracao
		- nao tem estados useless
		- termina por paragem
	}
	*)
	let tm_astar1 = {| {
		kind: "turing machine",
		description: "this is an example",
		name: "tm_astar1",
		entryAlphabet: ["a", "b"],
		tapeAlphabet: ["a", "b","B"],
		empty: "B",
		states: ["q1", "q2"],
		initialState: "q1",
		transitions: [
			["q1", "B", "q2", "B", "L"],
			["q1", "a", "q1", "b", "R"],
			["q1", "b", "q1", "a", "R"],
			["q2", "a", "q2", "a", "L"],
			["q2", "b", "q2", "b", "L"]
		],
		acceptStates: [],
		criteria: "false",
		markers: [],
		} |}


	(*
	Segundo exemplo dos slides do professor - Transforma a fita BuB em BuBuB
	Este exemplo e:
		- determinista
		- nao entra em loop de configuracao
		- nao corre infinitamente sem repetir configuracao
		- nao tem estados useless
		- termina por paragem
	*)
	let tm_astar2 = {| {
		kind: "turing machine",
		description: "this is an example",
		name: "tm_astar2",
		entryAlphabet: ["a", "b"],
		tapeAlphabet: ["a", "b", "X", "Y","B"],
		empty: "B",
		states: ["q1", "q2", "q3", "q4", "q5", "q6", "q7"],
		initialState: "q1",
		transitions: [
			["q1", "a", "q2", "X", "R"],
			["q1", "b", "q5", "Y", "R"],
			["q1", "B", "q7", "B", "L"],

			["q2", "a", "q2", "a", "R"],
			["q2", "b", "q2", "b", "R"],
			["q2", "B", "q3", "B", "R"],

			["q3", "a", "q3", "a", "R"],
			["q3", "b", "q3", "b", "R"],
			["q3", "B", "q4", "a", "L"],

			["q4", "a", "q4", "a", "L"],
			["q4", "b", "q4", "b", "L"],
			["q4", "B", "q4", "B", "L"],
			["q4", "X", "q1", "X", "R"],
			["q4", "Y", "q1", "Y", "R"],

			["q5", "a", "q5", "a", "R"],
			["q5", "b", "q5", "b", "R"],
			["q5", "B", "q6", "B", "R"],

			["q6", "a", "q6", "a", "R"],
			["q6", "b", "q6", "b", "R"],
			["q6", "B", "q4", "b", "L"],

			["q7", "X", "q7", "a", "L"],
			["q7", "Y", "q7", "b", "L"]
		],
		acceptStates: [],
		criteria: "false",
		markers: []
		} |}

	(*
		Terceiro exemplo dos slides do stor - Aceita a palavra (a + b)*aa(a + b)*
		Este exemplo e:
			- determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por estados de aceitacao
	*)
	let tm_astar3 = {| {
			kind: "turing machine",
			description: "this is an example changed",
			name: "tm_astar3",
			entryAlphabet: ["a", "b"],
			tapeAlphabet: ["a", "b","B"],
			empty: "B",
			states: ["q1", "q2", "q3"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q2", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q2", "a", "q3", "a", "R"],
				["q2", "b", "q1", "b", "R"]
			],
			acceptStates: ["q3"],
			criteria: "true",
			markers: []
			} |}

	(*
		Quarto exemplo dos slides do professor - Aceita a palavra a(i)b(i)c(i) para i >= 0
		Este exemplo e:
			- determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless	
			- termina por estados de aceitacao
	*)
	let tm_astar4 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar4",
			entryAlphabet: ["a", "b", "c"],
			tapeAlphabet: ["a", "b", "c", "X", "Y", "Z", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4", "q5", "q6"],
			initialState: "q1",
			transitions: [
				["q1", "B", "q6", "B", "R"],
				["q1", "Y", "q5", "Y", "R"],
				["q1", "a", "q2", "X", "R"],

				["q2", "a", "q2", "a", "R"],
				["q2", "Y", "q2", "Y", "R"],
				["q2", "b", "q3", "Y", "R"],

				["q3", "b", "q3", "b", "R"],
				["q3", "Z", "q3", "Z", "R"],
				["q3", "c", "q4", "Z", "L"],

				["q4", "Z", "q4", "Z", "L"],
				["q4", "Y", "q4", "Y", "L"],
				["q4", "b", "q4", "b", "L"],
				["q4", "a", "q4", "a", "L"],

				["q5", "Y", "q5", "Y", "R"],
				["q5", "Z", "q5", "Z", "R"],
				["q5", "B", "q6", "B", "R"]
			],
			acceptStates: ["q6"],
			criteria: "true",
			markers: []
			} |}

	(*
		Quinto exemplo dos slides do professor - Aceita a palavra (a + b)*aa(a + b)* por paragem (Semelhante ao exemplo 3)
		Este exemplo e:
			- nao determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por paragem
	*)
	let tm_astar5 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar5",
			entryAlphabet: ["a", "b"],
			tapeAlphabet: ["a", "b", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q2", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q1", "B", "q4", "B", "R"],

				["q2", "a", "q3", "a", "R"],
				["q2", "b", "q1", "b", "R"],
				["q2", "B", "q4", "B", "R"],

				["q4", "a", "q4", "a", "R"],
				["q4", "b", "q4", "b", "R"],
				["q4", "B", "q4", "B", "R"]
			],
			acceptStates: [],
			criteria: "false",
			markers: []
			} |}

	(*
		Exemplo nao determinista dos slides - Aceita todas as palavras contendo um c precedido ou seguido de ab
		Este exemplo e:
			- nao determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por estados de aceitacao
	*)
	let tm_astar6 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar6",
			entryAlphabet: ["a", "b", "c"],
			tapeAlphabet: ["a", "b", "c", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4", "q5", "q6", "q7"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q1", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q1", "c", "q1", "c", "R"],

				["q1", "c", "q2", "c", "R"],
				["q1", "c", "q5", "c", "L"],

				["q2", "a", "q3", "a", "R"],

				["q3", "b", "q4", "b", "R"],

				["q5", "b", "q6", "b", "L"],

				["q6", "a", "q7", "a", "L"]
			],
			acceptStates: ["q4", "q7"],
			criteria: "true",
			markers: []
			} |}

	(*
		Primeiro exemplo original
		Este exemplo e:
			- nao determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por estado de aceitacao
	*)
	let tm_astar7 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar7",
			entryAlphabet: ["a", "b", "c", "d", "e"],
			tapeAlphabet: ["a", "b", "c", "d", "e", "B"],
			empty: "B",
			states: ["q1", "q2", "q3"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q2", "a", "R"],

				["q1", "a", "q1", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q1", "c", "q1", "c", "R"],
				["q1", "d", "q1", "d", "R"],
				["q1", "e", "q1", "e", "R"],

				["q2", "c", "q3", "c", "R"]
			],
			acceptStates: ["q3"],
			criteria: "true",
			markers: []
			} |}

	(*
		Segundo exemplo original
		Este exemplo e:
			- determinista
			- entra em loop de configuracao quando a palavra e vazia -> ""
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por estados de aceitacao
	*)
	let tm_astar8 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar8",
			entryAlphabet: ["a"],
			tapeAlphabet: ["a", "B"],
			empty: "B",
			states: ["q1", "q2", "q3"],
			initialState: "q1",
			transitions: [
				["q1", "B", "q2", "B", "R"],
				["q2", "B", "q1", "B", "L"],

				["q2", "a", "q3", "a", "R"]
			],
			acceptStates: ["q3"],
			criteria: "true",
			markers: []
			} |}

	(*
		Variante do exemplo 4 com estados useless
		Este exemplo e:
			- determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- tem estados useless:
				- o estado q7 e unreachable e productive
				- o estado q8 e unreachable e unproductive
				- o estado q9 e reachable e unproductive
				(Os restantes sao todos reachable e productive)
			- termina por estados de aceitacao				
	*)
	let tm_astar9 = {| {
			kind: "turing machine",
			description : "this is an example",
			name: "tm_astar9",
			entryAlphabet: ["a", "b", "c"],
			tapeAlphabet: ["a", "b", "c", "X", "Y", "Z", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"],
			initialState: "q1",
			transitions: [

				["q1", "B", "q6", "B", "R"],
				["q1", "Y", "q5", "Y", "R"],
				["q1", "a", "q2", "X", "R"],

				["q2", "a", "q2", "a", "R"],
				["q2", "Y", "q2", "Y", "R"],
				["q2", "b", "q3", "Y", "R"],

				["q3", "b", "q3", "b", "R"],
				["q3", "Z", "q3", "Z", "R"],
				["q3", "c", "q4", "Z", "L"],

				["q4", "Z", "q4", "Z", "L"],
				["q4", "Y", "q4", "Y", "L"],
				["q4", "b", "q4", "b", "L"],
				["q4", "a", "q4", "a", "L"],
				
				["q4", "X", "q1", "X", "R"],

				["q5", "Y", "q5", "Y", "R"],
				["q5", "Z", "q5", "Z", "R"],
				["q5", "B", "q6", "B", "R"],

				["q5", "b", "q9", "c", "R"],

				["q7", "b", "q8", "c", "R"],
				["q7", "B", "q6", "B", "R"]

			],
			acceptStates: ["q6"],
			criteria: "true"
			} |}

	(*
		Este exemplo e:
			- determinista
			- nao entra em loop de configuracao
			- corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por paragem
	*)
	let tm_astar10 = {| {
		kind: "turing machine",
		description: "this is an example",
		name: "tm_astar10",
		entryAlphabet: ["a", "b", "c"],
		tapeAlphabet: ["a", "b", "c", "B"],
		empty: "B",
		states: ["q1"],
		initialState: "q1",
		transitions: [
			["q1", "B", "q1", "c", "R"],
			["q1", "a", "q1", "a", "R"],
			["q1", "b", "q1", "b", "R"],
			["q1", "c", "q1", "c", "R"]
		],
		acceptStates: [],
		criteria: "false",
		markers: []
		} |}

	(*
		Este exemplo e:
			- determinista
			- entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por paragem
	*)
	let tm_astar11 = {| {
		kind : "turing machine",
		description : "this is an example",
		name : "tm_astar11",
		entryAlphabet: ["a", "b", "c"],
		tapeAlphabet: ["a", "b", "c", "B"],
		empty: "B",
		states : ["q1", "q2", "q3"],
		initialState : "q1",
		transitions : [
			["q1", "a", "q2", "c", "R"],
			["q1", "b", "q1", "b", "R"],
			["q1", "c", "q1", "a", "R"],
			["q2", "b", "q1", "b", "L"],
			["q2", "c", "q3", "c", "R"]
		],
		acceptStates : [],
		criteria : "false",
		markers: []
		} |}
	(*
		Testar:

		method representation: t
		method representationx: tx
		method toJSon: JSon.t
		method validate: unit 
		method tracing : unit
		method acceptOld(w: word): bool
		method accept (w: word): bool
		method acceptFull (w:word): bool * 'c list * 'c set list
		method generate (length: int): words 
		method reachable (s:state): states
		method productive : states
		method getUsefulStates: states
		method getUselessStates: states
		method isDeterministic: bool
		method cleanUselessStates: t
		method areAllStatesUseful: bool
		method acceptLB (w: word) : bool
		method acceptFullLB (w: word) : bool * 'c list * 'c set list
		method isLB : bool
		method convertToStopCriteria: model
		method addState (s: state) : t
		method addInitialState (s: state) : t
		method addFinalState (s: state) : t
		method removeState (s: state) : t
		method changeStateToInitial (s: state) : t
		method changeStateToFinal (s: state) : t
		method changeStateFromFinal (s: state) : t
		method renameState (s:state) (newS:state): t 
		method addTransition (trs:transition) : t
		method removeTransition (trs:transition) : t
		method downgradeModelToFiniteAutomaton: FiniteAutomaton.model

		Tipos de maquina:
		 - Determinista ou Nao
		 - Entra em loop de configuracoes ou Nao
		 - Tem estados useless ou nao
		 - Corre infinitamente sem repetir configuracao ou nao
	*)
	let print_init_msg b =
		String.concat " " ["current function:"; b]

	let check_accept f w =
		let msg = if f w then "true" else "false" in 
			Util.println [msg]

	let check_ret_bool f =
		let msg = if f then "true" else "false" in 
			Util.println [msg]

	let check_reachable f st =
		let sts : states = f st in
			Util.printStates sts

	let check_ret_states f =
		let sts : states = f in
			Util.printStates sts

	
	let test0 () =
		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
			Util.println ["++++++++++++++++++"];
			check_accept tm3#acceptOld (word "baaa");
			check_accept tm3#accept (word "baaa");
			check_accept tm3#acceptOld (word "bab");
			check_accept tm3#accept (word "bab")
(*
	let test_representation () =
	let test_representationx () =
	let test_toJSon () =
	let test_validate () =
	let test_tracing () +
	let test_acceptOld () =
	let test_accept () =
	let test_acceptFull () =
	let test_generate () =
	let test_reachable () =
	let test_productive () =
	let test_getUsefulStates () =
	let test_getUselessStates () =
	let test_isDeterministic () =
	let test_cleanUselessStates () =
	let test_areAllStatesUseful () =
	let test_acceptLB () =
	let test_acceptFullLB () =
	let test_isLB () =
	let test_convertToStopCriteria () =
	let test_addState () =
	let test_addInitialState () =
	let test_addFinalState () =
	let test_removeState () =
	let test_changeStateToInitial () =
	let test_changeStateToFinal () =
	let test_changeStateFromFinal () =
	let test_renameState () =
	let test_addTransition () =
	let test_removeTransition () =
	let test_downgradeModelToFiniteAutomaton () =
*)

(*
	For the accept, we will test if it accepts a word,
	if an automata gets stuck repeating the same configuration
	or simply keeps going infinetly
*)
	
	let testAccept () =
		Util.println [print_init_msg "accept"];

		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
		let tm5 = new TuringMachine.model (Arg.Text tm_astar5) in
		let tm10 = new TuringMachine.model (Arg.Text tm_astar10) in
		let tm11 = new TuringMachine.model (Arg.Text tm_astar11) in

			(* 
				Test accepts word / stop by acceptence
				Expeted: q3, true
			*)
			check_accept tm3#acceptOld (word "baaa");
			(* 
				Test NOT accepts word
				Expected: q1, false
			*)
			check_accept tm3#acceptOld (word "bbb");

			(* 
				Test accepts word / no accept states
				Expected: q3, true
			*)
			check_accept tm5#accept (word "aaa");
			(*
				Test NOT accepts word / no replace states (expected to be impossible)
				Expected: q1, true
			*)
			check_accept tm5#acceptOld (word "abbcc");
			(* 
				Test runs infinitely
				Expected: ~, false
			*)
			check_accept tm10#acceptOld (word "a");
			(* 
				Test get stuck in configuration loop 
				Expected: ~, false
			*)
			check_accept tm11#acceptOld (word "ab");
			(*
				Test does NOT get stuck in configuration loop
				Expected: true
			*)
			check_accept tm11#acceptOld (word "ac")

			

	let testReachable () =
		Util.println [print_init_msg "reachable"];

		let tm4 = new TuringMachine.model (Arg.Text tm_astar4) in
		let tm9 = new TuringMachine.model (Arg.Text tm_astar9) in

		 	(*
				Expected: q2, q3, q4	 
			*)
			check_reachable tm4#reachable (state "q2");
			(*
				Expected: q2, q3, q4, q5, q6, q9	 
			*)
			check_reachable tm9#reachable (state "q1");
			(*
				Expected: q7, q8, q6	 
			*)
			check_reachable tm9#reachable (state "q7")
			

	let testProductive () =
		Util.println [print_init_msg "productive"];

		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
		let tm6 = new TuringMachine.model (Arg.Text tm_astar6) in
		let tm9 = new TuringMachine.model (Arg.Text tm_astar9) in
			(*
				Expected:	 q1,q2
			*)
			check_ret_states tm3#productive;
			(*
				Expected:	 q1,q2,q3,q4,q5
			*)
		  check_ret_states tm6#productive;
			(*
				Expected:	 q1,q2,q3,q4,q5,q7
			*)
			check_ret_states tm9#productive

	let testGetUsefulStates () =
		Util.println [print_init_msg "getUsefulStates"];

		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
		let tm6 = new TuringMachine.model (Arg.Text tm_astar6) in
		let tm9 = new TuringMachine.model (Arg.Text tm_astar9) in

			(*
				Expected:	 q1,q2,q3
			*)
			check_ret_states tm3#getUsefulStates;
			(*
				Expected:	 q1,q2,q3,q4,q5,q6,q7
			*)
		  check_ret_states tm6#getUsefulStates;
			(*
				Expected:	 q1,q2,q3,q4,q5,q6
			*)
			check_ret_states tm9#getUsefulStates

	let testGetUselessStates () =
		Util.println [print_init_msg "getUselessStates"];

		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
		let tm6 = new TuringMachine.model (Arg.Text tm_astar6) in
		let tm9 = new TuringMachine.model (Arg.Text tm_astar9) in

			(*
				Expected:	
			*)
			check_ret_states tm3#getUselessStates;
			(*
				Expected:	 
			*)
		  check_ret_states tm6#getUselessStates;
			(*
				Expected:	 q1,q2,q3,q4,q5,q6
			*)
			check_ret_states tm9#getUselessStates

	let testIsDeterministic () =
		Util.println [print_init_msg "isDeterministic"];

		let tmND = new TuringMachine.model (Arg.Text tm_astar6) in
		let tmD = new TuringMachine.model (Arg.Text tm_astar4) in

			(*
				Expected:	 false
			*)
			check_ret_bool tmND#isDeterministic;
			(*
				Expected:	 true
			*)
			check_ret_bool tmD#isDeterministic

	let testAreAllStatesUseful () =
		Util.println [print_init_msg "areAllStatesUseful"];

		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
		let tm6 = new TuringMachine.model (Arg.Text tm_astar6) in
		let tm9 = new TuringMachine.model (Arg.Text tm_astar9) in

			(*
				Expected:	 true
			*)
			check_ret_bool tm3#areAllStatesUseful;
			(*
				Expected:	 true
			*)
			check_ret_bool tm6#areAllStatesUseful;
			(*
				Expected:	 false
			*)
			check_ret_bool tm9#areAllStatesUseful
		
	let testCleanUselessStates () =
		let tm = new TuringMachine.model (Arg.Text tm_astar9) in
			Util.println [print_init_msg "areAllStatesUseful"];
			let ntm = new TuringMachine.model (Arg.Representation tm#cleanUselessStates) in
				let j = ntm#toJSon in
					JSon.show j;
					Util.println []

	let runAll =
		if Util.testing active "TuringMachine" then begin
			Util.header "TuringMachineTests starting...";
			test0 ();
			(*
				testCleanUselessStates ();
				testAccept ();
				testReachable ();
				testProductive ();
				testGetUsefulStates ();
				testGetUselessStates ();
				testIsDeterministic ();
				testAreAllStatesUseful ();
				testCleanUselessStates ();
			*)
		end
end

#endif