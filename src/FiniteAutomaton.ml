(*
 * FiniteAutomaton.ml
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
 * jan/2021 (amd) - Module in an independent file and some cleanup.
 * dec/2019 (jg) - Main functionalities.
 * jun/2019 (amd) - Initial skeleton, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Finite automata functionality.
 *
 * TODO: More cleanup.
 *)

module type FiniteAutomatonSig = sig

	type transition = state * symbol * state
	type transitions = transition set
	type t = {
		alphabet : symbols;
		states : states;
		initialState : state;
		transitions : transitions;
		acceptStates : states;
	}
	val modelDesignation : string
	class model :
		t Arg.alternatives ->
			object
				method kind : string
				method description : string
				method name : string
				method errors : string list
				method handleErrors : unit
				method validate : unit
				method toJSon: JSon.t

				method tracing : unit

				method acceptBreadthFirst: word -> bool
				method accept : word -> bool
				method acceptWithTracing : word -> unit
				method generate : int -> words
				method generateUntil : int -> words
				method reachable : state -> states
				method productive : states
				method getUsefulStates : states
				method getUselessStates : states
				method cleanUselessStates: model
				method areAllStatesUseful: bool

				method toDeterministic : model
				method isDeterministic : bool
				method equivalencePartition: states set
				method minimize : model
				method isMinimized : bool

				method representation : t

				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise -> (words * words)
			end
end

module FiniteAutomaton : FiniteAutomatonSig =
struct

	type transition =
		state	(* state *)
		* symbol	(* consumed input symbol *)
		* state	(* next state *)

	type transitions = transition set

	type t = {
		alphabet: symbols;			(* Alphabet *)
		states: states;				(* States *)
		initialState: state;		(* Initial state *)
		transitions: transitions;	(* Transition relation *)
		acceptStates: states		(* Accept states *)
	}

	let modelDesignation = "finite automaton"

	(*------Auxiliary functions---------*)

	(* get starting state, symbol, and/or end state of all transitions in set *)
	let transitionGet1 trns = Set.map ( fun (a,_,_) -> a ) trns
	let transitionGet2 trns = Set.map ( fun (_,b,_) -> b ) trns
	let transitionGet3 trns = Set.map ( fun (_,_,c) -> c ) trns
	let transitionGet23 trns = Set.map (fun(_,b,c) -> (b,c)) trns

	(* fuse all states into a new state *)
	let fuseStates sts = String.concat "_" sts


	(* checks if set ts has at least one transition from state st through symbol sy *)
	let hasTrans st sy ts = Set.exists (fun (x,y,_) -> x = st && y = sy) ts

	(* returns the set of state st and all its states reachable by an epsilon transition *)
	let nextEpsilon1 st ts =
		let trns = Set.filter (fun (a,b,c) -> st = a && b = epsilon) ts in
		let nextStates = transitionGet3 trns in
			Set.add st nextStates

	(* returns the set of states sts and all states reachable from sts through epsilon transitions *)
	let rec closeEmpty sts t =
		let ns = Set.flatMap (fun st -> nextEpsilon1 st t) sts in
			if (Set.subset ns sts) then ns else closeEmpty (Set.union sts ns) t

	(* returns states reachable from st through symbol sy *)
	let nextStates st sy t =
		let n = Set.filter (fun (a,b,c) -> st = a && sy = b) t in
			transitionGet3 n

	class model (arg: t Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation

			val representation: t =
				let j = Arg.fromAlternatives arg in
					if j = JSon.JNull then
						Arg.getRepresentation arg
					else
						let alphabet = JSon.field_char_set j "alphabet" in
						let states = JSon.field_string_set j "states" in
						let initialState = JSon.field_string j "initialState" in
						let transitions = JSon.field_triples_set j "transitions" in
						let acceptStates = JSon.field_string_set j "acceptStates" in
							{	alphabet = alphabet;
								states = states;
								initialState = initialState;
								transitions = transitions;
								acceptStates = acceptStates
							}

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation =
				representation

			method toJSon: JSon.t =
				let open JSon in
				let rep = representation in
				JAssoc [
					("kind", JString self#kind);
					("description", JString self#description);
					("name", JString self#name);
					("alphabet", JList (List.map (fun c -> JString (Util.ch2str c)) (Set.toList rep.alphabet)));
					("states", JList (List.map (fun s -> JString s) (Set.toList rep.states)));
					("initialState", JString rep.initialState);
					("transitions", JList (List.map (fun (a,b,c) ->
						JList [JString a; JString (Util.ch2str b); JString c]) (Set.toList rep.transitions)));
					("acceptStates", JList (List.map (fun s -> JString s) (Set.toList rep.acceptStates)))
				]


			(**
			* This method verifies if the automaton is valid.
			* An automaton is considered valid if its initial and acceptance states belong to the set of all its states
			* and if all its transitions have states and symbols belonging to the set of all its states and its alphabet respectively.
			*
			* Desc: If the automaton is invalid, the cause could derive from any combination of the following
			* three options: either the initial state, one of the acceptance states, or one of the transitions does not follow the
			* previously discussed predicate. This method will print to the console stating which combination of these options caused
			* the automaton to be invalid
			*)
			method validate: unit = (

				(* does initial state belong to the set of all states *)
				let validInitSt = Set.belongs representation.initialState representation.states in


				(* are all accepted states members of all states *)
				let validAccSts = Set.subset representation.acceptStates representation.states in

				let fromSt = transitionGet1 representation.transitions in
				let sy = transitionGet2 representation.transitions in
				let toSt = transitionGet3 representation.transitions in
				let alpha = Set.add epsilon representation.alphabet in

				(* do all transitions have states belonging to all states and symbols belonging to the alphabet *)
				let validTrns = (Set.subset fromSt representation.states) &&
				(Set.subset sy alpha) && (Set.subset toSt representation.states) in


				if not validInitSt then
					Error.error representation.initialState
						"initial state does not belong to the set of all states" ()
				;

				if not validAccSts then
					Error.error self#name
						"not all accepted states belong to the set of all states" ()
				;

				if not validTrns then
					Error.error self#name
						"not all transitions are valid" ()
				)


			method tracing : unit = ()



			(**
			* This method verifies if the given word is accepted by the automaton
			*
			* @param w:word -> word to be tested for acceptance
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			* Desc: Checks if the automaton accepts word w using configurations (that is, pairs formed by a state and
			* a remaining word) and a breadth-first approach as to deal with potential non-termination
			*)
			method acceptBreadthFirst(w: word): bool = false
			(*
				let rec acc cf t sta =
					match cf with
						[] -> false
						|(st,[])::ls ->
							let accepts = (Set.inter (closeEmpty (Set.make [st]) t) sta) <> Set.empty in
								accepts || acc ls t sta
						|(st,x::xs)::ls ->
							let n = nextStates st x t in
							let cfn = Set.map (fun c -> (c,xs)) n in
							let n2 = nextStates st epsilon t in
							let cfn2 = Set.map (fun c -> (c,x::xs)) n2 in
								acc (Set.flatten (Set.make [ls;cfn;cfn2])) t sta in
				acc (Set.make [(representation.initialState,w)]) representation.transitions representation.acceptStates
			*)

			(**
			* This method verifies if the given word is accepted by the automaton
			*
			* @param w:word -> word to be accepted
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			* Desc: Checks if the automaton accepts word w using functions over sets of states
			*)
			method accept (w: word): bool =

				let transition sts sy t =
					let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
						Set.union nsts (closeEmpty nsts t) in

				let rec acceptX sts w t =
					match w with
						[] -> (Set.inter sts representation.acceptStates) <> Set.empty
						|x::xs -> let nextSts = transition sts x t in
							nextSts <> Set.empty && acceptX nextSts xs t in

				let i = closeEmpty (Set.make [representation.initialState]) representation.transitions in
					acceptX i w representation.transitions



			method acceptWithTracing (w:word): unit =


				let transition sts sy t =
					let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
						Set.union nsts (closeEmpty nsts t) in

				let rec acceptX sts w t =
					match w with
						[] -> [(w,sts)]
						|x::xs -> let nextSts = transition sts x t in
									let res = acceptX nextSts xs t in
										(w,sts)::res
				in

				let i = closeEmpty (Set.make [representation.initialState]) representation.transitions in

				let res = acceptX i w representation.transitions in

				let printRes w sts =
					Util.print ["('"; Util.word2str w; "',["];
					Set.iter (fun st -> Util.print [st; ";"]) sts;
					Util.print ["])"];

				in

				List.iter (fun (w,sts) -> printRes w sts; Util.print [";"]) res; Util.println []




			(**
			* This method generates all words of the given size which are accepted by the automaton
			*
			* Precondition -> length >= 0
			*
			* @param length:int -> size of all words to be generated
			*
			* @returns words -> the set of all words with size length
			*)
			method generate (length: int): words =

				(* adds symbol to the left of all words *)
				let addSyToRWords symb ws = Set.map (fun l -> symb::l) ws in

				let hasAcceptState sts accSts = Set.exists (fun st -> Set.belongs st accSts) sts in
				let nxtNonEmptyTrns st ts = Set.filter (fun (a,b,_) -> a = st && b <> epsilon) ts in

				let rec gen n state transitions accSts =

					let clsEmpty = (closeEmpty (Set.make [state]) transitions) in
					if n = 0 then
						if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty

					else
						let trnsSet = Set.flatMap (fun st -> nxtNonEmptyTrns st transitions ) clsEmpty in

						let rwords st1 l1 = gen (l1-1) st1 transitions accSts in
						let genX sy st l = addSyToRWords sy (rwords st l) in

								Set.flatMap (fun (_,sy,st) -> genX sy st n) trnsSet
				in
				gen length representation.initialState representation.transitions representation.acceptStates


			(**
			* This method generates all words up to a given size which are accepted by the automaton
			*
			* Precondition -> length >= 0
			*
			* @param length:int -> maximum size of all words to be generated
			*
			* @returns words -> the set of all words with size length or less
			*)
			method generateUntil (length: int): words =

				(* adds symbol to the left of all words *)
				let addSyToRWords symb ws = Set.map (fun l -> symb::l) ws in

				let hasAcceptState sts accSts = Set.exists (fun st -> Set.belongs st accSts) sts in
				let nxtNonEmptyTrns st ts = Set.filter (fun (a,b,_) -> a = st && b <> epsilon) ts in

				let rec gen n state transitions accSts =

					let clsEmpty = (closeEmpty (Set.make [state]) transitions) in
					if n = 0 then
						if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty

					else
						let trnsSet = Set.flatMap (fun st -> nxtNonEmptyTrns st transitions ) clsEmpty in
						let genX sy st l = addSyToRWords sy (gen (l-1) st transitions accSts) in
						let lenOneOrMore = Set.flatMap (fun (_,sy,st) -> genX sy st n) trnsSet in
						let lenZero = if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty in
							Set.union lenOneOrMore lenZero
				in
				gen length representation.initialState representation.transitions representation.acceptStates


			(**
			* This method generates all states that are reachable from the given state. A state is reachable from s if there
			* exists a word that starting on s will lead to that state
			*
			* @param s:state -> the given state
			*
			* @returns states -> the set of all states reachable from s.
			*)
			method reachable (s:state): states =

				let neighbourSts st t = transitionGet3 (Set.filter (fun (a,_,_) -> a = st) t) in
				let nextStates sts t = Set.flatMap (fun st -> neighbourSts st t) sts in
				let remain s t = Set.filter (fun (a,_,_) -> not (Set.belongs a s)) t in

				let rec reach visited s t = if visited = s then Set.empty else Set.union s ( reach s (nextStates s t) (remain s t) ) in
					reach Set.empty (Set.make [s]) representation.transitions



			(**
			* This method generates all productive states. A state is productive if there exists a word that will lead said state
			* to an acceptance state
			*
			* @returns states -> the set of all productive states
			*
			* Desc: For each state of the automaton, this method applies the reachable method and checks if any of the resulting
			* states is an acceptance state, and if it is then that state will belong to the resulting set of productive states
			*)
			method productive: states =

				let reachsAccSt st = Set.exists (fun s -> Set.belongs s representation.acceptStates ) (self#reachable st) in
					Set.filter (fun st -> reachsAccSt st) representation.states

			(**
			* This method generates the set of all useful states
			*
			* @returns states -> the set of all useful states
			*)
			method getUsefulStates: states =
				Set.inter self#productive (self#reachable representation.initialState)


			(**
			* This method generates the set of all non useful states
			*
			* @returns states -> the set of all non useful states
			*)
			method getUselessStates: states =
				Set.diff representation.states self#getUsefulStates


			(**
			* This method creates the equivalent automaton where all states are useful
			*
			* @returns FiniteAutomaton.model -> the new equivalent automaton where all states are useful
			*
			* Desc: The new automaton is created by eliminating from the original automaton all its non useful states, all transitions
			* that have a non useful state, and all symbols of the alphabet that only appear in said transitions
			*)
			method cleanUselessStates: model =

				let usfSts = self#getUsefulStates in
				let usfTrs = Set.filter (fun (a,_,c) -> Set.belongs a usfSts &&
														Set.belongs c usfSts)
								representation.transitions in

				let alf = transitionGet2 usfTrs in
				let usfAlf = Set.diff alf (Set.make [epsilon]) in
				let newAccSts = Set.inter representation.acceptStates usfSts in
				let usfSts = Set.add representation.initialState usfSts in

				new model (Arg.Representation {
								alphabet = usfAlf;
								states = usfSts;
								initialState = representation.initialState;
								transitions = usfTrs;
								acceptStates = newAccSts
						} )


			(**
			* This method verifies if all the automaton's states are useful
			*
			* @returns bool -> true if all states of the automaton are useful, false otherwise
			*)
			method areAllStatesUseful: bool =

				let usfSts = self#getUsefulStates in
					Set.size representation.states = Set.size usfSts


			(**
			* This method converts the non-deterministic automaton into its deterministic equivalent
			*
			* @returns FiniteAutomaton.model -> the new deterministic automaton
			*
			* Desc: If the automaton to determinize is already deterministic,
			* the resulting automaton will be equal to the original
			*)
			method toDeterministic: model =

				let move sts sy ts = Set.flatMap (fun st -> nextStates st sy ts ) sts in

				(* generates the set of states reachable from the given state set though the given symbol *)
				let newR oneR sy ts =
					let nxtSts = move oneR sy ts in
					let clsempty = closeEmpty nxtSts ts in
					Set.union nxtSts clsempty in

				(* creates all transitions (given state set, a given symbol, states reachable from set through given symbol) *)
				let rToTs r =
					let nxtTrans = Set.map (fun sy -> (r,sy,newR r sy representation.transitions)) representation.alphabet in
						Set.filter (fun (_,_,z) -> not (z = Set.empty)) nxtTrans in

				(* applies previous function to all state sets until no new set is generated *)
				let rec rsToTs stsD rD trnsD alph =
					let nxtTs = Set.flatMap (fun stSet -> rToTs stSet ) rD in
					let nxtRs = Set.map (fun (_,_,z) -> z) nxtTs in
					let newRs = Set.filter (fun r -> not (Set.belongs r stsD)) nxtRs in
					if newRs = Set.empty then (Set.union trnsD nxtTs) else
						rsToTs (Set.union newRs stsD) newRs (Set.union trnsD nxtTs) alph in


				let r1 = closeEmpty (Set.make [representation.initialState]) representation.transitions in

				(* all transitions of the new deterministic automaton *)
				let trnsD = rsToTs (Set.make [r1]) (Set.make [r1]) Set.empty representation.alphabet in

				let tds = Set.map (fun (a,b,c) -> (fuseStates (Set.toList a), b, fuseStates (Set.toList c))) trnsD in

				let newInitialState = fuseStates (Set.toList r1) in

				let stSet1 = Set.map (fun (a,_,_) -> a) trnsD in
				let stSet2 = Set.map (fun (_,_,c) -> c) trnsD in
				let stSet = Set.union stSet1 stSet2 in

				let isAccepState st = Set.belongs st representation.acceptStates in
				let hasAnAccepSt set = Set.exists (fun st -> isAccepState st ) set in
				let newAccStsSet = Set.filter (fun set -> hasAnAccepSt set) stSet in

				let newAllSts = Set.map (fun set -> fuseStates (Set.toList set)) stSet in
				let newAccSts = Set.map (fun set -> fuseStates (Set.toList set)) newAccStsSet in


				new model (Arg.Representation {
								alphabet = representation.alphabet;
								states = newAllSts;
								initialState = newInitialState;
								transitions = tds;
								acceptStates = newAccSts
						} )

			(**
			* This method verifies if the automaton is deterministic
			*
			* @returns bool -> true if automaton is deterministic, false otherwise
			*
			* Desc: For each state s, this method checks if there exists 2 or more transitions with the same symbol from any
			* state belonging to closeempty of s, independently of the state which said transitions will lead to.
			* If there is no state for which this property is true, then the automaton is deterministic
			*)
			method isDeterministic: bool =

				let trnsFromSt st ts = Set.filter (fun (st1,sy,_) -> st1 = st && sy <> epsilon) ts in

				let isStDeter st ts =
					let allSts = closeEmpty (Set.make [st]) ts in
					let allTs = Set.flatMap (fun st -> trnsFromSt st ts) allSts in
					let sys = transitionGet2 allTs in
						Set.size allTs = Set.size sys in

				let hasNondeterSt = Set.exists (fun st -> not (isStDeter st representation.transitions) )
										representation.states in
					not hasNondeterSt



			(* partition states by equivalence *)
			method equivalencePartition: states set =

				let fa = self#toDeterministic in

				let fa2 = fa#cleanUselessStates in


				let rep = fa2#representation in

				let (inF, notF) = Set.partition (fun x -> Set.belongs x rep.acceptStates) rep.states in
				let distI1 = Set.combinations inF notF in

				let hasTransMulti sts sy ts = Set.partition (fun st -> hasTrans st sy ts) sts in
				let distI2 = Set.flatMap (fun sy -> Util.distrib2 Set.combinations
													(hasTransMulti rep.states sy rep.transitions))
								rep.alphabet in


				let distI = Set.union distI1 distI2 in

				let stsXSts = Set.combinations rep.states rep.states in

				(* generates all pairs of states that can reach the pair (st1,st2) through a transition with symbol sy *)
				let reachingSts st1 st2 sy p =
					let t1 = Set.filter (fun (_,y,z) -> z = st1 && y = sy) rep.transitions in
					let t2 = Set.filter (fun (_,y,z) -> z = st2 && y = sy) rep.transitions in
					let s1 = transitionGet1 t1 in
					let s2 = transitionGet1 t2 in
						Set.diff (Set.combinations s1 s2) p in

				let findAR p q = Set.flatMap (fun (a,b) -> Set.flatMap (fun sy -> reachingSts a b sy p) rep.alphabet) q in

				let distA = findAR distI distI in

				let rec aped p q = if (q = Set.empty || (Set.union p q) = stsXSts) then Set.union p q
					else aped (Set.union p q) (findAR (Set.union p q) q ) in

				let dist = aped distI distA in


				(* given for example states a b c d generates (a,a) (a,b) (a,c) (a,d) (b,b) (b,c) (b,d) (c,c) (c,d) (d,d) *)
				let rec halfCombs sts =
					match sts with
						[] -> Set.empty
						|x::xs -> Set.union (Set.combinations (Set.make [x]) (Set.make sts)) (halfCombs xs) in

				let halfTriang = halfCombs (Set.toList rep.states) in

				(* given set of equivalent states dicti, substitutes state st for its leftmost equivalent state according to dicti *)
				let rec translate st dicti =
					match dicti with
						[] -> st
						|(eq1,eq2)::xs -> if eq2 = st then eq1 else translate st xs in

				(* the set of equivalent state pairs are those not present in the set of distinct state pairs *)
				let equiv = Set.filter ( fun (a,b) -> not (Set.belongs (a,b) dist) &&
														not (Set.belongs (b,a) dist) ) halfTriang in

				let equivList = Set.toList equiv in
				let hasAny st1 st2 sta stb = (translate st1 equivList) = sta || (translate st2 equivList) = sta
											|| (translate st1 equivList) = stb || (translate st2 equivList) = stb in


				let rec agroup eq =
					match eq with
						| [] -> Set.empty
						| (a,b)::ls ->
							let (part1,part2) = Set.partition (fun (x,y) -> hasAny x y a b) (Set.make eq) in
							let gRemain = Set.flatMap (fun (c,d) -> Set.make [c;d]) part1 in
								Set.add (Set.union (Set.make [a;b]) gRemain) (agroup (Set.toList part2))
				in

				agroup equivList




			(**
			* This method minimizes the automaton
			*
			* @returns FiniteAutomaton.model -> the new minimal equivalent automaton
			*
			* Desc: The given automaton is minimized according to the process described in lecture a15.
			*)
			method minimize: model =

				let fa = self#toDeterministic in

				let fa2 = fa#cleanUselessStates in


				let rep = fa2#representation in

				let (inF, notF) = Set.partition (fun x -> Set.belongs x rep.acceptStates) rep.states in
				let distI1 = Set.combinations inF notF in

				let hasTransMulti sts sy ts = Set.partition (fun st -> hasTrans st sy ts) sts in
				let distI2 = Set.flatMap (fun sy -> Util.distrib2 Set.combinations
													(hasTransMulti rep.states sy rep.transitions))
								rep.alphabet in


				let distI = Set.union distI1 distI2 in

				let stsXSts = Set.combinations rep.states rep.states in

				(* generates all pairs of states that can reach the pair (st1,st2) through a transition with symbol sy *)
				let reachingSts st1 st2 sy p =
					let t1 = Set.filter (fun (_,y,z) -> z = st1 && y = sy) rep.transitions in
					let t2 = Set.filter (fun (_,y,z) -> z = st2 && y = sy) rep.transitions in
					let s1 = transitionGet1 t1 in
					let s2 = transitionGet1 t2 in
						Set.diff (Set.combinations s1 s2) p in

				let findAR p q = Set.flatMap (fun (a,b) -> Set.flatMap (fun sy -> reachingSts a b sy p) rep.alphabet) q in

				let distA = findAR distI distI in

				let rec aped p q = if (q = Set.empty || (Set.union p q) = stsXSts) then Set.union p q
					else aped (Set.union p q) (findAR (Set.union p q) q ) in

				let dist = aped distI distA in


				(* given for example states a b c d generates (a,b) (a,c) (a,d) (b,c) (b,d) (c,d) *)
				let rec halfCombs sts =
					match sts with
						[] -> Set.empty
						|x::xs -> Set.union (Set.combinations (Set.make [x]) (Set.make xs)) (halfCombs xs) in
				let halfTriang = halfCombs (Set.toList rep.states) in

				(* given set of equivalent states dicti, substitutes state st for its leftmost equivalent state according to dicti *)
				let rec translate st dicti =
					match dicti with
						[] -> st
						|(eq1,eq2)::xs -> if eq2 = st then eq1 else translate st xs in

				(* the set of equivalent state pairs are those not present in the set of distinct state pairs *)
				let equiv = Set.filter ( fun (a,b) -> not (Set.belongs (a,b) dist) &&
														not (Set.belongs (b,a) dist) ) halfTriang in

				let equivList = Set.toList equiv in

				let eq = Set.map (fun (a,b) -> b) equiv in
				let newSts = Set.diff rep.states eq in
				let newInitSt = translate rep.initialState equivList in
				let newAccSts = Set.inter rep.acceptStates newSts in
				let newTrans = Set.map (fun (x,y,z) -> (translate x equivList,y,translate z equivList) ) rep.transitions in


				new model (Arg.Representation {
								alphabet = rep.alphabet;
								states = newSts;
								initialState = newInitSt;
								transitions = newTrans;
								acceptStates = newAccSts
						} )


			(**
			* This method verifies if the automaton is minimal
			*
			* @returns boolean -> true if automaton is minimal, false otherwise
			*
			* Desc: The given automaton is considered minimal if the result of minimizing it is an automaton with the same
			* number of states
			*)
			method isMinimized: bool =

				let fa = self#minimize in
				let rep = fa#representation in
					Set.size representation.states = Set.size rep.states


		end

end

module FiniteAutomatonTests : sig end =
struct
	let active = false

	let test0 () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
			let j = fa#toJSon in
				JSon.show j

	let testBug () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
		let fa2 = fa#toDeterministic in
			let j = fa2#toJSon in
				JSon.show j;
		let fa3 = fa2#cleanUselessStates in
			let j = fa3#toJSon in
				JSon.show j

	let testBug2 () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
		let fa2 = fa#toDeterministic in
			Util.println ["productive states:"];
			Util.printStates (Set.toList fa2#productive);
			Util.println []

	let fa_accept = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "ab123",
		alphabet : ["a", "b"],
		states : ["1", "2", "3"],
		initialState : "1",
		transitions : [
				["1","a","2"], ["1","b","3"],
				["2","b","2"],
				["3","a","3"]
			],
		acceptStates : ["2", "3"]
	} |}

	let fa_accept2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b", "c", "d"],
		states : ["START", "A", "AB", "C", "SUCCESS", "D"],
		initialState : "START",
		transitions : [
				["START","a","A"], ["START","~","AB"],
				["A","~","C"],
				["AB","b","SUCCESS"], ["AB","~","SUCCESS"],
				["C","~","SUCCESS"], ["C","d","C"],
				["SUCCESS","~","START"]
			],
		acceptStates : ["SUCCESS"]
	} |}

	let check f w =
		let msg = 
			if f w then "word was accepted" else "word was not accepted"
		in Util.println [msg]

	let testAcceptBF () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_accept) in
			check fa#acceptBreadthFirst [];
			check fa#acceptBreadthFirst ['a'];
			check fa#acceptBreadthFirst ['a';'b'];
			check fa#acceptBreadthFirst ['b'];
			check fa#acceptBreadthFirst ['b';'a'];
			check fa#acceptBreadthFirst ['a';'b';'b'];
			check fa#acceptBreadthFirst ['a';'b';'a'];
			check fa#acceptBreadthFirst ['b';'a';'a'];
			check fa#acceptBreadthFirst ['b';'a';'b'];
			Util.println []

	let testAcceptBF2 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_accept2) in
			check fa#acceptBreadthFirst [];
			check fa#acceptBreadthFirst ['a'];
			check fa#acceptBreadthFirst ['a';'d'];
			check fa#acceptBreadthFirst ['a';'b';'a';'d'];
			check fa#acceptBreadthFirst ['c'];
			Util.println []

	let testAccept () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_accept) in
			check fa#accept [];
			check fa#accept ['a'];
			check fa#accept ['a';'b'];
			check fa#accept ['b'];
			check fa#accept ['b';'a'];
			check fa#accept ['a';'b';'b'];
			check fa#accept ['a';'b';'a'];
			check fa#accept ['b';'a';'a'];
			check fa#accept ['b';'a';'b'];
			Util.println []

	let testAccept2 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_accept2) in
			check fa#accept [];
			check fa#accept ['a'];
			check fa#accept ['a';'d'];
			check fa#accept ['a';'b';'a';'d'];
			check fa#accept ['c'];
			Util.println []

	let testAccTrace () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
			fa#acceptWithTracing ['a';'b';'e']

	let fa_generate = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","b","S2"], ["S1","a","S3"], ["S1","~","S3"],
				["S2","~","S3"],
				["S3","~","S3"]
			],
		acceptStates : ["S3"]
	} |}

	let fa_generate2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2"],
		initialState : "S1",
		transitions : [
				["S1","a","S1"], ["S1","b","S2"],
				["S2","a","S2"], ["S2","b","S1"]

			],
		acceptStates : ["S2"]
	} |}

	let fa_generate3 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a"],
		states : ["S1"],
		initialState : "S1",
		transitions : [
				["S1","a","S1"]
			],
		acceptStates : ["S1"]
	} |}

	let fa_generate4 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2"],
		initialState : "S1",
		transitions : [
				["S1","a","S1"], ["S1","b","S2"],
				["S2","a","S2"]
			],
		acceptStates : ["S1"]
	} |}

	let testGenerate () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate) in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (fa#generate 0) );
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (fa#generate 1) );
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (fa#generate 2) );
			Util.println ["generated words size 100:"]; Util.printWords (Set.toList (fa#generate 100) );
			Util.println []

	let testGenerate2 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate2) in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (fa#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (fa#generate 1));
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (fa#generate 2));
			Util.println ["generated words size 3:"]; Util.printWords (Set.toList (fa#generate 3) );
			Util.println ["generated words size 4:"]; Util.printWords (Set.toList (fa#generate 4) );
			Util.println ["generated words size 18:"]; Util.printWords (Set.toList (fa#generate 18) );

			Util.println []

	let testGenerate3 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate3) in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (fa#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (fa#generate 1));
			Util.println ["generated words size 10:"]; Util.printWords (Set.toList (fa#generate 10));
			Util.println ["generated words size 50:"]; Util.printWords (Set.toList (fa#generate 50));
			Util.println ["generated words size 100:"]; Util.printWords (Set.toList (fa#generate 100));
			Util.println ["generated words size 1000:"]; Util.printWords (Set.toList (fa#generate 1000));
			Util.println []

	let testGenerate4 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate4) in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (fa#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (fa#generate 1));
			Util.println ["generated words size 10:"]; Util.printWords (Set.toList (fa#generate 10));
			Util.println ["generated words size 100:"]; Util.printWords (Set.toList (fa#generate 100));
			Util.println []

	let testGenerateUntil () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate) in
			Util.println ["generated words size 5:"]; Util.printWords (Set.toList (fa#generateUntil 5));
			Util.println [];
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate2) in
			Util.println ["generated words size 5:"]; Util.printWords (Set.toList (fa#generateUntil 5));
			Util.println [];
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate3) in
			Util.println ["generated words size 5:"]; Util.printWords (Set.toList (fa#generateUntil 5));
			Util.println [];
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate4) in
			Util.println ["generated words size 5:"]; Util.printWords (Set.toList (fa#generateUntil 5));
			Util.println []

	let fa_reach = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3"],
		initialState : "S1",
		transitions : [
			],
		acceptStates : ["S1"]
	} |}

	let fa_reach2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4","S5","S6"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","a","S3"],
				["S2","a","S2"],
				["S3","~","S4"],
				["S4","~","S5"],
				["S5","~","S3"], ["S5","b","S6"], ["S5","~","S5"]
			],
		acceptStates : ["S1"]
	} |}

	let testReachable () =
			let open FiniteAutomaton in
			let fa = new FiniteAutomaton.model (Arg.Text fa_reach) in
			let fa2 = new FiniteAutomaton.model (Arg.Text fa_reach2) in
			let start = fa#representation.initialState in
			let start2 = fa2#representation.initialState in
				Util.println ["reachable states:"]; Util.printStates (Set.toList (fa#reachable start)); Util.println [];
				Util.println ["reachable states:"]; Util.printStates (Set.toList (fa#reachable start2)); Util.println []

	let fa_productive = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","b","S3"],
				["S4","a","S2"], ["S4","b","S3"], ["S3","a","S3"]
			],
		acceptStates : ["S4"]
	} |}

	let fa_productive2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4","S5","S6","S7"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","a","S2"], ["S1","~","S3"], ["S1","a","S3"], ["S1","~","S5"], ["S1","a","S5"],
				["S2","~","S1"], ["S2","a","S1"],
				["S4","~","S3"], ["S4","a","S3"],["S4","~","S4"], ["S4","a","S4"],
				["S5","~","S2"], ["S5","a","S2"],["S5","~","S6"], ["S5","a","S6"],
				["S6","~","S6"], ["S6","a","S6"],["S6","~","S7"], ["S6","a","S7"],
				["S7","~","S3"], ["S7","a","S3"],["S7","~","S5"], ["S7","a","S5"]
			],
		acceptStates : ["S2","S4"]
	} |}

	let testProductive () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_productive) in
		let fa2 = new FiniteAutomaton.model (Arg.Text fa_productive2) in
			Util.println ["productive states:"]; Util.printStates (Set.toList (fa#productive)); Util.println [];
			Util.println ["productive states:"]; Util.printStates (Set.toList (fa2#productive)); Util.println []


	let fa_clean = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","b","S3"],
				["S4","a","S2"], ["S4","b","S3"], ["S3","a","S3"]
			],
		acceptStates : ["S4"]
	} |}

	let fa_clean2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","~","S3"],
				["S3","a","S2"], ["S3","~","S1"], ["S3","a","S4"]
			],
		acceptStates : ["S2"]
	} |}

	let testClean () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_clean) in
		let fa2 = new FiniteAutomaton.model (Arg.Text fa_clean2) in
		let mfa = fa#cleanUselessStates in
		let mfa2 = fa2#cleanUselessStates in
		let j = mfa#toJSon in
		let j2 = mfa2#toJSon in
			JSon.show j; Util.println [];
			JSon.show j2; Util.println []

	let fa_isDeter = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "isDeter",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","a","S3"],
				["S2","a","S3"], ["S2","b","S2"]
			],
		acceptStates : ["S3"]
	} |}

	let fa_isDeter2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "isDeter",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","b","S3"],
				["S2","a","S4"], ["S4","b","S5"],
				["S3","b","S5"]
			],
		acceptStates : ["S5"]
	} |}

	let fa_isDeter3 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "isDeter",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","a","S3"],
				["S2","b","S4"],
				["S3","b","S4"]
			],
		acceptStates : ["S4"]
	} |}

	let fa_toDeter = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["0", "1"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","1","S2"], ["S1","1","S3"] , ["S1","0","S5"],
				["S2","~","S4"],
				["S4","0","S3"],
				["S5","1","S2"], ["S5","0","S3"], ["S5","0","S4"]
			],
		acceptStates : ["S3"]
	} |}

	let testIsDeterministic () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_isDeter) in
		let fa2 = new FiniteAutomaton.model (Arg.Text fa_isDeter2) in
		let fa3 = new FiniteAutomaton.model (Arg.Text fa_isDeter3) in
			if fa#isDeterministic then
				Util.println ["automata is deterministic"] else Util.println ["automata is non-deterministic"];
			if fa2#isDeterministic then
				Util.println ["automata is deterministic"] else Util.println ["automata is non-deterministic"];
			if fa3#isDeterministic then
				Util.println ["automata is deterministic"] else Util.println ["automata is non-deterministic"]



	let testToDeterministic () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_toDeter) in
		let mfa = fa#toDeterministic in
		let j = mfa#toJSon in
			JSon.show j;
		let fa = new FiniteAutomaton.model (Arg.Text fa_isDeter) in
		let mfa = fa#toDeterministic in
		let j = mfa#toJSon in
			JSon.show j


	let testEquivalence () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
		let s = fa#equivalencePartition in
			Set.iter (fun s -> Util.print ["set: "]; Util.printStates (Set.toList s)) s


	let fa_minimize = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","b","S3"],
				["S2","b","S4"], ["S2","a","S3"],
				["S3","a","S2"], ["S3","b","S4"],
				["S4","b","S3"], ["S4","a","S2"],
				["S4","a","S5"]
			],
		acceptStates : ["S4"]
	} |}

	let fa_minimize2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "min",
		alphabet : ["i", "c", "1", "2"],
		states : ["S01", "S02", "S03", "S04", "S05",
				"S06", "S07", "S08", "S09", "S10"],
		initialState : "S01",
		transitions : [
				["S01","i","S02"],
				["S02","1","S03"], ["S02","i","S02"],
				["S03","1","S04"], ["S03","i","S04"],
				["S04","1","S03"], ["S04","2","S05"], ["S04","i","S04"],
				["S05","i","S06"], ["S05","c","S07"],
				["S06","i","S06"], ["S06","1","S03"],
				["S07","1","S04"], ["S07","i","S08"],
				["S08","i","S08"], ["S08","1","S03"], ["S08","2","S09"],
				["S09","c","S03"], ["S09","i","S10"],
				["S10","1","S03"], ["S10","i","S10"]
			],
		acceptStates : ["S10"]
	} |}

	let fa_minimize3 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b","c"],
		states : ["S0","S1", "S2", "S3", "S4", "S5"],
		initialState : "S0",
		transitions : [
				["S0","a","S1"], ["S0","b","S2"],
				["S1","b","S0"], ["S1","a","S1"], ["S1","c","S4"],
				["S2","b","S0"], ["S2","a","S2"], ["S2","c","S5"],
				["S3","b","S1"], ["S3","a","S3"], ["S3","c","S4"],
				["S4","b","S5"],
				["S5","b","S4"]
			],
		acceptStates : ["S4","S5"]
	} |}

	let fa_minimize4 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["0", "1"],
		states : ["00","01", "10", "11"],
		initialState : "00",
		transitions : [
				["00","1","01"], ["00","0","10"],
				["01","1","00"], ["01","0","11"],
				["10","0","00"], ["10","1","11"],
				["11","1","10"], ["11","0","01"]
			],
		acceptStates : ["01"]
	} |}

	let testMinimize () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_minimize) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let testMinimize2 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_minimize2) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let testMinimize3 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_minimize3) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let testMinimize4 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_minimize4) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let runAll =
		if active then (
			Util.header "FiniteAutomatonTests";
			test0 ();
			testBug ();
			testBug2 ();
			testAcceptBF ();
			testAcceptBF2 ();
			testAccept ();
			testAccept2 ();
			testAccTrace ()
		)
end

