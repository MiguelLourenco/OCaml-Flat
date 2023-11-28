(*
 * PolyModel.ml
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
 * apr/2021 (amd) - Several new build functions.
 * jan/2021 (amd) - Created this module, collecting all the operation.
                    involving two or more kinds of models.
                    This allows to got rid of the mutual recursion between
                    modules, allowing storing each module in a different file.
 * dec/2019 (jg) - Initial code, across several modules in file "OCamlFlat.ml".
 *)

(*
 * Description: Poly-model operations.
 *
 * TODO: Cleanup.
 *)
 
open BasicTypes
open Util

module type PolyModelSig =
sig
	val json2model : JSon.t -> Model.model
	val text2model : string -> Model.model
	val file2model : string -> Model.model
	val example2model : string -> Model.model
	val re2fa : RegularExpression.model -> FiniteAutomaton.model
	val fa2re : FiniteAutomaton.model -> RegularExpression.model
	val fa2tm : FiniteAutomaton.model -> TuringMachine.model
	val re2tm : RegularExpression.model -> TuringMachine.model
	val re2cfg : RegularExpression.model -> ContextFreeGrammar.model
	val fa2cfg : FiniteAutomaton.model -> ContextFreeGrammar.model
	val cfg2fa : ContextFreeGrammar.model -> FiniteAutomaton.model
	val cfg2re : ContextFreeGrammar.model -> RegularExpression.model
	val cfg2tm : ContextFreeGrammar.model -> TuringMachine.model

	
end

module PolyModel : PolyModelSig =
struct

	let json2model (j: JSon.t): Model.model =	(* will build any model *)
		let kind = JSon.fieldString j "kind" in
			if FiniteAutomaton.modelDesignation = kind then
				(new FiniteAutomaton.model (Arg.JSon j) :> Model.model)
			else if RegularExpression.modelDesignation = kind then
				(new RegularExpression.model (Arg.JSon j) :> Model.model)
			else if ContextFreeGrammar.modelDesignation = kind then
				(new ContextFreeGrammar.model (Arg.JSon j) :> Model.model)
			else if FiniteEnumeration.modelDesignation = kind then
				(new FiniteEnumeration.model (Arg.JSon j) :> Model.model)
			else
				(new FiniteAutomaton.model (Arg.JSon j) :> Model.model)

	let text2model (text: string): Model.model =	(* will build any model *)
		json2model (JSon.parse text)

	let file2model (filename: string): Model.model =	(* will load any model *)
		json2model (JSon.fromFile filename)

	let example2model (name: string): Model.model =	(* will load any model *)
		text2model (Examples.example name)

(**
	* This method converts the automaton into a regular expression that accepts its language, by
	* using the transitive closure algorithm
	*
	* @returns RegularExpression.model -> the resulting regular expression
	*
	* Desc: The resulting expression is not minimal
	*)

		(* This method converts the regular expression to its equivalent finite automaton
	*
	* @returns FiniteAutomaton.model -> the resulting finite automaton
	*)
	let re2fa re =
		let open FiniteAutomaton in
		let open RegExpSyntax in

		(*auxiliary var for genName function*)
		let k = ref 0 in

		(*for each new state, generates a name that will distinguish it from all the other generated states *)
		let genName () =
			let n = !k in
			let () = k:= n + 1 in
				(*easy way of having all single digit state names have a zero before their actual number*)
				let name = if n > 9 then "new_St" ^ (string_of_int n)
							else "new_St0" ^ (string_of_int n) in
					str2state name in


		let rec compile (rep: RegExpTypes.t) : FinAutTypes.t =
			match rep with
				| Plus(l, r) ->
						let fa1 = compile l in
						let fa2 = compile r in
						let newStart = genName () in
						let newSts = Set.add newStart (Set.union fa1.states fa2.states) in
						let newAccSts = Set.union fa1.acceptStates fa2.acceptStates in
						let newTran1 = (newStart, epsilon, fa1.initialState) in
						let newTran2 = (newStart, epsilon, fa2.initialState) in
						let newTrans = Set.add newTran1 (Set.add newTran2
							(Set.union fa1.transitions fa2.transitions)) in
						let newAlf = Set.union fa1.alphabet fa2.alphabet in

							{alphabet = newAlf; states = newSts; initialState = newStart;
								transitions = newTrans; acceptStates = newAccSts}

				| Seq(l, r) ->
						let fa1 = compile l in
						let fa2 = compile r in
						let ist = fa1.initialState in
						let sts = Set.union fa1.states fa2.states in
						let asts = fa2.acceptStates in
						let newTrns = Set.map (fun x -> (x, epsilon, fa2.initialState) ) fa1.acceptStates in
						let trns = Set.union newTrns (Set.union fa1.transitions fa2.transitions) in
						let alf = Set.union fa1.alphabet fa2.alphabet in

							{alphabet = alf; states = sts; initialState = ist;
								transitions = trns; acceptStates = asts}

				| Star(r) ->
						let fa = compile r in
						let newStart = genName () in
						let newSts = Set.add newStart fa.states in
						let newTrns = Set.map (fun st -> (st, epsilon, newStart)) fa.acceptStates in
						let allNewTrns = Set.add (newStart, epsilon, fa.initialState) (Set.union newTrns fa.transitions) in

							{alphabet = fa.alphabet; states = newSts; initialState = newStart;
								transitions = allNewTrns; acceptStates = Set.make [newStart]}

				| Symb(c) ->
						let newStart = genName () in
						let newAcc = genName () in
						let newSts = Set.make [newStart; newAcc] in
						let newTrn = Set.make [(newStart, c, newAcc)] in

							{alphabet = Set.make [c]; states = newSts; initialState = newStart;
								transitions = newTrn; acceptStates = Set.make [newAcc]}

				| Empty ->
						let newStart = genName () in

								{alphabet = Set.empty; states = Set.make [newStart]; initialState = newStart;
									transitions = Set.empty; acceptStates = Set.make [newStart]}

				| Zero ->
						let newStart = genName () in

							{alphabet = Set.empty; states = Set.make [newStart]; initialState = newStart;
									transitions = Set.empty; acceptStates = Set.empty}
		in

			new FiniteAutomaton.model (Arg.Representation (compile re#representation))

	let fa2tm (fa : FiniteAutomaton.model) = 
		let rep: FinAutTypes.t = fa#representation in
		let transitionsFa2Tm trns = Set.map (fun (a,b,c) -> (a,b,c,b,R)) trns in
		let tm: TurMachTypes.t = {
						entryAlphabet = rep.alphabet;
						tapeAlphabet = rep.alphabet;
						empty = empty;
						states = rep.states;
						initialState = rep.initialState;
						transitions = transitionsFa2Tm rep.transitions;
						acceptStates = rep.acceptStates;
						criteria = true;
						markers = Set.empty
					}	in
		new TuringMachine.model (Arg.Representation tm)

	let re2tm (re: RegularExpression.model) =
		let reFA = re2fa re in
			fa2tm reFA

	(* Fazer conversao de TM para FA, quando as condicoes se reunem*)
 (*
	let generateTransitionsToPD st alphEntr alphPD =
		let allAlph = Set.add "$" (Set.union alphEntr alphPD) in
			Set.map (fun symb -> (st,symb,st,symb,R)) allAlph 

	let generateTransitionsFromPD st alphEntr alphPD =
		let allAlph = Set.add "$" (Set.union alphEntr alphPD) in
			Set.map (fun symb -> (st,symb,st,symb,L)) allAlph 

	let insertSymbolsPD alphEntr alphPD initSymbPD initState sts trs =
		let newSts = ["q0", "q1","q2"] in
		let newTrs = Set.union (Set.union (generateTransitionsToPD "q0" alphEntr alphPD) [("q0","B","q1","$",R); ("q1","B","q2",initSymbPD,R); ("q2","B",initState,"B",R)]) (generateTransitionsFromPD "q2" alphEntr alphPD) in
			(Set.union sts newSts) , (Set.union trs newTrs)

	let fillStackTransition stLast prevSt trs wordL = 
		match wordL with
		| [] -> trs
		| x::y ->	let newState = if (Set.isEmpty y) then stLast else IdGenerator.gen("q") in
							let dir = if (Set.isEmpty y) then L else R in
								fillStackTransition newState (Set.union trs (prevSt, "B", newState, x, dir)) y

	let convertTransitionX trs alphEntr alphPD initialStackSymb = 
		let (_,readSymbol,_,_,_) = trs in
			if readSymbol == dollar then convertAcceptTransition trs alphEntr alphPD initialStackSymb
			else convertNormalTransition trs alphEntr alphPD 

	let convertNormalTransition trs alphEntr alphPD =
		let (startState,unstackedSymbol,readSymbol,nextState,writeSymbolL) = trs in

		let st1 = IdGenerator.gen("q") in
		let st2 = IdGenerator.gen("q") in
		let st3 = IdGenerator.gen("q") in
		let st4 = IdGenerator.gen("q") in

		let ftrs = (startState,readSymbol,st1,"B",R) in
		let trsTPD = Set.add ftrs (generateTransitionsToPD st1 alphEntr alphPD) in
		let trsRTOP = Set.add (st1,"B",st2,"B",L) trsTPD in

		let firstDirection = if (writeSymbolL.length == 1) then L else R in
		let lastSt = if (writeSymbolL.length == 1) then st3 else st4 in

		let replaceTop = Set.add (st2,unstackedSymbol,st3,writeSymbolL.hd, firstDirection) trsRTOP in
		let additionalSymbolTrs = Set.union replaceTop (fillStackTransition lastSt st3 Set.empty writeSymbolL.tl) in
		let trsFPD = Set.union additionalSymbolTrs (generateTransitionsFromPD lastSt alphEntr alphPD) in
		let trsLast = Set.add (stLast,"B",nextState,"B",R) trsFPD in
			Set.add lastSt (Set.add st3 (Set.add st2 (Set.add st1 Set.empty))), trsLast

	let convertAcceptTransition trs alphEntr alphPD initialStackSymb =
		let (startState,unstackedSymbol,readSymbol,nextState,writeSymbolL) = trs in

		let st1 = IdGenerator.gen("q") in
		let st2 = IdGenerator.gen("q") in
		let st3 = IdGenerator.gen("q") in
		
		let ftrs = Set.union (startState,dollar,st1,dollar,R) Set.empty in
		let checkInitSS = Set.union (st1,initialStackSymb,st2,"B",R) ftrs in
		let lastCheck = Set.union (st2,"B",st3,"B",R) checkInitSS in
			Set.add st3 (Set.add st2 (Set.add st1 Set.empty)), lastCheck

	let convertTransitions newSts newTrs trs alphEntr alphPD initialStackSymb = 
		match trs with
		| [] -> newSts, newTrs
		| x::y -> let (nSts,nTrs) = convertTransitionX x alphEntr alphPD initialStackSymb in
								convertTransitions (Set.union nSts newSts) (Set.union nTrs newTrs) trs alphEntr alphPD initialStackSymb

(*Se parar por pilha vazia 'e ncess'ario criar um estado final*)

	let getFinalStates fsts trs =
		Set.map (fun (_,_,_,d,_) -> d) (Set.filter (fun (_,_,c,_,_) -> b == dollar) trs)

	let pda2tm (pda: PushdownAutomaton.model) =
		IdGenerator.reset();;
		let rep: FinAutTypes.t = pda#representation in
		let (initialStates, initialTransitions) = insertSymbolsPD rep.inputAlphabet rep.stackAlphabet rep.initialStackSymbol rep.initialState rep.states rep.transitions in
		let (convertedTransitionStates,convertedTransitions) = convertTransitions Set.empty Set.empty rep.transitions rep.inputAlphabet rep.stackAlphabet rep.initialStackSymbol in
		let allAlphabet = Set.union rep.inputAlphabet rep.stackAlphabet in
		let allStates = Set.union initialStates convertedTransitionStates in
		let allTransitions = Set.union initialTransitions convertedTransitions in
		let allAcceptStates = Set.union rep.acceptStates (getFinalStates rep.transitions) in
		let tm: TurMachTypes.t = {
						alphabet = allAlphabet;
						states = allStates;
						initialState = rep.initialState;
						transitions = allTransitions;
						acceptStates = rep.acceptStates;
						criteria = true
					}	in
		new TuringMachine.model (Arg.Representation tm)
	*)
	(*
		module IdGenerator =
		struct
			let current = ref 0;;

			let reset () =
				current := 0

			let gen (s: string) =
				let res = Printf.sprintf "%s%02d" s (!current) in
					current := !current+1;
					res
		end
	*)

	let fa2reMake fa =
		let open FinAutTypes in
		let open RegExpTypes in
		(* Since the algorithm only works for deterministic automaton, we first convert it
			to its deterministic equivalent *)
		let fa = fa#toDeterministic in

		let rep = fa#representation in

		let sts = rep.states in
		let trns = rep.transitions in

		(* transforms the set of expressions into the regex: plus of all expressions of the set *)
		let rec plusSet reSet =
			let open RegExpTypes in
			let rec pls l =
				match l with
					[] -> Zero
					| x::xs -> if xs = [] then x else Plus (x, pls xs)
			in
				pls (Set.toList reSet)
		in

		(* For the given i and j, returns the value of R when k is zero.
			Note that k will always be 0 when called inside this method *)
		let calczerok k i j =
			let ts = Set.filter (fun (a,_,b) -> i = a && j = b) trns in
			if ts <> Set.empty then
				if i <> j then
					let res = Set.map (fun (_,c,_) -> Symb c) ts in
						(k,i,j,plusSet res)
				else
					let res = Set.map (fun (_,c,_) -> Symb c) ts in
					let re = Plus(Empty, (plusSet res)) in
						(k,i,j,re)

			else (k,i,j,Zero)
		in


		(* For the given i and j, returns the value of R when k is not zero. *)
		let calck k i j prvK =
			let getRij i j =
				let r = Set.nth (Set.filter (fun (_,x,y,_) -> x = i && y = j) prvK) 0 in
					(fun (_,_,_,re) -> re) r
			in
			let assembleRe st i j =
				let rik = getRij i st in
				let rkk = Star (getRij st st) in
				let rkj = getRij st j in
					Seq(rik, Seq(rkk,rkj))
			in

			let rij = getRij i j in
			let rikjs = Set.map (fun st -> assembleRe st i j) sts in
			let rikj = plusSet rikjs in
				(k,i,j,Plus(rij,rikj))

		in

		(* Main function that applies previous 2 functions to all possible i and j pairs *)
		let rec rkij k =
			if k < 1 then
				Set.map (fun (i,j) -> calczerok k i j) (Set.combinations sts sts)
			else
				let prvK = rkij (k-1) in
					Set.map (fun(i,j) -> calck k i j prvK) (Set.combinations sts sts)
		in

		let allRks = rkij (Set.size sts) in
		let result = Set.filter (fun (_,i,j,_) -> i = rep.initialState && Set.belongs j rep.acceptStates ) allRks in
		let res = Set.map (fun (_,_,_,re) -> re) result in
		let newRe = plusSet res in
			newRe

	let fa2re fa =
		let re = fa2reMake fa in
			new RegularExpression.model (Arg.Representation re)





		(* This function converts a regular expression to its equivalent regular grammar
		*
		* @returns FiniteAutomaton.model -> the resulting regular grammar
		*)


	let re2cfg re =
		let open ContextFreeGrammar in
		let open CFGSyntax in

		(*auxiliary var for genVar function*)
		let k = ref 0 in

		(* generates new unused variable name for the cfg *)
		let genVar () =
			let n = !k in
			let () = k:= n + 1 in
			let ascii = 65 + n in
			if ascii < 65 || ascii > 90
			then char2symb 'A'
			else char2symb (Char.chr ascii)
		in

		(*
		let convertPlsRules rl i1 i2 newInit =
			(* swaps the initial variables of both old cfgs for the new initial var *)
			let swapInits c = if c = i1 || c = i2 then newInit else c in

			let newBody b = List.map (fun c -> swapInits c) b in
			let newRule r = {head = swapInits r.head; body = newBody r.body} in

				Set.map (fun r -> newRule r) rl

		in
		*)

		(* create gcf rules for plus expression *)
		let convertPlsRules rl i1 i2 newInit =
			let open CFGTypes in
			let newRule1 = {head = newInit; body = [i1]} in
			let newRule2 = {head = newInit; body = [i2]} in

				Set.add newRule1 (Set.add newRule2 rl)

		in

		(* create gcf rules for seq expression *)
		let convertSeqRules lcfg rcfg =
			let open CFGTypes in
			let rl1 = lcfg.rules in
			let rl2 = rcfg.rules in
			let alp1 = lcfg.alphabet in
			let rl = Set.union rl1 rl2 in

			let newBody r =
				let b = r.body in
					match b with
						| [c] when Set.belongs r rl1 && not (Set.belongs c alp1) && c <> epsilon -> b
						| [c] when Set.belongs r rl1 && Set.belongs c alp1 -> [c; rcfg.initial]
						| [epsilon] when Set.belongs r rl1 -> [epsilon; rcfg.initial]
						| b when Set.belongs r rl2 -> b
						| _ -> b
			in
			let newRule r = {head = r.head; body = newBody r} in
				Set.map (fun r -> newRule r) rl
		in

		(* create gcf rules for star expression *)
		let convertStrRules cfg =
			let open CFGTypes in

			let newBody b =
				match b with
					| [c] when Set.belongs c cfg.alphabet -> [c; cfg.initial]
					| _ -> b
			in
			let r0 = {head = cfg.initial; body = [epsilon]} in

			let newRule r = {head = r.head; body = newBody r.body} in
			let newRules = Set.map (fun r -> newRule r) cfg.rules in
				Set.add r0 newRules
		in



		let rec compile rep =
			let open RegExpTypes in
			let open CFGTypes in
			match rep with

				| Plus(l, r) ->
						let cl = compile l in
						let cr = compile r in
						let alp = Set.union cl.alphabet cr.alphabet in
						let init = genVar () in
						let vs = Set.add init (Set.union cl.variables cr.variables) in
						let rl = Set.union cl.rules cr.rules in
						let rl = convertPlsRules rl cl.initial cr.initial init in

							{alphabet = alp; variables = vs;
								initial = init; rules = rl}

				| Seq(l, r) ->
						let cl = compile l in
						let cr = compile r in
						let alp = Set.union cl.alphabet cr.alphabet in
						let init = cl.initial in
						let vs = Set.union cl.variables cr.variables in
						let rl = convertSeqRules cl cr in

							{alphabet = alp; variables = vs;
								initial = init; rules = rl}

				| Star(re) ->
						let cre = compile re in
						let alp = cre.alphabet in
						let init = cre.initial in
						let vs = cre.variables in
						let rl = convertStrRules cre in

							{alphabet = alp; variables = vs;
								initial = init; rules = rl}

				| Symb(c) ->
						let alp = Set.make [c] in
						let init = genVar () in
						let vars = Set.make [init] in
						let rules = Set.make [{head = init; body = [c]}] in

							{alphabet = alp; variables = vars;
								initial = init; rules = rules}

				| Empty ->
						let alp = Set.empty in
						let init = genVar () in
						let vars = Set.make [init] in
						let rules = Set.make [{head = init; body = [epsilon]}] in
							{alphabet = alp; variables = vars;
								initial = init; rules = rules}

				| Zero ->
						let alp = Set.empty in
						let init = genVar () in
						let var2 = genVar () in
						let vars = Set.make [init; var2] in
						let r1 = {head = init; body = [var2]} in
						let r2 = {head = var2; body = [init]} in
						let rules = Set.make [r1; r2] in

							{alphabet = alp; variables = vars;
								initial = init; rules = rules}
		in


		let cfg = compile re#representation in

			new ContextFreeGrammar.model (Arg.Representation (cfg))


	(**
		* This method converts the automaton into its equivalent regular grammar
		*
		* @returns ContextFreeGrammar.model -> the resulting regular grammar
		*)

	let fa2cfg fa =
			let re = fa2re fa in
				re2cfg re





	(* This method converts the right-linear grammar to its automaton equivalent
	*
	* @pre - the grammar needs to be regular
	*
	* @returns FiniteAutomaton.model -> the equivalent finite automaton
	*)
	let cfg2fa cfg =
		let open CFGTypes in

		let rep = cfg#representation in

		let alp = rep.alphabet in
		let vrs = rep.variables in
		let toState sy = state (symb2str sy) in

		(* This name will always be unique in the generated automaton *)
		let accSt = state "AccSt" in

		let alphabet = alp in
		let states = Set.map (fun v -> toState v) rep.variables in
		let states = Set.add accSt states in
		let initialState = toState rep.initial in
		let acceptStates = Set.make [accSt] in



		let ruleToTrans rh rb =
			match rb with
				| [s;v] when Set.belongs s alp && Set.belongs v vrs	-> Set.make [(toState rh, s, toState v)]

				| [v] when Set.belongs v vrs -> Set.make [(toState rh, epsilon, toState v)]

				| [s] when Set.belongs s alp -> Set.make [(toState rh, s, accSt)]

				| [e] when e = epsilon -> Set.make [(toState rh, epsilon, accSt)]

				| _ -> Set.empty
		in

		let transitions = Set.flatMap (fun r -> ruleToTrans r.head r.body) rep.rules in

		let open FinAutTypes in
		let fa = {
			alphabet = alphabet;
			states = states;
			initialState = initialState;
			transitions = transitions;
			acceptStates = acceptStates
		} in
			new FiniteAutomaton.model (Arg.Representation (fa))


	(* This method converts the right-linear grammar to its equivalent regular expression
	*
	* @pre - the grammar needs to be regular
	*
	* @returns FiniteAutomaton.model -> the equivalent regular expression
	*)

	let cfg2re cfg =
		let fa = cfg2fa cfg in
			fa2re fa

	let cfg2tm (cfg: ContextFreeGrammar.model) =
		let cfgFA = cfg2fa cfg in
			fa2tm cfgFA
end

module PolyModelTests: sig end =
struct
	open PolyModel

	let active = false

	let testToFA () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let testToFA2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let testToFA3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let testToFA4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let fa_toRe = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "fa_toRe",
		alphabet : ["a","b"],
		states : ["1", "2"],
		initialState : "1",
		transitions : [
				["1","a","2"],["2","b","2"]
			],
		acceptStates : ["2"]
	} |}

	let testSimplify () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_toRe) in
		let r = fa2re fa in
		JSon.show r#toJSon;
		let rs = r#simplify in
		let j = rs#toJSon in
			JSon.show j

	let testToRe () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_toRe) in
		let r = fa2re fa in
		let j = r#toJSon in
			JSon.show j

	let testToGrammar () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let res = re2cfg re in
			JSon.show res#toJSon

	let testToAutomaton () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_abc") in
		let res = cfg2fa m in
			JSon.show res#toJSon

	let testToRe () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_abc") in
		let res = cfg2re m in
			JSon.show res#toJSon

	let runAll =
		if Util.testing active "PolyModel" then begin
			testSimplify ()
		end
end
