(*
 * LRGrammar.ml
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
 *  Written by Bernardo Sousa (br)
 *)

(*
 * ChangeLog:
 *
 * may/2022 (br) - Most of the LR theory implemented.
 * mar/2022 (amd) - Skeleton.
 *)

(*
 * Description: A very simple parser for CFG syntax.
 *)
 
open BasicTypes
open CFGSyntax

(*
module type LRGrammarSig =
sig
	open CFGSyntax
	type t = ContextFreeGrammar.t
	type tx = ContextFreeGrammar.tx
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation: t
				method representationx : tx
				method validate : unit

				method tracing: unit
				method isRegular: bool
				method accept: word -> bool
				method acceptWithTracing: word -> unit
				method generate: int -> words

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
								
			(* LR parsing *)
				method isLR1 : bool


			end
end
*)

module LRAux =
struct
	let word s = str2word s
		
	let rec nats n = if n = 0 then [] else nats(n-1) @ [string_of_int (n-1)]
	
	let rec pop n l (*uses reverse stack and removes n heads from stack *)= 
		match l with
		| [] -> []
		| x::xs -> if(n>0) then pop (n-1) xs else x::pop n xs  
	
	
	let getTail l = (*remove head from list *)
		match l with
		| [] -> []
		|_::xs -> xs
	
	
end	

module LR0Grammar =
struct
	open LRAux
	type t = ContextFreeGrammar.t
	type tx = ContextFreeGrammar.tx

	type lr0Item = {head:symbol; body1:symbol list; body2:symbol list}		
	type lr0State = lr0Item set
	type lr0Diagram = lr0State set * (lr0State * symbol * lr0State ) set
	
	type stateName = string
	
	type lr0StateId = stateName * lr0State 
	type lr0DiagramId = lr0StateId set * (lr0StateId * symbol * lr0StateId ) set
	
	type lr0Action = Accept | Shift | Reduce of rule 
	type lr0TableEntry = stateName * (symbol * stateName) set * lr0Action
	type lr0Table = lr0TableEntry set
	
	let rule2Item (rule: rule) : lr0Item = (* converte uma regra num item novo, com o ponto á esquerda do corpo desse item *)
		{head = rule.head; body1 = []; body2 = rule.body} 
		
	
	let kernelAdvanceItem {head=h; body1=b1; body2=b2} = (* função auxiliar para avançar o ponto de um item em um simbolo para o nucleo do proximo estado. Ex: A ->.ab para A -> a.b *)
		match b2 with
		| [] -> failwith "kernelAdvanceItem: Este caso nem deve ser alcançavel"
		| x::xs -> {head = h; body1 = b1 @ [x]; body2 = xs} 
	
	

	
		
	let getDirector {head=_; body1=_; body2=b2} = (* obtem o simbolo diretor de um item *)
		match b2 with 
			| [] -> epsilon (* epsilon, aka no symbol *) 
			| x::_ -> x	
			
			
		
			
	let getDirectors state = (* Aplica a função getDirector a todos os itens de um dado estado*)
		Set.filter (fun d-> d <> epsilon)(Set.map getDirector state)
	
		
		
	let getRulesWithThisHead rules director = (* recebe o conjunto de regras da gramática e filtra esse conjunto para obter as regras cuja cabeça tem aquele simbolo diretor *)
		Set.filter (fun {head = h; body =_} -> h = director) rules 
		
	
	let diagramsJoin2 (s0,t0) (s1,t1) = (Set.union s0 s1, Set.union t0 t1) (* juntar dois diagramas LR0 para obter um diagrama LR0 resultante da união *)	
	
	let rec diagramsJoinList l : lr0Diagram = (* Juntar um conjunto de diagramas para produzir o diagrama LR0, cada diagrama desta lista corresponde a um estado rescrito como diagrama *)
		match l with
		| [] -> (Set.empty , Set.empty) 
		| d::ds -> diagramsJoin2 d (diagramsJoinList ds)
	
	
	let isNextSymbolNotAVariable {head=h; body1=b1; body2=b2} (cfg:t)=
		if(List.length b2 = 0) then true
		else
			if(Set.belongs (List.hd b2) cfg.variables) then false else true
	
	let isCompleteItem {head=h; body1=b1; body2=b2} =
		b2 = []
	
	let isStateInConflict lr0State cfg = 
		let completeItems = Set.filter(isCompleteItem) lr0State in
		if(Set.size completeItems < 1 ) then false
		else if(Set.size completeItems > 1 ) then true
		else
			let itemsProneToConflict = Set.filter(fun it -> isNextSymbolNotAVariable it cfg) lr0State in
			if(Set.size itemsProneToConflict > 1) then true else false
	
	let makeLR0DiagramId diagram : lr0DiagramId (* Cria etiquetas para os estados e os mesmos estados contidos nas transições do diagrama*) =
		let (states,transitions) = diagram in
		let dictionary = List.combine (Set.toList states) (nats (Set.size states)) in
		let statesId = Set.map (fun s -> (List.assoc s dictionary,s) ) states in
		let transitionsId = Set.map (fun (a,b,c) -> ((List.assoc a dictionary, a), b,(List.assoc c dictionary, c))) transitions in
			(statesId, transitionsId)
	
	
	let makeLR0TableEntry (id, lr0State) (cfg:t) transitions = 
		let stateTransitions = Set.filter (fun ((a,_),_,_)-> a = id) transitions in
			if Set.size stateTransitions = 0 then
				let {head = h;body1 = b1;body2 = b2} = List.hd (Set.toList lr0State) in
					if h = cfg.initial then 
						(id,Set.empty,Accept)
					else
						(id,Set.empty,Reduce ({head = h;body = b1}))			
			else  
				let nextShifts = Set.map (fun (a,b,(cId,c)) -> (b,cId)) stateTransitions in
					(id, nextShifts, Shift)
		
	(* falta uma função para aceitar/recusar palavra usando a tabela *)
	
	(*pre: isLR0 cfg *)
	let makeLR0Table (labeledDiagram:lr0DiagramId) cfg : lr0Table = (* recebe um diagrama numerado e constroi a tabela de parsing a partir das transições *) 
		let (statesId, transitionsId) = labeledDiagram in
			Set.map (fun s -> makeLR0TableEntry s cfg transitionsId) statesId
	
	
	let startRules (cfg: t) =
		let initial = cfg.initial in
		let rules = cfg.rules in
			Set.filter (fun {head=h; body=_} -> h = initial) rules
			
		
		
	let lr0StateClosureStep (cfg: t) currentItems = (* Create items for current directors *)
		let directors = getDirectors currentItems in
		let varDirectors = Set.inter directors cfg.variables in
		let newRules = Set.flatMap (fun d -> getRulesWithThisHead cfg.rules d) varDirectors in
		let newItems = Set.map rule2Item newRules in
			Set.union currentItems newItems 
		
		
	let rec lr0StateClosure cfg currentItems : lr0State = (* Create all items for a singular state *)
		let next = lr0StateClosureStep cfg currentItems in
		let next2 = Set.union currentItems next in
			if Set.size next2 = Set.size currentItems then next2
			else lr0StateClosure cfg next2


		
	let makeSingularNextLR0Diagram (cfg:t) prevState symbol : lr0Diagram = (* Creates a diagram containing only 1 state using the previous state and the transition symbol*)
		let items4Kernel = Set.filter (fun it -> getDirector it = symbol) prevState in (* falta avançar o ponto *)
		let kernel = Set.map (kernelAdvanceItem) items4Kernel in
		let closure = lr0StateClosure cfg kernel in
			(Set.make [prevState; closure], Set.make [(prevState ,symbol , closure )])
					

	
	let makeNextLR0Diagram (cfg:t) prevState : lr0Diagram = (* For each director symbol on the previous state, create a diagram and join all obtained diagrams into a single diagram*)
		let dirs = getDirectors prevState in
		let diagrams = Set.map (fun d -> makeSingularNextLR0Diagram cfg prevState d) dirs in
			diagramsJoinList (Set.toList diagrams) 

			
		
	let makeNextLR0DiagramAll (cfg:t) states : lr0Diagram = (* build the diagram using the initial state, then use the makeNextLR0Diagram function to calculate all states obtainable from the initial state*)
		let diagrams = Set.map (fun s -> makeNextLR0Diagram cfg s) states in
			diagramsJoinList (Set.toList diagrams)
			
	
	let makeFirstLR0Diagram (cfg:t) : lr0Diagram = (* O primeiro estado tem um procedimento de criação um pouco differente *) 
		let kernel = Set.map rule2Item (startRules cfg) in
		let closure = lr0StateClosure cfg kernel in
		(Set.make [closure], Set.empty)
		
		(*Set.make [closure] (* apesar de ser criado um par, nesta função só se cria o conjunto de items, o conjunto vazio das transições vazias é criado no makeLR0Diagram *) *)
		

	
	let rec makeLR0DiagramX (cfg:t) diagram = (* função auxiliar que irá produzir o diagrama LR0 *)
		let (states,transitions) : lr0Diagram = diagram in 
		let next = makeNextLR0DiagramAll cfg states in
		let next2 = diagramsJoin2 next (states,transitions) in
		let (states2,transitions2) = next2 in
			if Set.size states = Set.size states2 && Set.size transitions = Set.size transitions2 then next2
			else makeLR0DiagramX cfg next2 
					 

	let makeLR0Diagram (cfg:t) = makeLR0DiagramX cfg (makeFirstLR0Diagram cfg)  (* ponto de partida na construlão do diagrama LR0 *)
	
	
	let rec parseOperation lr0Table word stateStack symbolStack (cfg:t) = 
		let currentState = int_of_string (List.hd stateStack) in 
		let (id,shifts,action) = Set.nth lr0Table currentState in (* get corresponding table entry *)	
		match action with
		| Shift -> 
			begin
				match word with
				| [] -> false
				| s::_ -> 
					if(Set.belongs s cfg.alphabet || Set.belongs s cfg.variables) then
						let targetShifts = Set.filter (fun (a,b) -> a = s) shifts in
							if(Set.size targetShifts = 0) then false
							(* failwith (String.of_seq (List.to_seq ([char_of_int ((int_of_char '0') + currentState)] @ [s]))) *)
							(* para testar failwith (String.of_seq (List.to_seq [s])) (* Casos corretos estão a ir parar aqui por alguma razão, provavelmente após uma redução *) *)
							else
								let (nextSymbol,nextState) = Set.nth targetShifts 0 in
								let nextStateStack = [nextState] @ stateStack in
								let nextSymbolStack = [nextSymbol] @ symbolStack in
									parseOperation lr0Table (getTail word) nextStateStack nextSymbolStack cfg
					else
						failwith "singleParseOperation: este simbolo não pertence ao alfabeto desta gramatica"
			end
		| Accept -> 
			word = []	
		| Reduce({head = h;body = b}) -> 
			let popNumber = List.length b in
			let nextStateStack = (pop popNumber stateStack) in
			let nextSymbolStack = (pop popNumber symbolStack) in
			let wordWithAddedHead = [h] @ word in
				parseOperation lr0Table (wordWithAddedHead) nextStateStack nextSymbolStack cfg (*Add the variable, aka head of the reduction rule, to the word being processed *)
					
	
	(* pre: isLR0 cfg 
	   pre: isWordValid word cfg *)
	let acceptWordLR0 (word:symbol list) cfg : bool = 
		let lr0Table = makeLR0Table (makeLR0DiagramId (makeLR0Diagram cfg)) cfg in
		let stateRevStack = ["0"] in (*char list due to mix of numbers and symbols *) 
		let symbolRevStack : symbol list = [] in
			parseOperation lr0Table word stateRevStack symbolRevStack cfg
			
			
		
	let isLR0 cfg = (* verificar se a gramatica é lr0, ou seja, em todos os estados com items completos, não existem simbolos não terminais á direita de um ponto (item não completo *)
		let (states,transitions) = makeLR0Diagram cfg in
		let conflictItemStates = Set.filter(fun s -> isStateInConflict s cfg) states in
			if(Set.size conflictItemStates > 0) then false else true
		
end		
	(* ----- SLR1 -----*)
module SLR1Grammar =
struct
	open LRAux
	open LR0Grammar
	type t = ContextFreeGrammar.t
	type tx = ContextFreeGrammar.tx
	
(*
	type lr0Item = LR0Grammar.lr0Item		
	type lr0State = LR0Grammar.lr0State	
	type lr0Diagram = LR0Grammar.lr0Diagram	
	
	type stateName = LR0Grammar.stateName
	
	type lr0StateId = LR0Grammar.lr0StateId	
	type lr0DiagramId = LR0Grammar.lr0DiagramId		
*)
	
	type slr1Action = Accept | Shift | Reduce of rule 
	type slr1TableEntry = stateName * (symbol * stateName) set * (symbol * slr1Action set ) set
	type slr1Table = slr1TableEntry set
	
	let kernelAdvanceItem {head=h; body1=b1; body2=b2} = (* função auxiliar para avançar o ponto de um item em um simbolo para o nucleo do proximo estado. Ex: A ->.ab para A -> a.b *)
		match b2 with
		| [] -> failwith "kernelAdvanceItem: Este caso nem deve ser alcançavel"
		| x::xs -> {head = h; body1 = b1 @ [x]; body2 = xs} 
	
	let getNextSymbolForLR0Item (it:lr0Item)  =
		match it.body2 with
		| [] -> epsilon
		| x::xs -> x
	
	let follow w = (* Injected follow to test SLR1 grammars*)
		match w with
		| [] -> Set.make [dollar] (* Não deve acontecer*)
		| x::xs -> 
			if(x = symb "A") then Set.make [symb "c"]
			else if(x = symb "B") then Set.make [symb "d"]
			else if(x = symb "X") then Set.make [dollar]
			else Set.make [dollar]

			
	let followSetForSLR1Item it =
		follow([it.head])
		
	let isCompleteLR0Item (it:lr0Item) =
		it.body2 = []

	let countCompleteLR0Items lr0State = 
		let completeItems = Set.filter(isCompleteLR0Item) lr0State in
			Set.size completeItems	
			
	let rec buildSLR1ReductionActionsForOne completeItems symbol = (* Warning, input must only contain complete items *)
		let reductionItems = Set.filter(fun it -> Set.belongs symbol (followSetForSLR1Item it)) completeItems in
			Set.map (fun it -> Reduce ({head = it.head;body = it.body1}) ) reductionItems	
			


	let buildSLR1ReductionActions completeItems alphabet = 
		Set.map(fun symbol -> (symbol, buildSLR1ReductionActionsForOne completeItems symbol) ) alphabet
	
	
	
	let buildSLR1ShiftActionsForOne items symbol : slr1Action set = 
		let shiftItems = Set.filter(fun it -> (getNextSymbolForLR0Item it) = symbol) items in
		if(Set.size shiftItems > 0) then
			Set.make [Shift]
		else
			Set.empty
			


	let buildSLR1ShiftActions completeItems alphabet = 
		Set.map(fun symbol -> (symbol, buildSLR1ShiftActionsForOne completeItems symbol) ) alphabet
	
			
	let buildSLR1MixedActionsForOne items symbol = 
		let reductionItems = Set.filter(fun it -> (Set.belongs symbol (followSetForSLR1Item it)) && isCompleteLR0Item it) items in
		let shiftItems = Set.filter(fun it -> (getNextSymbolForLR0Item it) = symbol) items in
		let reductionEntries = Set.map (fun it -> Reduce ({head = it.head;body = it.body1}) ) reductionItems in
			if(Set.size shiftItems > 0) then
				Set.union (Set.make [Shift]) reductionEntries
			else
				reductionEntries
				

	let rec buildSLR1MixedActions (items:lr0State) alphabet = (* True build function - prototype *) (* transformar na forma do buildLR1ReductionActions *)
		Set.map(fun symbol -> (symbol, buildSLR1MixedActionsForOne items symbol) ) alphabet
	
			
			
	let makeSLR1TableEntry (id, lr0State) (cfg:t) transitions = 
		let stateTransitions = Set.filter (fun ((a,_),_,_)-> a = id) transitions in
			if Set.size stateTransitions = 0 then
				let {head = h;body1 = b1;body2 = b2} = List.hd (Set.toList lr0State) in
					if h = cfg.initial then 
						let slr1Actions : (symbol * slr1Action set) set = Set.make [dollar,Set.make [Accept]] in
							(id,Set.empty,slr1Actions)
					else
						let completeAlphabet = Set.add dollar cfg.alphabet in (* Se o $, final da palavra, não estiver no alfabeto da gramática *)
						let slr1Actions : (symbol * slr1Action set) set = buildSLR1ReductionActions lr0State completeAlphabet in
							(id,Set.empty, slr1Actions)	
			else  
				let nextShifts = Set.map (fun (a,b,(cId,c)) -> (b,cId)) stateTransitions in
					if(countCompleteLR0Items lr0State = 0) then (* Não existem reducoes *)
						let slr1Actions = buildSLR1ShiftActions lr0State cfg.alphabet in
							(id, nextShifts, slr1Actions)
					else (* Existem reducoes e transferencias *)
						let completeAlphabet = Set.add dollar cfg.alphabet in (* Se o $, final da palavra, não estiver no alfabeto da gramática *)
						let slr1Actions = buildSLR1MixedActions lr0State completeAlphabet in
							(id, nextShifts, slr1Actions)	
		
	
	(*pre: isLR1 cfg *)

	let makeSLR1Table (labeledDiagram:lr0DiagramId) cfg : slr1Table = (* recebe um diagrama numerado e constroi a tabela de parsing a partir das transições *) 
		let (statesId, transitionsId) = labeledDiagram in
			Set.map (fun s -> makeSLR1TableEntry s cfg transitionsId) statesId
			
			
		
		
	let rec parseOperationSLR1 slr1Table word stateStack symbolStack (cfg:t) = 
		let currentState = int_of_string(List.hd stateStack) in 
		let (id,shifts,actionSet) = Set.nth slr1Table currentState in (* get corresponding table entry *)
		let topSymbol = List.nth word 0 in
			if(Set.belongs topSymbol cfg.variables) then (*Fazemos um Shift com uma variavel*)
				let targetShifts = Set.filter (fun (a,b) -> a = topSymbol) shifts in
					if(Set.size targetShifts = 0) then false
					else
						let (nextSymbol,nextState) = Set.nth targetShifts 0 in
						let nextStateStack = [nextState] @ stateStack in
						let nextSymbolStack = [nextSymbol] @ symbolStack in
							parseOperationSLR1 slr1Table (getTail word) nextStateStack nextSymbolStack cfg
			else 
				let peekedSymbol = List.nth word 0 in
				let peekedsymbolAndActions = Set.filter( fun (s,a) -> s = peekedSymbol && Set.size a > 0 ) actionSet in
				let nEntries = Set.size peekedsymbolAndActions in
					if nEntries = 0 then 
						false
					else if nEntries > 1 then
						failwith "ParseOperationLR1: conflito"
					else
						let (symbol,actions) = Set.hd peekedsymbolAndActions in (* atualmente está a falhar aqui, com hd failure *)
						let action = Set.hd actions in
						match action with
						| Shift -> 
							begin
								match word with
								| [] -> false
								| s::_ -> 
									if(Set.belongs s cfg.alphabet || Set.belongs s cfg.variables) then
										let targetShifts = Set.filter (fun (a,b) -> a = s) shifts in
											if(Set.size targetShifts = 0) then false
											(* failwith (String.of_seq (List.to_seq ([char_of_int ((int_of_char '0') + currentState)] @ [s]))) *)
											(* para testar failwith (String.of_seq (List.to_seq [s])) (* Casos corretos estão a ir parar aqui por alguma razão, provavelmente após uma redução *) *)
											else
												let (nextSymbol,nextState) = Set.nth targetShifts 0 in
												let nextStateStack = [nextState] @ stateStack in
												let nextSymbolStack = [nextSymbol] @ symbolStack in
													parseOperationSLR1 slr1Table (getTail word) nextStateStack nextSymbolStack cfg
									else
										failwith "ParseOperationLR1: este simbolo não pertence ao alfabeto desta gramatica"
							end
						| Accept -> 
							word = [dollar]	
						| Reduce({head = h;body = b}) -> 
							let popNumber = List.length b in
							let nextStateStack = (pop popNumber stateStack) in
							let nextSymbolStack = (pop popNumber symbolStack) in
							let wordWithAddedHead = [h] @ word in
								parseOperationSLR1 slr1Table (wordWithAddedHead) nextStateStack nextSymbolStack cfg (*Add the variable, aka head of the reduction rule, to the word being processed *)
			
	(* pre: isSLR1 cfg 
	   pre: isWordValid word cfg *)
	let acceptWordSLR1 (word:symbol list) cfg : bool = 
		let slr1Table = makeSLR1Table (LR0Grammar.makeLR0DiagramId (LR0Grammar.makeLR0Diagram cfg)) cfg in
		let stateRevStack = ["0"] in (*char list due to mix of numbers and symbols *) 
		let symbolRevStack : symbol list = [] in
			parseOperationSLR1 slr1Table (word @ [dollar]) stateRevStack symbolRevStack cfg	
	
	let entryHasConflict slr1TableEntry : bool =
		let (id,shifts,actionSet) = slr1TableEntry in
		let entryConflicts = Set.filter ( fun (_, actions) -> Set.size actions > 1) actionSet in
			not (Set.isEmpty entryConflicts)
	
	let isSLR1 cfg : bool =
		let slr1Table = makeSLR1Table (makeLR0DiagramId (makeLR0Diagram cfg)) cfg in
		let conflicts = Set.filter (entryHasConflict) slr1Table in
			Set.isEmpty conflicts

end
	(* ----- LR1 -----*)
module LR1Grammar =
struct
	open LRAux
	type t = ContextFreeGrammar.t
	type tx = ContextFreeGrammar.tx
	
	type lr1Item = {head:symbol; body1:symbol list; body2:symbol list; lookahead:symbols}	
	type lr1State = lr1Item set
	type lr1Diagram = lr1State set * (lr1State * symbol * lr1State ) set
	
	type stateName = string
	type lr1StateId = stateName * lr1State 
	type lr1DiagramId = lr1StateId set * (lr1StateId * symbol * lr1StateId ) set
	

	type lr1Action = Accept | Shift | Reduce of rule
	type lr1TableEntry = stateName * (symbol * stateName) set * (symbol * lr1Action set ) set (* talvez seja (symbol * lr1Action set ) set *)
	type lr1Table = lr1TableEntry set
	
	let isCompleteLR1Item {head=h; body1=b1; body2=b2;lookahead=l} =
		b2 = []

	let countCompleteLR1Items lr1State = 
		let completeItems = Set.filter(isCompleteLR1Item) lr1State in
			Set.size completeItems
			
			
	let getNextSymbolForLR1Item {head=h; body1=b1; body2=b2;lookahead=l}  =
		match b2 with
		| [] -> epsilon
		| x::xs -> x
		
	let getDirectorLR1 {head=_; body1=_; body2=b2; lookahead=l} = (* obtem o simbolo diretor de um item *)
		match b2 with 
			| [] -> epsilon (* epsilon, aka no symbol *) 
			| x::_ -> x
			
	(*
	let first symbols = (* Injected first to test LR1 grammars lookahead - Luis Monteiro *)
		match symbols with
		| [] -> Set.make ['$'] (* Não deve acontecer*)
		| x::xs -> 
			if(x = 'A') then ['$';'a';'b']
			else if(x = 'B') then ['a';'b']
			else if(x = 'a') then ['a']
			else ['b']
	*)		
			
	let first symbols = (* Injected first to test LALR1 grammars lookahead - Luis Monteiro *)
		match symbols with
		| [] -> Set.make [dollar] (* Não deve acontecer*)
		| x::xs -> 
			if(x = symb "X") then Set.make [symb "c"; symb "d"]
			else if(x = symb "C") then Set.make [symb "c"; symb "d"]
			else if(x = symb "c") then Set.make [symb "c"]
			else Set.make [symb "d"]
	
	let getDirectorWithLookaheadLR1 {head=_; body1=_; body2=b2; lookahead=l} cfg = (* obtem o simbolo diretor de um item *)
		match b2 with 
			| [] -> (epsilon,Set.empty) (* epsilon, aka no symbol *) 
			| x::xs -> if(List.length b2 > 1) then (x,first xs) else (x,l)	
			
			
	let getDirectorsLR1 state = (* Aplica a função getDirector a todos os itens de um dado estado*)
		Set.filter (fun d-> d <> epsilon)(Set.map getDirectorLR1 state)
			
	let kernelAdvanceLR1Item {head=h; body1=b1; body2=b2;lookahead = l} = (* função auxiliar para avançar o ponto de um item em um simbolo para o nucleo do proximo estado. Ex: A ->.ab para A -> a.b *)
		match b2 with
		| [] -> failwith "kernelAdvanceItem: Este caso nem deve ser alcançavel"
		| x::xs -> {head = h; body1 = b1 @ [x]; body2 = xs;lookahead = l} 
	
			
			
			
	let buildLR1Item {head=h; body1=b1; body2=b2} lookahead =
		{head=h; body1=b1; body2=b2; lookahead=lookahead}
	
			
	let getDirectorsWithLookaheadLR1 (state:lr1State) cfg = (* Aplica a função getDirectorWithLookaheadLR1 a todos os itens de um dado estado*)
		Set.filter (fun (d,l)-> d <> epsilon)(Set.map (fun it -> getDirectorWithLookaheadLR1 it cfg) state) 
	
	
	let hasSameCore {head=h1; body1=b1; body2=b2; lookahead=l1} {head=h2; body1=b21; body2=b22; lookahead=l2} = 
		(h1 = h2 && b1 = b21 && b2 = b22)
		
	let mergeTwoItemsWithSameCore {head=h1; body1=b1; body2=b2; lookahead=l1} {head=h2; body1=b21; body2=b22; lookahead=l2} =
		let combinedLookahead = Set.union l1 l2 in
		{head=h1; body1=b1;body2=b2;lookahead=combinedLookahead}
	
		
	let mergeOneItem item currentItems = (* careful with the args order*)
		let (a,b) = Set.partition (fun i -> hasSameCore item i ) currentItems in
			if Set.size a = 0 then Set.add item currentItems
			else Set.add (mergeTwoItemsWithSameCore (Set.hd a) item) b	
			
	(*
	let mergeItems2 currentItems newItems =
		let rec process currentItems newItems =
			match newItems with
			| [] -> currentItems 
			| i::is -> process (mergeOneItem i currentItems) is 
		in
			process currentItems (Set.toList newItems) 		
	*)
	
			
	let rec mergeItems currentItems newItems =
		if Set.isEmpty newItems then
			currentItems
		else
			let (i,is) = Set.cut newItems in
				mergeItems (mergeOneItem i currentItems) is 
	
	(*			
	let rec mergeItems currentItems newItems =
		Set.match_ newItems 
			(fun () -> currentItems)
			(fun i is -> mergeItems (mergeOneItem i currentItems) is)
	*)
	
	let rule2ItemLR1 (rule: rule) lookahead =
		{head = rule.head; body1 = []; body2 = rule.body; lookahead = lookahead} 
	
	let generateItemsForVarDirectorWithLookahead director rules lookahead = 
		let itemRules = Set.filter (fun {head = h; body =_} -> h = director) rules in 
		let items = Set.map (fun r -> rule2ItemLR1 r lookahead) itemRules in	
			items
			
	let diagramsJoin2LR1 (s0,t0) (s1,t1) = (Set.union s0 s1, Set.union t0 t1) (* juntar dois diagramas LR0 para obter um diagrama LR0 resultante da união *)	
	
	let rec diagramsJoinListLR1 l : lr1Diagram = (* Juntar um conjunto de diagramas para produzir o diagrama LR0, cada diagrama desta lista corresponde a um estado rescrito como diagrama *)
		match l with
		| [] -> (Set.empty , Set.empty) 
		| d::ds -> diagramsJoin2LR1 d (diagramsJoinListLR1 ds)
	
	
	
	let makeLR1DiagramId diagram : lr1DiagramId (* Cria etiquetas para os estados e os mesmos estados contidos nas transições do diagrama*) =
		let (states,transitions) = diagram in
		let dictionary = List.combine (Set.toList states) (nats (Set.size states)) in
		let statesId = Set.map (fun s -> (List.assoc s dictionary,s) ) states in
		let transitionsId = Set.map (fun (a,b,c) -> ((List.assoc a dictionary, a), b,(List.assoc c dictionary, c))) transitions in
			(statesId, transitionsId)
	



	let rec buildLR1ReductionActionsForOne completeItems symbol = (* Warning, input must only contain complete items *)
		let reductionItems = Set.filter(fun it -> Set.belongs symbol it.lookahead) completeItems in
			Set.map (fun it -> Reduce ({head = it.head;body = it.body1}) ) reductionItems	
			


	let buildLR1ReductionActions completeItems alphabet = 
		Set.map(fun symbol -> (symbol, buildLR1ReductionActionsForOne completeItems symbol) ) alphabet
	
	
	let buildLR1ShiftActionsForOne items symbol : lr1Action set = 
		let shiftItems = Set.filter(fun it -> (getNextSymbolForLR1Item it) = symbol) items in
		if(Set.size shiftItems > 0) then
			Set.make [Shift]
		else
			Set.empty
			


	let buildLR1ShiftActions completeItems alphabet = 
		Set.map(fun symbol -> (symbol, buildLR1ShiftActionsForOne completeItems symbol) ) alphabet
	
	

	let buildLR1MixedActionsForOne items symbol = 
	let reductionItems = Set.filter(fun it -> (Set.belongs symbol it.lookahead) && isCompleteLR1Item it) items in
		let shiftItems = Set.filter(fun it -> (getNextSymbolForLR1Item it) = symbol) items in
		let reductionEntries = Set.map (fun it -> Reduce ({head = it.head;body = it.body1}) ) reductionItems in
			if(Set.size shiftItems > 0) then
				Set.union (Set.make [Shift]) reductionEntries
			else
				reductionEntries
				

	let rec buildLR1MixedActions (items:lr1State) alphabet = (* True build function - prototype *) (* transformar na forma do buildLR1ReductionActions *)
		Set.map(fun symbol -> (symbol, buildLR1MixedActionsForOne items symbol) ) alphabet
		
			
	let makeLR1TableEntry (id, lr1State) (cfg:t) transitions = (* possivelmente dar merge aos buildLR1Actions?*)
		let stateTransitions = Set.filter (fun ((a,_),_,_)-> a = id) transitions in
			if Set.size stateTransitions = 0 then (* this part seems fine *)
				let {head = h;body1 = b1;body2 = b2;lookahead = l} = List.hd (Set.toList lr1State) in
					if h = cfg.initial then 
						let lr1Actions : (symbol * lr1Action set) set = Set.make [dollar,Set.make [Accept]] in
							(id,Set.empty,lr1Actions)
					else
						let completeAlphabet = Set.add dollar cfg.alphabet in (* Se o $, final da palavra, não estiver no alfabeto da gramática *)
						let lr1Actions : (symbol * lr1Action set) set = buildLR1ReductionActions lr1State completeAlphabet in
							(id,Set.empty, lr1Actions)		
			else  (* Existem Shifts e possivelmente tambem reducoes *)
				let nextShifts = Set.map (fun (a,b,(cId,c)) -> (b,cId)) stateTransitions in
					if(countCompleteLR1Items lr1State = 0) then (* Não existem reducoes *)
						let lr1Actions = buildLR1ShiftActions lr1State cfg.alphabet in
							(id, nextShifts, lr1Actions)
					else (* Existem reducoes e transferencias *)
						let completeAlphabet = Set.add dollar cfg.alphabet in (* Se o $, final da palavra, não estiver no alfabeto da gramática *)
						let lr1Actions = buildLR1MixedActions lr1State completeAlphabet in
							(id, nextShifts, lr1Actions)
					
	
	(*pre: isLR1 cfg *)
	let makeLR1Table (labeledDiagram:lr1DiagramId) cfg : lr1Table = (* recebe um diagrama numerado e constroi a tabela de parsing a partir das transições *) 
		let (statesId, transitionsId) = labeledDiagram in
			Set.map (fun s -> makeLR1TableEntry s cfg transitionsId) statesId


	
		
	let lr1StateClosureStep (cfg: t) currentItems = (* Create items for current directors *)
		let directorsWithLookahead : (symbol * symbols) set = getDirectorsWithLookaheadLR1 currentItems cfg in
		
		let varDirectorsWithLookahead = Set.filter (fun (d,_) -> Set.belongs d cfg.variables) directorsWithLookahead in
		let newItems = Set.flatMap (fun (d,l) -> generateItemsForVarDirectorWithLookahead d cfg.rules l) varDirectorsWithLookahead in
		let mergedItems = mergeItems currentItems newItems in
			mergedItems
		
		
	let rec lr1StateClosure cfg currentItems : lr1State = (* Create all items for a singular state *)
		let next = lr1StateClosureStep cfg currentItems in
			if Set.subset next currentItems then next
			else lr1StateClosure cfg next
			
			
	
	let makeSingularNextLR1Diagram (cfg:t) prevState symbol : lr1Diagram = (* Creates a diagram containing only 1 state using the previous state and the transition symbol*)
		let items4Kernel = Set.filter (fun it -> getDirectorLR1 it = symbol) prevState in (* falta avançar o ponto *)
		let kernel = Set.map (kernelAdvanceLR1Item) items4Kernel in
		let closure = lr1StateClosure cfg kernel in
			(Set.make [prevState; closure], Set.make [(prevState ,symbol , closure )])
					

	
	let makeNextLR1Diagram (cfg:t) prevState : lr1Diagram = (* For each director symbol on the previous state, create a diagram and join all obtained diagrams into a single diagram*)
		let dirs = getDirectorsLR1 prevState in
		let diagrams = Set.map (fun d -> makeSingularNextLR1Diagram cfg prevState d) dirs in
			diagramsJoinListLR1 (Set.toList diagrams) 

			
		
	let makeNextLR1DiagramAll (cfg:t) states : lr1Diagram = (* build the diagram using the initial state, then use the makeNextLR0Diagram function to calculate all states obtainable from the initial state*)
		let diagrams = Set.map (fun s -> makeNextLR1Diagram cfg s) states in
			diagramsJoinListLR1 (Set.toList diagrams)

		
		
	let rec makeLR1DiagramX (cfg:t) diagram = (* função auxiliar que irá produzir o diagrama LR1 *)
		let (states,transitions) : lr1Diagram = diagram in 
		let next = makeNextLR1DiagramAll cfg states in
		let next2 = diagramsJoin2LR1 next (states,transitions) in
		let (states2,transitions2) = next2 in
			if Set.size states = Set.size states2 && Set.size transitions = Set.size transitions2 then next2
			else makeLR1DiagramX cfg next2 
			
			
	let makeFirstLR1Diagram (cfg:t) : lr1Diagram = (* O primeiro estado tem um procedimento de criação um pouco differente *) 
		let kernel = Set.map (fun r -> rule2ItemLR1 r (Set.make [dollar])) (LR0Grammar.startRules cfg) in	
		(*let kernelWithLookahead : lr1Item = buildLR1KernelItems kernel '$' in *)
		let closure = lr1StateClosure cfg kernel in
			(Set.make [closure], Set.empty)	
	
	let makeLR1Diagram (cfg:t) = makeLR1DiagramX cfg (makeFirstLR1Diagram cfg)  (* ponto de partida na construção do diagrama LR1 *)
	
	
	let rec parseOperationLR1 lr1Table word stateStack symbolStack (cfg:t) = 
		let currentState = int_of_string(List.hd stateStack) in 
		let (id,shifts,actionSet) = Set.nth lr1Table currentState in (* get corresponding table entry *)
		let topSymbol = List.nth word 0 in
			if(Set.belongs topSymbol cfg.variables) then (*Fazemos um Shift com uma variavel*)
				let targetShifts = Set.filter (fun (a,b) -> a = topSymbol) shifts in
					if(Set.size targetShifts = 0) then false
					else
						let (nextSymbol,nextState) = Set.nth targetShifts 0 in
						let nextStateStack = [nextState] @ stateStack in
						let nextSymbolStack = [nextSymbol] @ symbolStack in
							parseOperationLR1 lr1Table (getTail word) nextStateStack nextSymbolStack cfg
			else 
				let peekedSymbol = List.nth word 0 in
				let peekedsymbolAndActions = Set.filter( fun (s,a) -> s = peekedSymbol && Set.size a > 0 ) actionSet in
				let nEntries = Set.size peekedsymbolAndActions in
					if nEntries = 0 then 
						false
					else if nEntries > 1 then
						failwith "ParseOperationLR1: conflito"
					else
						let (symbol,actions) = Set.hd peekedsymbolAndActions in (* atualmente está a falhar aqui, com hd failure *)
						let action = Set.hd actions in
						match action with
						| Shift -> 
							begin
								match word with
								| [] -> false
								| s::_ -> 
									if(Set.belongs s cfg.alphabet || Set.belongs s cfg.variables) then
										let targetShifts = Set.filter (fun (a,b) -> a = s) shifts in
											if(Set.size targetShifts = 0) then false
											(* failwith (String.of_seq (List.to_seq ([char_of_int ((int_of_char '0') + currentState)] @ [s]))) *)
											(* para testar failwith (String.of_seq (List.to_seq [s])) (* Casos corretos estão a ir parar aqui por alguma razão, provavelmente após uma redução *) *)
											else
												let (nextSymbol,nextState) = Set.nth targetShifts 0 in
												let nextStateStack = [nextState] @ stateStack in
												let nextSymbolStack = [nextSymbol] @ symbolStack in
													parseOperationLR1 lr1Table (getTail word) nextStateStack nextSymbolStack cfg
									else
										failwith "ParseOperationLR1: este simbolo não pertence ao alfabeto desta gramatica"
							end
						| Accept -> 
							word = [dollar]	
						| Reduce({head = h;body = b}) -> 
							let popNumber = List.length b in
							let nextStateStack = (pop popNumber stateStack) in
							let nextSymbolStack = (pop popNumber symbolStack) in
							let wordWithAddedHead = [h] @ word in
								parseOperationLR1 lr1Table (wordWithAddedHead) nextStateStack nextSymbolStack cfg (*Add the variable, aka head of the reduction rule, to the word being processed *)
								
	
	(* pre: isLR1 cfg 
	   pre: isWordValid word cfg *)
	let acceptWordLR1 (word:symbol list) cfg : bool = 
		let lr1Table = makeLR1Table (makeLR1DiagramId (makeLR1Diagram cfg)) cfg in
		let stateRevStack = ["0"] in (*char list due to mix of numbers and symbols *) 
		let symbolRevStack : symbol list = [] in
			parseOperationLR1 lr1Table (word @ [dollar]) stateRevStack symbolRevStack cfg
	
	let entryHasConflict slr1TableEntry : bool =
		let (id,shifts,actionSet) = slr1TableEntry in
		let entryConflicts = Set.filter ( fun (_, actions) -> Set.size actions > 1) actionSet in
			Set.size entryConflicts > 0
	
	let isLR1 cfg : bool =
		let slr1Table = makeLR1Table (makeLR1DiagramId (makeLR1Diagram cfg)) cfg in
		let conflicts = Set.filter (entryHasConflict) slr1Table in
			Set.size conflicts = 0
	
end	
	(* ----- LALR1 -----*)
module LALR1Grammar =
	struct
	open LRAux
	open LR0Grammar	
	open LR1Grammar	
		
		
	let itemsSameCores it1 it2 =
		it1.head = it2.head && it1.body1 = it2.body1 && it1.body2 = it2.body2
		
	let itemsJoinLookahead it1 it2 =
		{head = it1.head; body1 = it1.body1; body2 = it1.body2; lookahead = (Set.union it1.lookahead it2.lookahead)}
	
	
	let getStateCore (state:lr1State) =
		Set.map (fun it -> {head = it.head; body1 = it.body1; body2 = it.body2}) state
	
	
	let haveSameCores lr1state1 lr1state2 =
		let state1Core = getStateCore lr1state1 in
		let state2Core = getStateCore lr1state2 in
			Set.equals state1Core state2Core
				
	(*pre: hasSameCores state1 state2 *)
	let rec mergeLR1States state1 state2 =
		Set.map (fun it -> 
			let fit = Set.find (fun it2 -> itemsSameCores it it2) state2 in itemsJoinLookahead it fit) state1 
	
	
	
	type lr1StateId = stateName * lr1State 
	type lr1DiagramId = lr1StateId set * (lr1StateId * symbol * lr1StateId ) set
		
	let rec lr1StateFusionId statesId  =
		match statesId with
		| [] -> []
		| (id,x)::xs -> 
			let ss = lr1StateFusionId xs in
			let (a,b) = List.partition (fun (_,y)-> haveSameCores x y) ss in 
				match a with
				| [] -> (id,x)::ss 
				| [(id2,y)] -> (id^","^id2,mergeLR1States x y)::b(* fundir x com y*)
				| _ -> failwith "lr1StateFusionFail"	
		
				
	
	let rec lr1StateFusion states  =
		match states with
		| [] -> []
		| x::xs -> 
			let ss = lr1StateFusion xs in
			let (a,b) = List.partition (haveSameCores x) ss in 
				match a with
				| [] -> x::ss 
				| [y] -> mergeLR1States x y::b(* fundir x com y*)
				| _ -> failwith "lr1StateFusionFail"
		
	let translate state fstates =
		Set.find (fun s -> haveSameCores state s) fstates
		
				
	let lr1TransFusion trans fstates =
		Set.map (fun (s1,sym,s2) -> (translate s1 fstates,sym,translate s2 fstates)) trans
				
	
	let makeLALR1FromLR1 diagram =
		let (states,transitions) : lr1Diagram = diagram in 
		let fstates = lr1StateFusion (Set.toList states) in
		let ftrans = lr1TransFusion transitions (Set.make fstates) in
		let lalr1Diagram : lr1Diagram = ((Set.make fstates),ftrans) in
			lalr1Diagram
			
			
	(* pre: isLR1 cfg 
	   pre: isWordValid word cfg *)
	let acceptWordLALR1 (word: word) cfg : bool = 
		let lr1Table = makeLR1Table (makeLR1DiagramId (makeLALR1FromLR1 (makeLR1Diagram cfg))) cfg in
		let stateRevStack = ["0"] in (*char list due to mix of numbers and symbols *) 
		let symbolRevStack : symbol list = [] in
			parseOperationLR1 lr1Table (word @ [dollar]) stateRevStack symbolRevStack cfg
			
			
	let entryHasConflict slr1TableEntry : bool =
		let (id,shifts,actionSet) = slr1TableEntry in
		let entryConflicts = Set.filter ( fun (_, actions) -> Set.size actions > 1) actionSet in
			not (Set.isEmpty entryConflicts)
	
	let isLALR1 cfg : bool =
		let slr1Table = makeLR1Table (makeLR1DiagramId (makeLALR1FromLR1 (makeLR1Diagram cfg))) cfg in
		let conflicts = Set.filter (entryHasConflict) slr1Table in
			Set.isEmpty conflicts
	
end	
(*----- Test functions LR1-----*)
module LRTests =
	struct
	type t = ContextFreeGrammar.t
	type tx = ContextFreeGrammar.tx
	open LR0Grammar
	open SLR1Grammar
	open LR1Grammar
	open LALR1Grammar

let (lr1grammar:t) = (* basic LR1 with no First usage *)
{alphabet = symbols "01";
variables = symbols "SXA" ;
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> X"; "X -> A"; "A -> 0" ; "A -> 1"])
} ;;

let (lr1grammar2:t) = (* basic LR1 with basic First usage *)
{alphabet = symbols "01";
variables = symbols "SXA" ;
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> X"; "X -> A01"; "A -> 0" ; "A -> 1"])
} ;;

let ttLR1 () = (* Full grammar *)
	makeLR1Diagram lr1grammar;;

let (lr1grammarX:t) = (* exemplo do professor Luis Monteiro *) 
{alphabet = symbols "ab";
variables = symbols "SAB";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> A"; "A -> BA"; "A -> ~" ; "B -> aB"; "B -> b"; ])
} ;; (* Resumindo esta gramatica: (B)^n, onde B = (a)^m b *)
(* (a*b)* *)

let ttLR1X () = (* Full grammar *)
	makeLR1Diagram lr1grammarX;;
	
let ttLR1Id () = 
	makeLR1DiagramId (makeLR1Diagram lr1grammarX) ;;
	
let ttLR1Table () =
	makeLR1Table (makeLR1DiagramId (makeLR1Diagram lr1grammarX)) lr1grammarX ;; 
	
let ttLR1Word () = (* simple test *)
	acceptWordLR1 (word "ab") lr1grammarX ;;
	
	
	
let ttLR1Word2 () = (* long simple test *)
	acceptWordLR1 (word "bbbbbbbb") lr1grammarX ;;
	
let ttLR1Word3 () = (* long complex test *)
	acceptWordLR1 (word "aaaaaaaaabbbbbbbb") lr1grammarX ;;
	
let ttLR1Word4 () = (* empty test *)
	acceptWordLR1 [] lr1grammarX ;;
	
let ttLR1Word5 () = (* combination test *)
	acceptWordLR1 (word "ababababababababaaaaaaaaabbbbbbbb") lr1grammarX ;;
	
let ttLR1WordFail () = (* falha mas da erro em vez de false *)
	acceptWordLR1 (word "bbbbbbbba") lr1grammarX ;;
	
	
(*----- Test functions LALR1-----*)


let (lalr1grammar:t) = (* basic SLR1 with Follow usage *)
	{alphabet = symbols "cd";
	variables = symbols "SXC";
	initial = symb "S";
	rules = CFGSyntax.parse (Set.make ["S -> X"; "X -> CC"; "X -> ~"; "C -> cC" ; "C -> d"])
} ;;

let ttLALR1 () = (* Full grammar *)
	makeLALR1FromLR1 (makeLR1Diagram lalr1grammar);;	
	
let ttLR1Table () =
	makeLR1Table (makeLR1DiagramId (makeLR1Diagram lalr1grammar)) lalr1grammar ;; 
	
let ttLALR1Table () =
	makeLR1Table (makeLR1DiagramId (makeLALR1FromLR1 (makeLR1Diagram lalr1grammar))) lalr1grammar ;; 	

let ttLALR1Word () = (* simple test *)
	acceptWordLALR1 [] lalr1grammar ;;
	
let ttLALR1Word2 () = (* simple test *)
	acceptWordLALR1 (word "dd" ) lalr1grammar ;;
	
	
let ttLALR1WordFail () = (* simple test *)
	acceptWordLALR1 (word "cd" )  lalr1grammar ;;
	
let ttIsLALR1 () = isLALR1 lalr1grammar ;;

let ttIsLR1 () = isLR1 lalr1grammar ;;

	
	
(*----- Test functions SLR1-----*)

let (slr1grammar:t) = (* basic SLR1 with Follow usage *)
	{alphabet = symbols "acdz";
	variables = symbols "SXAB";
	initial = symb "S";
	rules = CFGSyntax.parse (Set.make ["S -> X"; "X -> aAc"; "X -> aBd"; "A -> z" ; "B -> z"])
} ;;


let (slr1grammarFail:t) = (* basic SLR1 with Follow usage *)
	{alphabet = symbols "acdz";
	variables = symbols "SXAB";
	initial = symb "S";
	rules = CFGSyntax.parse (Set.make ["S -> X"; "X -> aAd"; "X -> aBd"; "A -> z" ; "B -> z"])
} ;;

let ttSLR1Table () =
	makeSLR1Table (makeLR0DiagramId (makeLR0Diagram slr1grammar)) slr1grammar ;; 

let ttSLR1Word() =
	acceptWordSLR1 (word "azc") slr1grammar ;;
	
let ttSLR1Word2() =
	acceptWordSLR1 (word "azd") slr1grammar ;;
	
let ttSLR1WordFail() =
	acceptWordSLR1 (word "") slr1grammar ;;
	
let ttSLR1WordFail2() =
	acceptWordSLR1 (word "azcd") slr1grammar ;;
	
let ttSLR1WordFail3() =
	acceptWordSLR1 (word "az") slr1grammar ;;
	
let ttSLR1WordFail4() =
	acceptWordSLR1 (word "azc$") slr1grammar ;;
	
let ttIsSLR1() = isSLR1 slr1grammar ;;

let ttIsSLR1Fail() = isSLR1 slr1grammarFail ;; (* é preciso alterar o follow para testar fails... dor *)
	


(*----- Test functions LR0-----*)

let showLR0States (cfg:t) =
	let diagram = makeLR0Diagram cfg in
	let (states,transitions) : lr0Diagram = diagram in
	states
	
let showLR0Transitions (cfg:t) =
	let diagram = makeLR0Diagram cfg in
	let (states,transitions) : lr0Diagram = diagram in
	transitions


let (grammar:t) = 
{alphabet = symbols "01";
variables = symbols "SX";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 1S0"; "S -> X"; "X -> 0X1" ; "X -> ~"])
} ;;

let tt () = (* Full grammar *)
	makeLR0Diagram grammar;;


let (grammar2:t) = 
{alphabet = symbols "1";
variables = symbols "S";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 1"])
} ;;

let tt2 () (* Single State Grammar *) =
	makeLR0Diagram grammar2;;
	

let (grammar3:t)  = 
{alphabet = symbols "01";
variables = symbols "S";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 111111"])
} ;;

let tt3 () (* Multiple State/Single Rule Grammar *)=
	makeLR0Diagram grammar3;;


let (grammar4:t)  = 
{alphabet = symbols "01";
variables = symbols "S";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 111111"; "S -> 000000"])
} ;;

let tt4 () (* Multiple State/Multiple Rule Grammar *) =
	makeLR0Diagram grammar4;;


let (grammar5:t)  = 
{alphabet = symbols "01";
variables = symbols "SA";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 111111"; "S -> 0X1"; "X -> 01" ])
} ;;

let (grammar5v2:t) (* Copy to test sorting in sets *) = 
{alphabet = symbols "01";
variables = symbols "SA";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 111111"; "S -> 0A1"; "A -> 01" ])
} ;;

let tt5 () (* Multiple Variables/Multiple State/Multiple Rule Grammar *) =
	makeLR0Diagram grammar5;;


let (grammar6:t) = 
{alphabet = symbols "01";
variables = symbols "SXA";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 1X0"; "X -> A"; "A -> 0A1"; "A -> 01"])
} ;;

let tt6 () (* Almost Full Grammar\No rule containing only epsilon *) =
	makeLR0Diagram grammar6;;

let (grammar7:t) = 
{alphabet = symbols "ab$";
variables = symbols "SXA";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> X$"; "X -> XA"; "X -> A"; "A -> aXb"; "A -> ab"])
} ;;

let tt7 () (* Gramática LR0 do exemplo de Luis Monteiro *) =
	makeLR0Diagram grammar7;;

	
let tt7Count () : bool (* Gramática LR0 do exemplo de Luis Monteiro *) =
	let (a,b) :lr0Diagram = makeLR0Diagram grammar7 in
	if (Set.size a = 9 && Set.size b = 13) then true else false
	
	
let (grammar7f:t) = 
{alphabet = symbols "abc$";
variables = symbols "SXA";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> X$"; "X -> XA"; "X -> A"; "A -> aXb"; "A -> ab";"A -> abc"])
} ;;

let (grammar7alt:t) = 
{alphabet = symbols "ab$";
variables = symbols "SXAF";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> X$"; "X -> XA"; "X -> A"; "A -> aXb"; "A -> ab";"A -> abF"; "F -> FA" ])
} ;;
	
let tt7LR0 () (* Gramática LR0 do exemplo de Luis Monteiro *) =
	isLR0 grammar7;;
	
let ttLR0Fail () (* Deve dar falso devido a dois items completos *) =
	isLR0 grammar7f;;
	
let ttLR0Alt () (* Devia dar verdadeiro com um item completo e um item incompleto que espera uma variavel, mas na prática isto é impossivel, porque é preciso calcular o fecho para essa variavel, esse fecho novo causa um conflito, a menos que a variavel não tenha uma regra respectiva, sendo nesse caso uma variavel inutil *) =
	isLR0 grammar7alt;;

let ttIncon1 () = makeLR0Diagram grammar5;;

let ttIncon2 () = makeLR0Diagram grammar5v2;;


let ttId () = makeLR0DiagramId (makeLR0Diagram grammar7) ;;
		
let ttx () =
	makeLR0Table (makeLR0DiagramId (makeLR0Diagram grammar7)) grammar7 ;; 
	
let ttx2 () =
	makeLR0Table (makeLR0DiagramId (makeLR0Diagram grammar5)) grammar5 ;; 


let ttWord () = 
	acceptWordLR0 (word "1") grammar2 ;;
	
let ttWordFail () = 
	acceptWordLR0 (word "10") grammar2 ;;
	
let ttWord2 () = 
	acceptWordLR0 (word "111111") grammar5 ;;
	
let ttWord2Fail () = 
	acceptWordLR0 (word "1111111") grammar5 ;;	
	
let ttWord3 () = 
	acceptWordLR0 (word "0011") grammar5 ;;
	
let ttWord3Fail () = 
	acceptWordLR0 (word "00111") grammar5 ;;
	
let ttWord4 () = 
	acceptWordLR0 (word "100110") grammar6 ;;
	
let ttWord4Fail () = 
	acceptWordLR0 (word "10011") grammar6 ;;
	
let ttWordX () =
	acceptWordLR0 (word "aaabbb$") grammar7 ;;
	
let ttWordXFail () =
	acceptWordLR0 (word "aaabbb") grammar7 ;;
	
end	
(*----- End of Testing -----*)
	
module LRGrammar =
struct
	type t = ContextFreeGrammar.t
	type tx = ContextFreeGrammar.tx
	open LR1Grammar
	
	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit LL1Grammar.model arg as super
		
	

	(* only a random example *)
			method isLR1 : bool =
				isLR1 (self#representation)
	end
end


open LRTests ;;
(*
module LRGrammarTests: sig end =
struct
	let active = false
	
	
	
	let test0 () =
		let m = new LR0Grammar.model (Arg.Predef "cfg_simple") in
		let j = m#toJSon in
			JSon.show j

	let test1 () =
		let m = new LR0Grammar.model (Arg.Predef "cfg_simple") in
		let r = m#startRules in
			Util.println ["*** Rules for the start symbol:"];
			CFGSyntax.show r

	let runAll =
		if Util.testing(active) then (
			Util.header "LRGrammarTests";
			test0 ();
			test1 ()	
		)
end
*)
