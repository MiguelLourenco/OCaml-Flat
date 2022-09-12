#ifdef ALL

open BasicTypes
open CFGTypes  

module type LL1GrammarSig =
sig  
  type syntaxTable = { term : symbol option; var : symbol option; rBody : word option }
  type acceptTable = { input : string; stack: string; production: string }
  type recognized = { recog : string; left : string }
	type acceptStep = {
    syntaxTable : syntaxTable;
    acceptedString: string;
    acceptTable : acceptTable;
    recognized : recognized;
    accepted: bool option;
    nodes: cfgTree list
  }
  
  val leftRecursionRemovalTransform : string
  val leftFactoringTransform : string
  val cleanProductiveTransform : string
  val cleanAccessibleTransform : string
  val unitRemovalTransform : string
  val epsilonRemovalTransform :  string
  val ll1Transform : string
  
  type transformation = { tType : string; grammar : ContextFreeGrammar.model }
	
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method validate : unit
				method toJSon: JSon.t
				method representation: t
				method representationx: tx

				method tracing: unit
				method isRegular: bool
				method accept: word -> bool
				method acceptWithTracing: word -> unit
				method generate: int -> words
				
				method isSimplified : bool
				method rdparserOpts : string list
				method toggleSimplified : unit
				
				method first: word -> symbol Set.t
			  method follow: symbol -> symbol Set.t
			  method lookahead: rule -> symbol Set.t
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

module LL1Grammar : LL1GrammarSig =
struct
  open ContextFreeGrammar  
  open RDParserC
  open RDParserOCaml
  open RDParserJava
   
  type syntaxTable = { term : symbol option; var : symbol option; rBody : word option }
  type acceptTable = { input : string; stack: string; production: string }
  type recognized = { recog : string; left : string }
	type acceptStep = {
    syntaxTable : syntaxTable;
    acceptedString: string;
    acceptTable : acceptTable;
    recognized : recognized;
    accepted: bool option;
    nodes: cfgTree list
  }
  
  let bodiesOfHead = RDParser.bodiesOfHead
  
  let leftRecursionRemovalTransform = "Remove left recursion"
  let leftFactoringTransform = "Left factoring"
  let cleanProductiveTransform = "Clean unproductive symbols"
  let cleanAccessibleTransform = "Clean inaccessible symbols"
  let unitRemovalTransform = "Unit productions removal"
  let epsilonRemovalTransform = "Epsilon productions removal"
  let ll1Transform = "LL1 transformation"
  
  type transformation = { tType : string; grammar : ContextFreeGrammar.model }
  
  let newStep ?(term = None) ?(var = None) ?(rBody = None)
              ?(acceptedString = "") 
              ?(input = "") ?(stack = "") ?(production = "") 
              ?(recog = "") ?(left = "") 
              ?(accepted = None) ?(nodes = []) simple =
    (* let dollar = String.make 1 dollar in
    let input = if simple then input else input ^ dollar in
    let stack = if simple then stack else stack ^ dollar in *)
    {
      syntaxTable = {term; var; rBody};
      acceptedString = acceptedString;
      acceptTable = {input; stack; production};
      recognized = {recog; left};
      accepted = accepted;
      nodes = nodes
    }
    
  
  (*type rule = CFGSyntax.rule*)
  

  let rec doWordGenerateEmptyX w seen (rep:t) =
    let rec doGenerateEmpty x =
      if List.mem x seen
      then false
      else(
		    let bodies = bodiesOfHead x rep.rules in
		    Set.exists (fun b -> doWordGenerateEmptyX b (x::seen) rep) bodies 
		  )
		in      
      List.for_all doGenerateEmpty w

  let removeEpsilonFromWord w =
    List.filter (fun c -> c <> epsilon) w

  let removeDollarFromWord w =
    List.filter (fun c -> c <> dollar) w

  let doWordGenerateEmpty w (rep:t) =
    doWordGenerateEmptyX (removeDollarFromWord w) [] rep
    
  let printRepresentation (rep:t) =
    Printf.printf "Alphabet = "; Util.printAlphabet rep.alphabet;
    Printf.printf "Variables = "; Util.printAlphabet rep.variables;
    Printf.printf "Initial = %s\n" (symb2str rep.initial);
    Printf.printf "Rules {\n"; Set.iter (fun {head=h; body=b} -> Printf.printf "\t%s -> %s\n" (symb2str h) (word2str b)) rep.rules;
    Printf.printf "}\n\n"

  let rec print_tuples = (*TEST*)
    function
    | [] -> ()
    | (a, b) :: rest ->
      Printf.printf "%c -> " a;
      Util.printAlphabet b;
      print_tuples rest
      
  let rec print_list = (*TEST*)
    function
    | [] -> Printf.printf "";
    | x::xs ->
      Printf.printf "%c" x;
      print_list xs  
  
  (*Given a variable, returns all rules with variable as head*)
  let sameHeadRules (testSymbol:symbol) (rep:t) =
	  Set.toList (Set.filter (fun r -> testSymbol = r.head) rep.rules)

  let rec pairs l =
    match l with
      | [] -> []
      | x::xs -> List.map (fun v -> (x,v)) xs :: pairs xs

  (*Given a word and variable, returns the word behind the variable*)
  let rec behindSymbol (word:word) (var:variable) = 
    match word with
    | [] -> []
    | x::xs -> if x <> var then x::behindSymbol xs var else []


  let rec leftRecursionTest initial (seen:variable Set.t) (rep:t) =
    if Set.belongs initial rep.alphabet then false else (*rule starting with a terminal symbol can't be left recursive*)
      let ruleBodies = Set.toList (bodiesOfHead initial rep.rules) in (* example: rulesBodies = [['B';'a']; ['b']*)
(*        Printf.printf "initial = %c\n" initial;*)
      let rec leftRecursionTest2 head body seen (rep:t) =
        let wordBehind = behindSymbol body head in
        let behindGenerateEmpty = doWordGenerateEmpty wordBehind rep in
        let body = if behindGenerateEmpty 
                     then List.filter (fun x -> not (List.mem x wordBehind)) body 
                     else body
        in
        
        match body with
        | [] -> false
        | x::xs when x = head || Set.belongs x seen -> true
        | x::xs -> leftRecursionTest x (Set.cons x seen) rep in
        
      List.exists (fun x -> x = true) (List.map (fun x -> leftRecursionTest2 initial x seen rep) ruleBodies)

      
  let isLeftRecursive (rep:t) = 
    Set.exists (fun x -> x = true) (Set.map (fun v -> leftRecursionTest v Set.empty rep) rep.variables)

  let isLL1Deterministic simple (rep:t) =
    let variables = rep.variables in
    let pairsSet = Set.map (fun v -> Set.make (List.flatten (pairs (sameHeadRules v rep)))) variables in
    let lookaheadInterSet = Set.flatMap (fun v -> Set.map (fun (p1,p2) -> Set.inter (lookahead p1 simple rep) (lookahead p2 simple rep)) v) pairsSet in
      Set.for_all (fun x -> Set.size x = 0) lookaheadInterSet

  let isLL1 simple (rep:t) = 
    isLL1Deterministic simple rep
  
  (*given a production X->a, does lookahead(X->a), b, and returns pair ((X,b),a)*)
  let lookahead2Tuple rule simple (rep:t) =
    let lookahead = lookahead rule simple rep in 
      Set.map (fun l -> ((rule.head, l), rule.body)) lookahead
  
  let createParsingTable simple (rep:t) = 
    let lookaheadSet = Set.flatMap (fun r -> lookahead2Tuple r simple rep) rep.rules in
      lookaheadSet
  
  let hasParsingTableConflict simple (rep:t) =
    let parsingTable = createParsingTable simple rep in
    let repeatsTbl = Hashtbl.create (Set.size parsingTable) in
    let getRepeatNum c repeatsTbl =
      let repeat = Hashtbl.find_opt repeatsTbl c in
      match repeat with
      | None -> Hashtbl.add repeatsTbl c 1; false
      | Some a -> true
    in
    let boolResults = Set.map (fun ( (v,t), _ ) -> getRepeatNum (v,t) repeatsTbl ) parsingTable in
    Set.exists (fun r -> r) boolResults
    
  
  (*accept*)
  
(*  let printParsingInfo entry stack sub isSub =*)
(*    Printf.printf "\t"; print_list entry;*)
(*    Printf.printf "\t"; print_list stack;*)
(*    if isSub*)
(*      then (Printf.printf "\t%c->" (List.nth stack 0); print_list sub;)*)
(*      else Printf.printf "\t";*)
(*    Printf.printf "\n"*)
(*  *)
(*  (*given the entry, stack and parsingTable, rewrites the leftmost*)*)
(*  (*variable on the stack with its respective parsingTable rule*)*)
(*  let ruleRewrite (entry:word) (stack:word) parsingTable =*)
(*    let entryChar = List.nth entry 0 in*)
(*    let stackChar = List.nth stack 0 in*)
(*    let parsingTableList = Set.toList parsingTable in*)
(*    let substitution = List.assoc (stackChar, entryChar) parsingTableList in*)
(*      match stack with*)
(*      | [] -> []*)
(*      | x::xs ->*)
(*                printParsingInfo entry stack substitution true;*)
(*                substitution@xs*)
(*  *)
(*  let rec acceptX entry stack parsingTable (rep:t) =*)
(*    match entry with*)
(*    | [] -> if doWordGenerateEmpty stack rep then true else false*)
(*    | x::xs -> match stack with*)
(*                | [] -> false*)
(*                | x2::xs2 -> if Set.belongs x2 rep.variables*)
(*                             then*)
(*                                let newStack = ruleRewrite entry stack parsingTable in*)
(*                                acceptX entry newStack parsingTable rep*)
(*                             else if x=x2 *)
(*                                  then (printParsingInfo entry stack [] false;*)
(*                                       acceptX xs xs2 parsingTable rep )*)
(*                                  else false*)
(*  *)
(*  let acceptZ word rep = *)
(*    Printf.printf "\t"; Printf.printf "Entry: ";*)
(*    Printf.printf "\t"; Printf.printf "Stack: ";*)
(*    Printf.printf "\t"; Printf.printf "Rule: \n";*)
(*    let parsingTable = createParsingTable rep in*)
(*      try (acceptX word [rep.initial] parsingTable rep) *)
(*        with Not_found -> Printf.printf "\t\t\tApplicable rule not found!\n"; false*)

  let word2tree w (rep:t) =
    let rec word2tree2 w =
    match w with
    | [] -> []
    | x::xs -> (if Set.belongs x rep.alphabet
                then Leaf x
                else Root(x,[]))
                :: word2tree2 xs
    in
    
    if List.length w = 0
    then [Leaf epsilon]
    else word2tree2 w

  let rec acceptX entry stack parsingTable (currPerm:symbol list) simple (rep:t) =
    match entry with
    | [] -> [] (*Not supposed to happen*)
    | x::xs when x = dollar ->
          (match stack with
          | [] -> [] (*Not supposed to happen*)
          | x::xs -> if doWordGenerateEmpty [x] rep
                      then
                        (
                          if x = dollar
                          then [newStep ~acceptedString:(word2str currPerm)
                               ~input:(word2str entry)
                               ~stack:(word2str stack)
                               ~recog:(word2str (currPerm))
                               ~accepted:(Some true)
                                simple]
                          else (newStep ~var:(Some (List.hd stack))
                              ~term:(Some dollar)
                              ~rBody:(Some [])
                              ~acceptedString:(word2str currPerm)
                              ~input:(word2str entry)
                              ~stack:(word2str stack) 
                              ~production:(symb2str (List.hd stack) ^ " -> " ^ "") 
                              ~recog:(word2str currPerm)
                              ~nodes:(word2tree [] rep)
                              simple) :: acceptX entry xs parsingTable currPerm simple rep
                        )
                      else [newStep ~var:(Some (List.hd stack))
                      ~term:(Some dollar)
                      ~rBody:(Some [])
                      ~acceptedString:(word2str currPerm) 
                      ~input:(word2str entry)
                      ~stack:(word2str stack)
                      ~recog:(word2str currPerm) ~left:(word2str (removeDollarFromWord stack))
                      ~accepted:(Some false)
                      simple]
             )
    | x::xs -> match stack with
                | [] -> [] (*Not supposed to happen*)
                | [epsilon] -> [newStep ~acceptedString:(word2str currPerm)
                                ~input:(word2str entry)
                                ~stack:(word2str stack)
                                ~recog:(word2str currPerm) ~left:(word2str (removeDollarFromWord stack))
                                ~accepted:(Some false)
                                simple]
                | x2::xs2 -> if Set.belongs x2 rep.variables
                             then
                                let entryChar = List.nth entry 0 in
                                let stackChar = List.nth stack 0 in
                                let parsingTableList = Set.toList parsingTable in
                                let substitution = List.assoc_opt (stackChar, entryChar) parsingTableList in
                                match substitution with
                                  | None -> [newStep ~term:(Some entryChar) ~var:(Some stackChar)
                                                     ~acceptedString:(word2str currPerm)
                                                     ~input:(word2str entry) ~stack:(word2str stack)
                                                     ~recog:(word2str currPerm) ~left:(word2str (removeDollarFromWord stack))
                                                     ~accepted:(Some false)
                                                     simple]
                                  | Some s -> let newStack = 
                                                match stack with
                                                | [] -> []
                                                | x::xs -> s@xs 
                                              in
                                              (newStep ~term:(Some entryChar) ~var:(Some stackChar) ~rBody:(Some s)
                                                       ~acceptedString:(word2str currPerm)
                                                       ~input:(word2str entry) ~stack:(word2str stack) ~production:(symb2str (List.nth stack 0) ^ " -> " ^ word2str s)
                                                       ~recog:(word2str currPerm) ~left:(word2str (removeDollarFromWord stack))
                                                       ~nodes:(word2tree s rep)
                                                       simple) :: acceptX entry newStack parsingTable currPerm simple rep
                              else if x=x2 
                                  then
                                    let newCurrPerm = currPerm @ [x] in
                                    (newStep ~acceptedString:(word2str newCurrPerm)
                                             ~input:(word2str entry) ~stack:(word2str stack)
                                             ~recog:(word2str newCurrPerm) ~left:(word2str (List.tl (removeDollarFromWord stack)))
                                             simple) :: acceptX xs xs2 parsingTable newCurrPerm simple rep 
                                  else [newStep ~acceptedString:(word2str currPerm)
                                                ~input:(word2str entry) ~stack:(word2str stack) 
                                                ~recog:(word2str currPerm) ~left:(word2str (removeDollarFromWord stack))
                                                ~accepted:(Some false)
                                                simple]
  
  let acceptZ word simple (rep:t) = 
    let word = word @ [dollar] in
    let initial = [rep.initial] @ [dollar] in
    let parsingTable = createParsingTable simple rep in
      (newStep ~input:(word2str word) ~stack:(word2str initial) ~nodes:[Root(rep.initial,[])] simple)
      ::acceptX word initial parsingTable [] simple rep

  let rec acumFixedPoint (f: 'a Set.t -> 'a Set.t) (x: 'a Set.t): 'a Set.t =
    let next = Set.union x (f x) in
      if x = next then x
      else acumFixedPoint f next
  
  (*productive symbols*)
  
  (*given a rule and a set of productive variables, verifies if given*)
  (*rule is productive*)
  let isRuleProductive r prodVars (rep:t) =
(*    Printf.printf "\t\t\tisRuleProductive - Prod = %s   prodVars = %s\n" (word2str r) (word2str (Set.toList prodVars));*)
    List.for_all (fun c -> (*Printf.printf "\t\t\t\tc = %c %b\n" c (Set.belongs c rep.alphabet || Set.belongs c prodVars);*) Set.belongs c rep.alphabet || Set.belongs c prodVars) r
      
  (*given a variable and a set of productive variables, verifies if given *)
  (*variable is productive*)
  let isSymbolProductive h prodVars (rep:t) =
    let rules = bodiesOfHead h rep.rules in
      Set.exists (fun r -> 
(*                          Printf.printf "\t\tProduction = %c -> %s\n" h (word2str r);*)
                          isRuleProductive r prodVars rep
                  ) rules
      
        
  let rec productiveSymbolsFP (rep:t) varP =
    Set.filter (fun v -> (*Printf.printf "\tVar %c\n" v;*) isSymbolProductive v varP rep) (Set.diff rep.variables varP)
  
  (*show the productive symbols of the current grammar*)
  let productiveSymbols (rep:t) =
    acumFixedPoint (productiveSymbolsFP rep) Set.empty
  
  (*show the simplified grammar with only productive symbols*)
  (*TODO Confirm correct new model*)
  let productiveGrammarRewrite (rep:t) =
    let prodSyms = productiveSymbols rep in
(*    Printf.printf "Productive Symbols:\n";*)
(*    Set.iter (fun s -> Printf.printf "\t%c\n" s) prodSyms;*)
(*    Printf.printf "\n";*)
    let unprodSyms = Set.diff rep.variables prodSyms in
(*    Printf.printf "Unproductive Symbols:\n";*)
(*    Set.iter (fun s -> Printf.printf "\t%c\n" s) unprodSyms;*)
(*    Printf.printf "\n";*)
    let newRules = Set.filter (fun r -> Set.belongs r.head prodSyms && List.for_all (fun c -> not (Set.belongs c unprodSyms)) r.body) rep.rules in
(*    Printf.printf "New productions:\n";*)
(*    Set.iter (fun {head=h;body=b} -> Printf.printf "\t%c -> %s\n" h (word2str b)) newRules;*)
      new ContextFreeGrammar.model (Arg.Representation {
								alphabet = rep.alphabet; (*TODO Get productive alphabet*)
								variables = prodSyms;
								initial = rep.initial;
								rules = newRules
						} )

  
  (*accessible symbols*)
  
  (*given a rule and a set of accessible symbols, adds all symbols from the*)
  (*rule to the set*)
  let ruleAccessibleSymbols r aSymbols =
    Set.flatten (Set.make (List.map (fun s -> Set.cons s aSymbols) r))

  let rulesAccessibleSymbols h aSymbols (rep:t) =
    let rules = bodiesOfHead h rep.rules in
      Set.flatMap (fun r -> ruleAccessibleSymbols r aSymbols) rules
  
  let rec accessibleSymbolsX (rep:t) aSymbols =
    let vars = Set.filter (fun v -> Set.belongs v rep.variables) aSymbols in (*Remove terminals*)
      Set.flatMap (fun v -> rulesAccessibleSymbols v aSymbols rep) vars
  
  (*show the accessible symbols of the current grammar*)
  let accessibleSymbols (rep:t) = 
    Util.fixedPoint (accessibleSymbolsX rep) (Set.make [rep.initial])
  
  (*TODO Confirm correct new model*)
  let accessibleGrammarRewrite (rep:t) =
    let accessSymbs = accessibleSymbols rep in
    let accessTerms = Set.filter (fun s -> Set.belongs s rep.alphabet) accessSymbs in
    let accessVars = Set.filter (fun s -> Set.belongs s rep.variables) accessSymbs in
    let rules = Set.filter (fun r -> Set.belongs r.head accessVars) rep.rules in
      new ContextFreeGrammar.model (Arg.Representation {
								alphabet = accessTerms;
								variables = accessVars;
								initial = rep.initial;
								rules = rules
						} )

  let clean (rep:t) =
    let prodRewrite = {tType = cleanProductiveTransform; grammar = productiveGrammarRewrite rep} in
    let accessRewrite = {tType = cleanAccessibleTransform; grammar = accessibleGrammarRewrite prodRewrite.grammar#representation} in
    [prodRewrite; accessRewrite]
(*    accessibleGrammarRewrite (productiveGrammarRewrite rep)#representation*)

  let isCFGFullyProductive (rep:t) =
    Set.equals (productiveSymbols rep) (rep.variables)

  let isCFGFullyAccessible (rep:t) =
    Set.equals (accessibleSymbols rep) (Set.union rep.variables rep.alphabet)
    
  let isClean (rep:t) =
    isCFGFullyProductive rep && isCFGFullyAccessible rep
  
  let getNewVar vs =
    let chars = Set.make ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'] in
    let symbs = Set.map char2symb chars in
    let acceptableVars = Set.diff symbs vs in
      Set.nth acceptableVars 0


  let rec leftCorner2 symbol seen (rep: t) =
    match symbol with
    | [] -> Set.empty
    | x::xs ->
      if Set.belongs x seen 
      then Set.make [x]
      else 
        if Set.belongs x rep.alphabet
        then Set.make [x]
        else Set.union 
              (Set.make [x])
              (Set.flatMap (fun b -> (leftCorner2 b (Set.cons x seen) rep)) (bodiesOfHead x rep.rules))

  let leftCorner symbol rep =
    leftCorner2 [symbol] Set.empty rep

    
  (*left recursion removal*)

  let sortLeftCorner l =
    let sortFun e1 e2 =
      let (_, e1) = e1 in
      let (_, e2) = e2 in
      if List.length e1 > List.length e2 then -1
      else (if List.length e1 < List.length e2 then 1 else 0)
    in
    List.sort sortFun l

  let addToMap map varL =
    let rec addToMap2 map varL value =
      match varL with
      | [] -> ()
      | x::xs -> (*Printf.printf "Adding var %c with value %d\n" x value;*)
                  Hashtbl.add map x value; addToMap2 map xs (value+1)
    in
    addToMap2 map varL 0

(* TODO Util.printWord does not exist anymore *)
(*  let rec print_rules r =*)
(*    match r with*)
(*    | [] -> Printf.printf "\n"*)
(*    | x::xs -> Printf.printf " - %c ->" x.head; Util.printWord x.body; print_rules xs*)


  let removeDirectLeftRecursion (rep:t) = 
    let hasRuleDirectLeftRecursion r =
      match r.body with
      | [] -> false
      | x::xs when x = r.head -> true
      | x::xs -> false
    in
    
    let recursiveRuleRewrite r nV =
      let body =
        match r.body with
        | [] -> [epsilon] (*Not reacheable*)
        | x::xs -> xs@[nV]
      in
      { head = nV; body = body }
    in
      
    let nRecursiveRuleRewrite r nV =
      let body = r.body@[nV] in
        { head = r.head; body = body }
    in
    
    let rec removeDLRFromVar v drs ndrs nV =
      if v = Set.empty then Set.empty
      else let (x,xs) = Set.cut v in
(*        Printf.printf "\tRemoving direct left recursion from variable %c\n" x;*)
        let recursiveRs = Set.filter (fun r -> r.head = x) drs in
        let nRecursiveRs = Set.filter (fun r -> r.head = x) ndrs in
        let newVar = getNewVar nV in
(*        Printf.printf "\tNew variable is %c\n" newVar;*)
        let recRulesRewriteTmp = Set.map (fun r -> recursiveRuleRewrite r newVar) recursiveRs in
        let recRulesRewrite = Set.cons ( {head = newVar; body = []} ) recRulesRewriteTmp in
        let nRecRulesRewrite = Set.map (fun r -> nRecursiveRuleRewrite r newVar) nRecursiveRs in
        let newRules = Set.union recRulesRewrite nRecRulesRewrite in
(*        print_rules (Set.toList newRules);*)
          Set.union newRules (removeDLRFromVar xs drs ndrs (Set.cons newVar nV))
    in
    
    let leftRecursiveRules = Set.filter (fun r -> hasRuleDirectLeftRecursion r) rep.rules in
(*        print_rules (Set.toList leftRecursiveRules);*)
    let leftRecursiveVars = Set.map (fun r -> r.head) leftRecursiveRules in
    let nonLeftRecursiveRules = Set.diff rep.rules leftRecursiveRules in
(*        print_rules (Set.toList nonLeftRecursiveRules);*)
    let nonLeftRecursiveRulesClean = Set.filter (fun {head = h; body = _} -> not (Set.belongs h leftRecursiveVars)) nonLeftRecursiveRules in
    let newRules = Set.union nonLeftRecursiveRulesClean (removeDLRFromVar leftRecursiveVars leftRecursiveRules nonLeftRecursiveRules rep.variables) in
(*      print_rules (Set.toList newRules);*)
    let newVars = Set.union rep.variables (Set.map (fun r -> r.head) newRules) in
      new ContextFreeGrammar.model (Arg.Representation {
		    alphabet = rep.alphabet;
		    variables = newVars;
			  initial = rep.initial;
			  rules = newRules
		  } )


  let rec removeIndirectLeftRecursion map varL (rep:t) = 
    match varL with
    | [] -> new ContextFreeGrammar.model (Arg.Representation {
		          alphabet = rep.alphabet;
		          variables = rep.variables;
			        initial = rep.initial;
			        rules = rep.rules
		        } )
    | var::xs -> 
      let perVarIndirectRemoval map var (rep:t) =
        let perVarProdIndirectRemoval prodHead iVal prodBody rhsValues (rep:t) =
          let results = Set.flatMap (
            fun (jVal, rhsBody) ->
              match jVal with
              | None -> Set.make [{head = prodHead; body = rhsBody}]
              | Some jVal -> 
                if iVal > jVal
                then (
                  let rhsVar = (List.hd rhsBody) in
                  let rhsVarBodies = bodiesOfHead rhsVar rep.rules in
                  let replaceRules = Set.flatMap (fun rhsBody ->
                    if List.length prodBody >= 1
                    then (
                      if List.hd prodBody = rhsVar
                      then Set.make [{head = prodHead; body = rhsBody@(if List.length prodBody >= 1 then List.tl prodBody else prodBody)}]
                      else Set.make []
                    )
                    else Set.make [] 
                  ) rhsVarBodies 
                  in
                  replaceRules
                )
                else  Set.make [{head = prodHead; body = rhsBody}]
          ) rhsValues in
          results
        in
        let iVal = Hashtbl.find_opt map var in
        match iVal with
        | None -> Set.filter (fun {head=h;body=_} -> h=var) rep.rules
        | Some iVal -> (
          let varRules = bodiesOfHead var rep.rules in
          let rhsValues = Set.map (
            fun b -> 
              if List.length b >= 1 
              then (Hashtbl.find_opt map (List.hd b), b)
              else (None, b)
          ) varRules
          in
          Set.flatMap (fun b ->
            let r = perVarProdIndirectRemoval var iVal b rhsValues rep in
            r
          ) varRules)
      in
      let newProds = Set.flatMap (fun v -> perVarIndirectRemoval map v rep) rep.variables in
      let newGrammar = new ContextFreeGrammar.model (Arg.Representation {
         alphabet = rep.alphabet;
         variables = rep.variables;
         initial = rep.initial;
         rules = newProds
       } ) in
      let newGrammar = removeDirectLeftRecursion newGrammar#representation in
      removeIndirectLeftRecursion map xs newGrammar#representation


  let removeLeftRecursion (rep:t) =
    let map = Hashtbl.create (Set.size rep.variables) in
    let leftCornerTest = List.map (fun v -> (v, (Set.toList (leftCorner v rep))) ) (Set.toList rep.variables) in
    let sortedLeftCornerTest = sortLeftCorner leftCornerTest in
    addToMap map (List.map (fun (v,_) -> v) sortedLeftCornerTest);
    let sortedVars = List.map (fun (s,_) -> s) sortedLeftCornerTest in
    let result = removeIndirectLeftRecursion map sortedVars rep in
      {tType = leftRecursionRemovalTransform; grammar = result}
      
  (*left factoring*)
  
  let rec lcp l1 l2 =
    match l1 with
    | [] -> []
    | x1::xs1 -> match l2 with
                | [] -> []
                | x2::xs2 -> if x1=x2 then [x1]@(lcp xs1 xs2) else []
  
  let perVarLCP v rs =
    let rules = Set.filter (fun r -> r.head = v) rs in
    let combos = List.flatten (pairs (Set.toList rules)) in
    let lcpList = List.map ( fun (r1,r2) -> lcp r1.body r2.body) combos in
    let lcpList = List.filter (fun l -> l <> []) lcpList in
      Set.toList (Set.make lcpList) (*Remove repeats*)
  
  let rec sameRuleFactoring nV p rb =
    match p with
    | [] -> [nV]
    | x::xs -> match rb with
              | [] -> []
              | x2::xs2 -> [x2]@sameRuleFactoring nV xs xs2
      
  let rec newRuleFactoring rb p =
    match rb with
    | [] -> []
    | x::xs -> match p with
              | [] -> [x]@newRuleFactoring xs p
              | x2::xs2 -> []@newRuleFactoring xs xs2
      
  let rec ruleHasPrefix r p rb =
    match p with
    | [] ->true
    | x::xs -> match rb with
              |[] -> false
              |x2::xs2 -> if x = x2 then ruleHasPrefix r xs xs2 else false
     
  let rec getSmallestLCP l currSmallest =
    match l with
    | [] -> currSmallest
    | x::xs -> if (x <> [] && List.length x < List.length currSmallest)
               then getSmallestLCP xs x
               else getSmallestLCP xs currSmallest
      
  let rec getBiggestList ll currBiggest =
    match ll with
    | [] -> currBiggest
    | x::xs -> let length = List.length x in
                if length > currBiggest
                then getBiggestList xs length
                else getBiggestList xs currBiggest
      
  let rec createLargeList size =
    match size with
    | 0 -> []
    | _ -> [symb "a"] @ createLargeList (size-1)
      
  let rec perVarFactoring pair allVars (rep:t) = (* pair = ('A', ['a']) *)
    if pair = Set.empty then Set.empty
    else let (x,xs) = Set.cut pair in
      let var = fst x in
      let prefix = snd x in
(*     Printf.printf "prefix = "; print_list prefix; Printf.printf "\n";*)
      let varRules = Set.filter (fun r -> r.head = var) rep.rules in
      let prefixedRules = Set.filter (fun r -> ruleHasPrefix r prefix r.body) varRules in
(*     Printf.printf "prefixedRules = "; Util.println (CFGSyntax.toStringList prefixedRules);*)
      let nonPrefixedRules = Set.filter (fun r -> not (ruleHasPrefix r prefix r.body)) varRules in
(*     Printf.printf "nonPrefixedRules = "; Util.println (CFGSyntax.toStringList nonPrefixedRules);*)
      let newVar = getNewVar allVars in
(*     Printf.printf "newVar = %c\n" newVar;*)
      let newSameHeadRulesSet = Set.map (fun r -> { head = var; body = sameRuleFactoring newVar prefix r.body } ) prefixedRules in
      let newHeadRulesSet = Set.map (fun r -> { head = newVar; body = newRuleFactoring r.body prefix } ) prefixedRules in
      let rules = Set.union nonPrefixedRules (Set.union newSameHeadRulesSet newHeadRulesSet) in
(*     print_rules (Set.toList rules);*)
        Set.union rules (perVarFactoring xs (Set.cons newVar allVars) rep)
  
  let getPerVarLCPResult (rep:t) = 
    let perVarLCPResult = Set.map (fun v -> (v, perVarLCP v rep.rules)) rep.variables in
    let perVarLCPResult = Set.filter (fun (_,l) -> l <> []) perVarLCPResult in
      Set.map ( fun (v,l) -> (v, getSmallestLCP l (createLargeList ((getBiggestList l 0)+1))) ) perVarLCPResult

  let isLeftFactoring (rep:t) =
    Set.map (fun (v,l) -> v) (getPerVarLCPResult rep) <> Set.empty

  let rec leftFactoring (rep:t) =
    let perVarLCPResult = getPerVarLCPResult rep in
(*    Printf.printf "perVarLCPResult = "; Set.iter (fun (v,l) -> Printf.printf "%c, " v; print_list l) perVarLCPResult; Printf.printf "\n";*)
    let variablesToFactorize = Set.map (fun (v,l) -> v) perVarLCPResult in
(*    Printf.printf "Variables to factorize = "; print_list (Set.toList variablesToFactorize); Printf.printf "\n";*)
    let unchangedVariables = Set.diff rep.variables variablesToFactorize in
(*    Printf.printf "Unchanged variables = "; print_list (Set.toList unchangedVariables); Printf.printf "\n";*)
    let unchangedRules = Set.filter (fun {head = h; body = _} -> Set.belongs h unchangedVariables) rep.rules in
    let newRules = perVarFactoring perVarLCPResult rep.variables rep in
    let newVars = Set.map (fun ({head=v;body=_}) -> v ) newRules in
    let newGrammar = new ContextFreeGrammar.model (Arg.Representation {
	      alphabet = rep.alphabet;
	      variables = Set.union rep.variables newVars;
	      initial = rep.initial;
	      rules = Set.union newRules unchangedRules
	    } ) in
    if isLeftFactoring newGrammar#representation 
    then leftFactoring newGrammar#representation 
    else {tType = leftFactoringTransform; grammar = newGrammar}

  let hasEmptyProductions (rep:t) =
    let nullableVars = Set.filter (fun v -> doWordGenerateEmpty [v] rep) rep.variables in
    Set.size nullableVars <> 0

  let removeEmptyProductions2 (rep:t) = 
    let rec combi vars body =
      match body with
      | [] -> Set.make [[]]
      | x::xs -> let res = combi vars xs in
                  (*Printf.printf "Current body symbol is %c\n" x;
                  Printf.printf "res = \n";
                  Set.iter (fun l -> Printf.printf "\t%s\n" (word2str l)) res;*)
                  Set.flatMap (fun v ->
                                (*(if x = v
                                then (
                                  Printf.printf "\tx = v (%c = %c)\n" x v;
                                  Set.iter (fun p -> Printf.printf "\t\t{%s}\n" (word2str p)) (Set.union res (Set.map (fun l -> v::l) res))
                                )
                                else (
                                  Printf.printf "\tx =/= v (%c =/= %c)\n" x v;
                                  Set.iter (fun p -> Printf.printf "\t\t{%s}\n" (word2str p)) (Set.map (fun l -> x::l) res)
                                ));*)
                                if x = v
                                then Set.union res (Set.map (fun l -> v::l) res)
                                else Set.map (fun l -> x::l) res
                  ) vars
    in
    let changeProds vars prod = 
      let {head=h; body=b} = prod in
      if List.length b = 0 then Set.empty
      else (
        let prodBodiesSet = Set.filter (fun p -> List.length p <> 0) (combi vars b) in
        Set.map (fun b -> {head = h; body = b} ) prodBodiesSet
      )
    in
    let nullableVars = Set.filter (fun v -> doWordGenerateEmpty [v] rep) rep.variables in
    if Set.size nullableVars = 0 
    then (
      new ContextFreeGrammar.model (Arg.Representation {
	        alphabet = rep.alphabet;
	        variables = rep.variables;
	        initial = rep.initial;
	        rules = rep.rules
	      })
    )
    else (
      let toChangeProds = Set.filter (fun {head=h;body=b} -> 
                                        Set.exists (
                                          fun v -> List.length b >= 1 && List.mem v b
                                        ) nullableVars
                           ) rep.rules 
      in
      let unchangedProds = Set.filter (
                            fun p -> List.length p.body >= 1
                           ) (Set.diff rep.rules toChangeProds) in
      let newProds = Set.flatMap (changeProds nullableVars) toChangeProds in
(*      Set.iter (fun p -> Printf.printf "{%c;%s}\n" p.head (word2str p.body) ) newProds;*)
(*      if Set.belongs rep.initial nullableVars
      then (
        let newInitial = getNewVar rep.variables in
        let newInitialProds = Set.make [ { head = newInitial; body = []}; { head = newInitial; body = [rep.initial]} ] in
        let newProds = Set.union newInitialProds newProds in
        new ContextFreeGrammar.model (Arg.Representation {
	        alphabet = rep.alphabet;
	        variables = Set.cons newInitial rep.variables;
	        initial = newInitial;
	        rules = Set.union newProds unchangedProds
	      } )
      ) else ( *)
        new ContextFreeGrammar.model (Arg.Representation {
	        alphabet = rep.alphabet;
	        variables = rep.variables;
	        initial = rep.initial;
	        rules = Set.union newProds unchangedProds
	      } (* ) *)
      )
    )
    
  
  let removeEmptyProductions (rep:t) =
    { tType = epsilonRemovalTransform; grammar = removeEmptyProductions2 rep }
  

  let isUnitProd body (rep:t) =
    let rec isUnitProd2 cS cB p =
      match cB with
      | [] -> false
      | x::xs -> if doWordGenerateEmpty cB rep && doWordGenerateEmpty p rep
                  then true
                  else isUnitProd2 x xs (p@[cS])
    in
    let isUnitProdAux r (rep:t) =
      match r with
      | [] -> false
      | x::xs -> isUnitProd2 x xs []
    in
    if (List.length body = 1 && Set.belongs (List.hd body) rep.variables) 
    then true 
    else (
      if List.length body > 1 && List.for_all ( fun c -> Set.belongs c rep.variables ) body
        then isUnitProdAux body rep 
        else false
      )

  let hasUnitProductions (rep:t) =
    Set.size (Set.filter (fun {head = _; body = b} -> isUnitProd b rep ) rep.rules) <> 0


  let rec findUnitPair2 cS cB p (rep:t) =
    match cB with
    | [] -> []
    | x::xs -> if doWordGenerateEmpty cB rep && doWordGenerateEmpty p rep
                then [cS]
                else findUnitPair2 x xs (p@[cS]) rep

  let findUnitPairAux r (rep:t) =
    match r with
    | [] -> []
    | x::xs -> findUnitPair2 x xs [] rep
             
  let rec findUnitPairX origVar var seen (rep:t) =
    if Set.belongs var seen then [] else (
      let rules = bodiesOfHead var rep.rules in
      let results = List.flatten (
                      List.map (fun r -> 
                          if List.length r = 1 && Set.belongs (List.hd r) rep.variables
                          then (
                            if Set.belongs (List.hd r) seen
                            then []@findUnitPairX origVar (List.hd r) (Set.cons var seen) rep
                            else r@findUnitPairX origVar (List.hd r) (Set.cons var seen) rep
                          )
                          else  findUnitPairAux r rep 
                      ) (Set.toList rules)
                    ) 
      in
      results
    )
    
  let findUnitPair var (rep:t) =
    let results = List.map (fun r -> (var, r)) (findUnitPairX var var Set.empty rep) in
    [(var, var)] @ results
(*    (var,(findUnitPairX var Set.empty rep))*)

  (*Used to sort unit pair lists by biggest length to lowest length*)
  let compareUnitPairList l1 l2 =
    if List.length l1 > List.length l2 then -1
    else (if List.length l1 < List.length l2 then 1
    else 0)  
   
  let getNonUnitProductions var (rep:t) = 
    let prods = bodiesOfHead var rep.rules in
(*    Printf.printf "var = %c\n" var;*)
(*    Set.iter (fun p -> Printf.printf "\tIs %c -> %s unit? %b\n" var (word2str p) (isUnitProd p rep)) prods;*)
(*    Printf.printf "\n";*)
    Set.filter (fun p -> not (isUnitProd p rep)) prods

  let removeUnitProductions (rep:t) = 
    let perVarPair pair (rep:t) =
      let (h,b) = pair in
      let nUnitProds = getNonUnitProductions b rep in
(*      Set.iter (fun p -> Printf.printf "%c -> %s\n" h (word2str p)) nUnitProds;*)
      Set.toList (Set.map (fun p -> {head = h; body = p}) nUnitProds)
    in
    let perVar pairs (rep:t) =
      List.flatten (List.map (fun p -> perVarPair p rep) pairs)
    in
    let unitPairs = List.map (fun v -> findUnitPair v rep) (Set.toList rep.variables) in
    (*let unitPairs = List.sort compareUnitPairList unitPairs in*)
    let newProds = List.flatten (
                    List.map (fun l ->
                      perVar l rep
                     ) unitPairs 
                   ) in
    let result = new ContextFreeGrammar.model (Arg.Representation {
	      alphabet = rep.alphabet;
	      variables = rep.variables;
	      initial = rep.initial;
	      rules = Set.make newProds
	    } )
	  in
	    {tType = unitRemovalTransform; grammar = result}

    
    
  let generateRecursiveDescendentParser lang (rep:t) =
    match String.lowercase_ascii lang with
      | "c" -> let parser = new RDParserC.parser in parser#build rep
      | "ocaml" -> let parser = new RDParserOCaml.parser in parser#build rep
      | "java" -> let parser = new RDParserJava.parser in parser#build rep
      | _ -> "Language " ^ lang ^ " is not supported.\n"


  let transformToLL1 (rep:t) =
    let transform1 = {tType = epsilonRemovalTransform; grammar = (removeEmptyProductions rep).grammar} in
    let transform2 = {tType = unitRemovalTransform; grammar = (removeUnitProductions transform1.grammar#representation).grammar} in
    let cleanResult = clean transform2.grammar#representation in
    let transform3 = {tType = cleanProductiveTransform; grammar = (List.nth cleanResult 0).grammar} in
    let transform4 = {tType = cleanAccessibleTransform; grammar = (List.nth cleanResult 1).grammar} in
    let transform5 = {tType = leftRecursionRemovalTransform; grammar = (removeLeftRecursion transform4.grammar#representation).grammar} in
    let transform6 = {tType = leftFactoringTransform; grammar = (leftFactoring transform5.grammar#representation).grammar} in
    [transform1; transform2; transform3; transform4; transform5; transform6]
  

  class model (arg: (t,tx) Arg.alternatives) =
    object(self) inherit ContextFreeGrammar.model arg as super
    
    val mutable simplified = false
    
    method isSimplified = simplified
    method rdparserOpts = [ "OCaml"; "C"; "Java"; "Rust" ]
    method toggleSimplified = Printf.printf "simplified is %b toggling to %b\n" simplified (not simplified);
                              simplified <- not simplified
    
    method follow testSymbol = follow testSymbol simplified self#representation
    method lookahead rule = lookahead rule simplified self#representation
    method isLL1 = isLL1 simplified self#representation
    method isLeftRecursive = isLeftRecursive self#representation
    method createParsingTable = createParsingTable simplified self#representation
    method hasParsingTableConflict = hasParsingTableConflict simplified self#representation
    method acceptZ w = acceptZ w simplified self#representation
    method productiveSymbols = productiveSymbols self#representation
    method accessibleSymbols = accessibleSymbols self#representation
    method productiveRewrite = productiveGrammarRewrite self#representation
    method accessibleRewrite = accessibleGrammarRewrite self#representation
    method clean = clean self#representation
    method isFullyProductive = isCFGFullyProductive self#representation
    method isFullyAccessible = isCFGFullyAccessible self#representation
    method isClean = isClean self#representation
    method removeLeftRecursion = removeLeftRecursion self#representation
    method removeDirectLeftRecursion = removeDirectLeftRecursion self#representation
    method leftFactoring = leftFactoring self#representation
    method isLeftFactoring = isLeftFactoring self#representation
    method leftCorner s = leftCorner s self#representation
    method hasEmptyProductions = hasEmptyProductions self#representation
    method removeEmptyProductions = removeEmptyProductions self#representation
    method hasUnitProductions = hasUnitProductions self#representation
    method removeUnitProductions = removeUnitProductions self#representation
    method generateRecursiveDescendentParser pLang = generateRecursiveDescendentParser pLang self#representation
    method transformToLL1 = transformToLL1 self#representation
  end
end

module LL1GrammarTests: sig end =
struct
	let active = false

  let example1 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 1",
	  name : "G1",
	  alphabet : ["a", "b", "c", "d", "e"],
	  variables : ["S", "A", "B", "C", "D", "E"],
	  initial : "S",
    rules : ["S -> ABCDE", "A -> a | ", "B -> b | ", "C -> c", "D -> d | ", "E -> e | "]
  } |}

  let example2 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 2",
	  name : "G2",
	  alphabet : ["a", "b", "c", "d"],
	  variables : ["S", "B", "C"],
	  initial : "S",
    rules : ["S -> Bb | Cd", "B -> aB | ", "C -> cC | "]
  } |}
  
  let example3 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 3",
	  name : "G3",
	  alphabet : ["+", "*", "(", ")", "i"],
	  variables : ["E", "D", "T", "U", "F"],
	  initial : "E",
    rules : ["E -> TD", "D -> +TD | ", "T -> FU", "U -> *FU | ", "F -> i | (E)"]
  } |}
  
  let example4 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 4",
	  name : "G4",
	  alphabet : ["a", "b", "c", "f", "g", "h"],
	  variables : ["S", "B", "C", "D", "E", "F"],
	  initial : "S",
    rules : ["S -> aBDh", "B -> cC", "C -> bC | ", "D -> EF", "E -> g | ", "F -> f | "]
  } |}

  let example5 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 5",
	  name : "G5",
	  alphabet : ["n", "+", "*"],
	  variables : ["E", "A", "B"],
	  initial : "E",
    rules : ["E -> nA", "A -> EB | ", "B -> +A | *A"]
  } |}
  
  let example6 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 6",
	  name : "G6",
	  alphabet : ["a", "b"],
	  variables : ["N", "A", "B", "C"],
	  initial : "N",
    rules : ["N -> AB | BA", "A -> a | CAC", "B -> b | CBC", "C -> a | b"]
  } |}
  
  let cfg_dissertation = {| {
	  kind : "context free grammar", 
	  description : "dissertation example",
	  name : "G2",
	  alphabet : ["a", "b", "c"],
	  variables : ["S", "A", "B", "C", "D", "E"],
	  initial : "S",
    rules : ["S -> ABC", "A -> aD", "B -> CE", "C -> c", "D -> AB | ", "E -> bE | "]
  } |}

  let non_deterministic_grammar = {| {
	  kind : "context free grammar", 
	  description : "Non deterministic grammar",
	  name : "N",
	  alphabet : ["a", "b", "d", "g", "h"],
	  variables : ["S", "A", "B", "C"],
	  initial : "S",
    rules : ["S -> ACB | CbB | Ba", "A -> da | BC", "B -> g | ", "C -> h | "]
  } |}

  let accessible_symbols1 = {| {
	  kind : "context free grammar", 
	  description : "Accessible symbols example",
	  name : "AS1",
	  alphabet : ["a", "b"],
	  variables : ["A", "B", "C", "D", "E"],
	  initial : "A",
    rules : ["A -> aBb | bBa", "B -> Cb | bC", "C -> a | aC", "D -> E | Db", "E -> aE | Da"]
  } |}
  
  let accessible_symbols2 = {| {
	  kind : "context free grammar", 
	  description : "Accessible symbols example",
	  name : "AS2",
	  alphabet : ["a", "b"],
	  variables : ["S", "B"],
	  initial : "S",
    rules : ["S -> a", "B -> b"]
  } |}
  
  let productive_symbols1 = {| {
	  kind : "context free grammar", 
	  description : "Productive symbols example",
	  name : "PS1",
	  alphabet : ["a", "b"],
	  variables : ["A", "B", "C", "D", "E"],
	  initial : "A",
    rules : ["A -> aBb | bBa", "B -> CD | aC | Ab", "C -> a | aC", "D -> E | DA", "E -> aE | Da"]
  } |}

  let productive_symbols2 = {| {
	  kind : "context free grammar", 
	  description : "Productive symbols example",
	  name : "PS2",
	  alphabet : ["a", "b"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> a | A", "A -> AB", "B -> b"]
  } |}
  
  let clean_grammar1 = {| {
	  kind : "context free grammar", 
	  description : "Clean example from https://www.cs.scranton.edu/~mccloske/courses/cmps260/cfg_remove_useless.html",
	  name : "Clean1",
	  alphabet : ["a", "b", "c", "d"],
	  variables : ["S", "A", "B", "C", "D"],
	  initial : "S",
    rules : ["S -> aSa | bB | bAA", "A -> a | SbA | aB", "B -> AB | CaB", "C -> cC | Sa | bD", "D -> dD | "]
  } |}

  let direct_left_recursion_grammar1 = {| {
	  kind : "context free grammar", 
	  description : "Direct Left-recursion example 1",
	  name : "DR1",
	  alphabet : ["a", "b"],
	  variables : ["A"],
	  initial : "A",
    rules : ["A -> Aa | b"]
  }  |}
  
  let direct_left_recursion_grammar2 = {| {
	  kind : "context free grammar", 
	  description : "Direct Left-recursion example 2",
	  name : "DR2",
	  alphabet : ["a", "b"],
	  variables : ["B"],
	  initial : "B",
    rules : ["B -> a | Bb"]
  } |}
  
  let indirect_left_recursion_grammar1 = {| {
	  kind : "context free grammar", 
	  description : "Indirect Left-recursion example 1",
	  name : "IR1",
	  alphabet : ["a"],
	  variables : ["S", "A"],
	  initial : "S",
    rules : ["S -> A | a", "A -> S"]
  }  |}

  let indirect_left_recursion_grammar2 = {| {
	  kind : "context free grammar", 
	  description : "Indirect Left-recursion example 2",
	  name : "IR2",
	  alphabet : ["a"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> A | B", "A -> a", "B -> S"]
  }  |}
  
  let indirect_left_recursion_grammar3 = {| {
	  kind : "context free grammar", 
	  description : "Indirect Left-recursion example 2",
	  name : "IR3",
	  alphabet : ["a", "b", "c"],
	  variables : ["S", "A", "B", "C"],
	  initial : "S",
    rules : ["S -> ABCS", "A -> a | ", "B -> b | ", "C -> c | "]
  }  |} 
  
  let indirect_left_recursion_grammar4 = {| {
	  kind : "context free grammar", 
	  description : "Indirect Left-recursion example",
	  name : "IR4",
	  alphabet : ["a", "b", "d", "e", "f", "g"],
	  variables : ["A", "B", "C", "D"],
	  initial : "A",
    rules : ["A -> Ba | b", "B -> Cd | e", "C-> Df | g", "D -> Df | Aa | Cg"]
  }  |}

  let indirect_left_recursion_grammar5 = {| {
	  kind : "context free grammar", 
	  description : "Indirect Left-recursion example 2",
	  name : "IR3",
	  alphabet : ["a"],
	  variables : ["S", "A"],
	  initial : "S",
    rules : ["S -> AS", "A -> a | "]
  }  |}

  let left_factoring_example = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF1",
	  alphabet : ["a", "b", "e", "i", "t"],
	  variables : ["S", "E"],
	  initial : "S",
    rules : ["S -> iEtS | iEtSeS | a", "E -> b"]
  } |}

  let left_factoring_example2 = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF2",
	  alphabet : ["a", "c"],
	  variables : ["A", "B"],
	  initial : "A",
    rules : ["A -> aAB | aBc | aAc", "B ->"]
  } |}

  let left_factoring_example3 = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF3",
	  alphabet : ["a", "b"],
	  variables : ["S"],
	  initial : "S",
    rules : ["S -> bSSaaS | bSSaSb | bSb | a"]
  } |}

  let left_factoring_example4 = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF4",
	  alphabet : ["a", "b"],
	  variables : ["S"],
	  initial : "S",
    rules : ["S -> aSSbS | aSaSb | abb | b"]
  } |}
  
  let left_factoring_example5 = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF5",
	  alphabet : ["a", "b", "c", "d"],
	  variables : ["S"],
	  initial : "S",
    rules : ["S -> a | ab | abc | abcd"]
  } |}
  
  let left_factoring_example6 = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF6",
	  alphabet : ["a", "b", "c", "d"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> aAd | aB", "A -> a | ab", "B -> ccd | ddc"]
  } |}
  
  let unit_removal_example1 = {| {
	  kind : "context free grammar", 
	  description : "unit removal example",
	  name : "UR1",
	  alphabet : ["a", "b"],
	  variables : ["E", "T", "F", "I"],
	  initial : "E",
    rules : ["E -> T", "T -> F", "F -> I", "I -> a | b | Ia | Ib"]
  } |}

  let unit_removal_example2 = {| {
	  kind : "context free grammar", 
	  description : "unit removal example 2",
	  name : "UR2",
	  alphabet : ["a", "b", "c"],
	  variables : ["A", "B", "C"],
	  initial : "A",
    rules : ["A -> B | a", "B -> C | b", "C -> A | c"]
  } |}
  
  let unit_removal_example3 = {| {
	  kind : "context free grammar", 
	  description : "unit removal example 3",
	  name : "UR3",
	  alphabet : ["a", "b", "c"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> Aa | B | c", "A -> a | bc | B", "B -> A | bb"]
  } |}
  
  let unit_removal_example4 = {| {
	  kind : "context free grammar", 
	  description : "unit removal example 4",
	  name : "UR4",
	  alphabet : ["a", "b", "d"],
	  variables : ["S", "A", "B", "C", "D", "E"],
	  initial : "S",
    rules : ["S -> AC", "A -> a", "B -> D", "C -> B | d", "D -> E", "E -> b"]
  } |}
  
  let unit_removal_example5 = {| {
	  kind : "context free grammar", 
	  description : "unit removal example 5",
	  name : "UR5",
	  alphabet : ["a", "b", "0", "1", "(", ")", "+", "*"],
	  variables : ["I", "F", "T", "E"],
	  initial : "E",
    rules : ["E -> T | E+T", "T -> F | T*F", "F -> I | (E)", "I -> a | b | Ia | Ib | I0 | I1"]
  } |}

  let epsilon_removal_example1 = {| {
	  kind : "context free grammar", 
	  description : "epsilon removal example",
	  name : "ER1",
	  alphabet : ["a", "b", "d"],
	  variables : ["S", "A", "B", "C", "D"],
	  initial : "S",
    rules : ["S -> ABaC", "A -> BC", "B -> b | ", "C -> D | ", "D -> d"]
  } |}
  
  let epsilon_removal_example2 = {| {
	  kind : "context free grammar", 
	  description : "epsilon removal example 2",
	  name : "ER2",
	  alphabet : ["a", "b"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> AB", "A -> AaA | ", "B -> BbB | "]
  } |}
  
  let epsilon_removal_example3 = {| {
	  kind : "context free grammar", 
	  description : "epsilon removal example 3",
	  name : "ER3",
	  alphabet : ["a", "b"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> AB", "A -> aAA | ", "B -> bBB | "]
  } |}

  let firstPairConversion_old l = Set.make (List.map (fun (a,b) -> (a, Set.make b)) l)
  let followPairConversion_old l = Set.make (List.map (fun (a,b) -> (a, Set.make b)) l)
  let lookaheadPairConversion_old l = Set.make (List.map (fun (a,b) -> CFGSyntax.parseLine a, Set.make b) l)

  let firstPairConversion l = Set.make (List.map (fun (a,b) -> (symb a, Set.make (List.map char2symb b))) l)
  let followPairConversion l = Set.make (List.map (fun (a,b) -> (char2symb a, Set.make (List.map char2symb b))) l)
  let lookaheadPairConversion l = Set.make (List.map (fun (a,b) -> (Set.nth (CFGSyntax.parseLine a) 0), Set.make (List.map char2symb b)) l)

  let printRepresentation (rep: t) =
    Printf.printf "Alphabet = "; Util.printAlphabet rep.alphabet;
    Printf.printf "Variables = "; Util.printAlphabet rep.variables;
    Printf.printf "Initial = %s\n" (symb2str rep.initial);
    Printf.printf "Rules {\n"; Set.iter (fun {head=h; body=b} -> Printf.printf "\t%s -> %s\n" (symb2str h) (word2str b)) rep.rules;
    Printf.printf "}\n\n"

  let rec testFunction2 f l c =
    if l = Set.empty then ()
    else let ((t,r),xs) = Set.cut l in
      if (f t = r) then () else Printf.printf "\t\tTest %i fails!\n" c;
      testFunction2 f xs (c+1)


	let colorRed = "\027[31m"
	let colorGreen = "\027[32m"
	let colorOff = "\027[0m"

(*	let colorRed = ""*)
(*	let colorGreen = ""*)
(*	let colorOff = ""*)

  let failPrint str =
    Printf.printf "%s" (colorRed ^ str ^ colorOff)
    
  let okPrint str =
    Printf.printf "%s" (colorGreen ^ str ^ colorOff)

  let printResult r =
    if r
    then okPrint "O"
    else failPrint "X"
  
  let printFirstTest t =
    Set.iter (fun (v,s) -> 
      Printf.printf "(%s, [" v;
      Set.iter (fun v -> Printf.printf "%c " v) s;
      Printf.printf "%s" "]) "
    ) t
    
  let compareTheseSets s1 s2 =
    Set.for_all (fun (h1,r1) ->
      Set.exists (fun (h2,r2) -> h1 = h2 && Set.equals r1 r2) s2
    ) s1 && Set.size s1 = Set.size s2

  let testFirst g r =
    let allResults = Set.map (fun v -> (v, g#first [v])) (g#representation : t).variables in
(*    Printf.printf "\n\tComparing:";*)
(*    Set.iter (fun (v,b) -> Printf.printf "\n\t\t%s->\t" (symb2str v); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) b) r;*)
(*    Printf.printf "\n\twith:";*)
(*    Set.iter (fun (v,b) -> Printf.printf "\n\t\t%s->\t" (symb2str v); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) b) allResults;*)
(*    Printf.printf "\n";*)
(*    r = allResults*)
    compareTheseSets r allResults

  let testFollow g r =
    let allResults = Set.map (fun v -> (v, g#follow v)) (g#representation : t).variables in
(*    Printf.printf "\n\tComparing:";*)
(*    Set.iter (fun (v,b) -> Printf.printf "\n\t\t%s->\t" (symb2str v); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) b) r;*)
(*    Printf.printf "\n\twith:";*)
(*    Set.iter (fun (v,b) -> Printf.printf "\n\t\t%s->\t" (symb2str v); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) b) allResults;*)
(*    Printf.printf "\n";*)
(*    r = allResults*)
    compareTheseSets r allResults
    
  let testLookahead g r =
    let rep = (g#representation : t) in
    let allResults = 
      Set.flatMap (fun v -> 
        let rules = Set.filter (fun {head=h; _} -> h = v ) rep.rules in
        Set.map (fun r -> 
          (r, g#lookahead r)
        ) rules
      ) rep.variables 
    in
(*    Printf.printf "\n\tComparing:";*)
(*    Set.iter (fun ({head=h;body=b},r) -> Printf.printf "\n\t\t%s->%s\t" (symb2str h) (word2str b); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) r) r;*)
(*    Printf.printf "\n\twith:";*)
(*    Set.iter (fun ({head=h;body=b},r) -> Printf.printf "\n\t\t%s->%s\t" (symb2str h) (word2str b); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) r) allResults;*)
(*    Printf.printf "\n";*)
(*    r = allResults*)
    compareTheseSets r allResults

  let testFunction1 f r =
    f = r



(*	let test0 () =*)
(*		let m = new LL1Grammar.model (Arg.Text cfg_simple) in*)
(*		let j = m#toJSon in*)
(*			JSon.show j*)

	let dollar = '$'
	let epsilon = '~'

  let testExample1 () =
    Printf.printf "Example1 test: [";
    let first = [ ("S", ['a'; 'b'; 'c']); ("A", ['a'; '~']); ("B", ['b'; '~']); ("C", ['c']); ("D", ['d'; '~']); ("E", ['e'; '~']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', ['$']); ('A', ['b'; 'c']); ('B', ['c']); ('C', ['d'; 'e'; '$']); ('D', ['e'; '$']); ('E', ['$']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->ABCDE", ['a'; 'b'; 'c']); 
                      ("A->a", ['a']); ("A->", ['b'; 'c']);
                      ("B->b", ['b']); ("B->", ['c']); 
                      ("C->c", ['c']); 
                      ("D->d", ['d']); ("D->", ['e'; dollar]); 
                      ("E->e", ['e']); ("E->", [dollar]) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testExample2 () =
    Printf.printf "Example2 test: [";
    let first = [ ("S", ['a'; 'b'; 'c'; 'd']); ("B", ['a'; epsilon]); ("C", ['c'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('B', ['b']); ('C', ['d']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->Bb", ['a'; 'b']); ("S->Cd", ['c'; 'd']);
                      ("B->aB", ['a']); ("B->", ['b']);
                      ("C->cC", ['c']); ("C->", ['d']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testExample3 () =
    Printf.printf "Example3 test: [";
    let first = [ ("U", ['*'; epsilon]); ("D", ['+'; epsilon]); ("E", ['('; 'i']); ("F", ['('; 'i']); ("T", ['('; 'i']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('U', [')'; '+'; dollar]); ('D', [')'; dollar]); ('E', [')'; dollar]); ('F', [')'; '*'; '+'; dollar]); ('T', [')'; '+'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("U->*FU", ['*']); ("U->", [')'; '+'; dollar]); 
                      ("D->+TD", ['+']); ("D->", [')'; dollar]); 
                      ("E->TD", ['('; 'i']); 
                      ("F->i", ['i']); ("F->(E)", ['(']);
                      ("T->FU", ['('; 'i']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example3) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testExample4 () =
    Printf.printf "Example4 test: [";
    let first = [ ("S", ['a']); ("B", ['c']); ("C", ['b'; epsilon]); ("D", ['f'; 'g'; epsilon]); ("E", ['g'; epsilon]); ("F", ['f'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('B', ['f'; 'g'; 'h']); ('C', ['f'; 'g'; 'h']); ('D', ['h']); ('E', ['f'; 'h']); ('F', ['h']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->aBDh", ['a']);
                      ("B->cC", ['c']);
                      ("C->bC", ['b']); ("C->", ['f'; 'g'; 'h']); 
                      ("D->EF", ['f'; 'g'; 'h']);
                      ("E->g", ['g']); ("E->", ['f'; 'h']);
                      ("F->f", ['f']); ("F->", ['h']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example4) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testExample5 () =
    Printf.printf "Example5 test: [";
    let first = [ ("E", ['n']); ("A", ['n'; epsilon]); ("B", ['*'; '+']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('E', ['*'; '+'; dollar]); ('A', ['*'; '+'; dollar]); ('B', ['*'; '+'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("E->nA", ['n']);
                      ("A->EB", ['n']); ("A->", ['*'; '+'; dollar]);
                      ("B->+A", ['+']); ("B->*A", ['*']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example5) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testExample6 () =
    Printf.printf "Example6 test: [";
    let first = [ ("N", ['a'; 'b']); ("A", ['a'; 'b']); ("B", ['a'; 'b']); ("C", ['a'; 'b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('N', [dollar]); ('A', ['a'; 'b'; dollar]); ('B', ['a'; 'b'; dollar]); ('C', ['a'; 'b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("N->AB", ['a'; 'b']); ("N->BA", ['a'; 'b']);
                      ("A->a", ['a']); ("A->CAC", ['a'; 'b']);
                      ("B->b", ['b']); ("B->CBC", ['a'; 'b']); 
                      ("C->a", ['a']); ("C->b", ['b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example6) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"

  let testDissertation () =
    Printf.printf "%s" (Printf.sprintf "Dissertation test: [");
    let first = [ ("S", ['a']); ("A", ['a']); ("B", ['c']); ("C", ['c']); ("D", ['a'; epsilon]); ("E", ['b'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['c']); ('B', ['c']); ('C', ['b'; 'c'; dollar]); ('D', ['c']); ('E', ['c']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->ABC", ['a']); 
                      ("A->aD", ['a']); 
                      ("B->CE", ['c']);
                      ("C->c", ['c']); 
                      ("D->AB", ['a']); ("D->", ['c']);
                      ("E->bE", ['b']); ("E->", ['c']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text cfg_dissertation) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testNFGrammar () =
    Printf.printf "Non deterministic grammar test: [";
    let first = [ ("S", ['a'; 'b'; 'd'; 'g'; 'h'; epsilon]); ("A", ['d'; 'g'; 'h'; epsilon]); ("B", ['g'; epsilon]); ("C", ['h'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['g'; 'h'; dollar]); ('B', ['a'; 'g'; 'h'; dollar]); ('C', ['b'; 'g'; 'h'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->ACB", ['d'; 'g'; 'h'; dollar]); ("S->CbB", ['b'; 'h']); ("S->Ba", ['a'; 'g']);
                      ("A->da", ['d']); ("A->BC", ['g'; 'h'; dollar]); 
                      ("B->g", ['g']); ("B->", ['a'; 'g'; 'h'; dollar]); 
                      ("C->h", ['h']); ("C->", ['b'; 'g'; 'h'; dollar]); ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text non_deterministic_grammar) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"

  let testAccessible1 () =
    Printf.printf "%s" (Printf.sprintf "Remove inaccessible symbols test 1: [");
    let first = [ ("A", ['a'; 'b']); ("B", ['a'; 'b']); ("C", ['a']); ("D", ['a']); ("E", ['a']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', [dollar]); ('B', ['a'; 'b']); ('C', ['a'; 'b']); ('D', ['a'; 'b']); ('E', ['a'; 'b']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("A->aBb", ['a']); ("A->bBa", ['b']); 
                      ("B->Cb", ['a']); ("B->bC", ['b']);
                      ("C->a", ['a']); ("C->aC", ['a']); 
                      ("D->E", ['a']); ("D->Db", ['a']);
                      ("E->aE", ['a']); ("E->Da", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text accessible_symbols1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 m#isFullyAccessible false);
    let transformed = new LL1Grammar.model (Arg.Representation m#accessibleRewrite#representation) in
(*      printRepresentation m#representation;*)
(*      printRepresentation transformed#representation;*)
      printResult (testFunction1 transformed#isFullyAccessible true);
    Printf.printf "]\n"
    
  let testAccessible2 () =
    Printf.printf "%s" (Printf.sprintf "Remove inaccessible symbols test 2: [");
    let first = [ ("S", ['a']); ("B", ['b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('B', []) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->a", ['a']); 
                      ("B->b", ['b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text accessible_symbols2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive false);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isFullyAccessible false);
      printResult (testFunction1 m#isFullyProductive true);
    let transformed = new LL1Grammar.model (Arg.Representation m#accessibleRewrite#representation) in
      printResult (testFunction1 transformed#isFullyAccessible true);
      printResult (testFunction1 transformed#isFullyProductive true);
    Printf.printf "]\n"

  let testProductive1 () =
    Printf.printf "%s" (Printf.sprintf "Remove unproductive symbols test 1: [");
    let first = [ ("A", ['a'; 'b']); ("B", ['a'; 'b']); ("C", ['a']); ("D", ['a']); ("E", ['a']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', ['a'; 'b'; dollar]); ('B', ['a'; 'b']); ('C', ['a'; 'b']); ('D', ['a'; 'b']); ('E', ['a'; 'b']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("A->aBb", ['a']); ("A->bBa", ['b']); 
                      ("B->CD", ['a']); ("B->aC", ['a']); ("B->Ab", ['a'; 'b']);
                      ("C->a", ['a']); ("C->aC", ['a']); 
                      ("D->E", ['a']); ("D->DA", ['a']);
                      ("E->aE", ['a']); ("E->Da", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text productive_symbols1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring true);
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 m#isFullyProductive false);
    let transformed = new LL1Grammar.model (Arg.Representation m#productiveRewrite#representation) in
      printResult (testFunction1 transformed#isFullyProductive true);
    Printf.printf "]\n"

  let testProductive2 () =
    Printf.printf "%s" (Printf.sprintf "Remove unproductive symbols test 2: [");
    let first = [ ("S", ['a']); ("A", []); ("B", ['b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['b'; dollar]); ('B', ['b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->a", ['a']); ("S->A", []); 
                      ("A->AB", []);
                      ("B->b", ['b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text productive_symbols2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isFullyAccessible true);
      printResult (testFunction1 m#isFullyProductive false);
    let transformed = new LL1Grammar.model (Arg.Representation m#productiveRewrite#representation) in
      printResult (testFunction1 transformed#isFullyAccessible false);
      printResult (testFunction1 transformed#isFullyProductive true);
    let fullyTransformed =  new LL1Grammar.model (Arg.Representation (List.nth m#clean 1).grammar#representation) in
      printResult (testFunction1 fullyTransformed#isFullyAccessible true);
      printResult (testFunction1 fullyTransformed#isFullyProductive true);
    Printf.printf "]\n"

  let cleanGrammar1 () = 
    Printf.printf "%s" (Printf.sprintf "Clean grammar test 1: [");
    let first = [ ("S", ['a'; 'b']); ("A", ['a'; 'b']); ("B", ['a'; 'b'; 'c']); ("C", ['a'; 'b'; 'c']); ("D", ['d'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', ['a'; 'b'; dollar]); ('A', ['a'; 'b'; 'c'; dollar]); ('B', ['a'; 'b'; 'c'; dollar]); ('C', ['a']); ('D', ['a']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->aSa", ['a']); ("S->bB", ['b']); ("S->bAA", ['b']);
                      ("A->a", ['a']); ("A->SbA", ['a'; 'b']); ("A->aB", ['a']);
                      ("B->AB", ['a'; 'b']); ("B->CaB", ['a'; 'b'; 'c']);
                      ("C->cC", ['c']); ("C->Sa", ['a'; 'b']); ("C->bD", ['b']);
                      ("D->dD", ['d']); ("D->", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text clean_grammar1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive false);
      printResult (testFunction1 m#isLeftFactoring true);
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 m#isFullyAccessible true);
      printResult (testFunction1 m#isFullyProductive false);
    let fullyTransformed =  new LL1Grammar.model (Arg.Representation (List.nth m#clean 1).grammar#representation) in
      printResult (testFunction1 fullyTransformed#isFullyAccessible true);
      printResult (testFunction1 fullyTransformed#isFullyProductive true);
    Printf.printf "]\n"
    
  let testDirectRecursion1 () =
    Printf.printf "Direct recursion test 1: [";
    let first = [ ("A", ['b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', ['a'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("A->Aa", ['b']); ("A->b", ['b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text direct_left_recursion_grammar1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeLeftRecursion.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
    Printf.printf "]\n"

  let testDirectRecursion2 () =
    Printf.printf "Direct recursion test 2: [";
    let first = [ ("B", ['a']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('B', ['b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("B->a", ['a']); ("B->Bb", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text direct_left_recursion_grammar2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeLeftRecursion.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
    Printf.printf "]\n"

  let testIndirectRecursion1 () =
    Printf.printf "Indirect recursion test 1: [";
    let first = [ ("A", ['a']); ("S", ['a']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', [dollar]); ('S', [dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->A", ['a']); ("S->a", ['a']); ("A->S", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text indirect_left_recursion_grammar1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are unit productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
      printResult (testFunction1 transformed#isClean false);
    (*Grammar can be cleaned yet.*)
    let transformed = new LL1Grammar.model (Arg.Representation (List.nth transformed#clean 1).grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
      printResult (testFunction1 transformed#isClean true);
    Printf.printf "]\n"
    
  let testIndirectRecursion2 () = (*FIXME Trying to clean grammar right away fails*)
    Printf.printf "Indirect recursion test 2: [";
    let first = [ ("S", ['a']); ("A", ['a']); ("B", ['a']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', [dollar]); ('B', [dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->A", ['a']); ("S->B", ['a']);
                      ("A->a", ['a']);
                      ("B->S", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text indirect_left_recursion_grammar2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are epsilon productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive true);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are unit productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation transformed#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
    (*Grammar can be cleaned yet.*)
      printResult (testFunction1 transformed#isClean false);
    let transformed = new LL1Grammar.model (Arg.Representation (List.nth transformed#clean 1).grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
      printResult (testFunction1 transformed#isClean true);
    Printf.printf "]\n"

  let testIndirectRecursion3 () =
    Printf.printf "Indirect recursion test 3: [";
    let first = [ ("S", ['a'; 'b'; 'c']); ("A", ['a'; epsilon]); ("B", ['b'; epsilon]); ("C", ['c'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a'; 'b'; 'c']); ('B', ['a'; 'b'; 'c']); ('C', ['a'; 'b'; 'c']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->ABCS", ['a'; 'b'; 'c']);
                      ("A->a", ['a']); ("A->", ['a'; 'b'; 'c']); 
                      ("B->b", ['b']); ("B->", ['a'; 'b'; 'c']); 
                      ("C->c", ['c']); ("C->", ['a'; 'b'; 'c']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text indirect_left_recursion_grammar3) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are epsilon productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive true);
      printResult (testFunction1 transformed#isLeftFactoring true);
      printResult (testFunction1 transformed#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are unit productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation transformed#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring true);
      printResult (testFunction1 transformed#isLL1 false);
    let transformed = new LL1Grammar.model (Arg.Representation transformed#leftFactoring.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 false); (*This grammar is not LL(1)*)
    Printf.printf "]\n"
    
  let testIndirectRecursion4 () =
    Printf.printf "Indirect recursion test 4: [";
    let first = [ ("A", ['b'; 'e'; 'g']); ("B", ['b'; 'e'; 'g']); ("C", ['b'; 'e'; 'g']); ("D", ['b'; 'e'; 'g']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', ['a'; dollar]); ('B', ['a']); ('C', ['d'; 'g']); ('D', ['f']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("A->Ba", ['b'; 'e'; 'g']); ("A->b", ['b']);
                      ("B->Cd", ['b'; 'e'; 'g']); ("B->e", ['e']);
                      ("C->Df", ['b'; 'e'; 'g']); ("C->g", ['g']);
                      ("D->Df", ['b'; 'e'; 'g']); ("D->Aa", ['b'; 'e'; 'g']); ("D->Cg", ['b'; 'e'; 'g']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text indirect_left_recursion_grammar4) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeLeftRecursion.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring true);
      printResult (testFunction1 transformed#isLL1 false);
    let transformed = new LL1Grammar.model (Arg.Representation transformed#leftFactoring.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 false);
    Printf.printf "]\n"

  let testIndirectRecursion5 () =
    Printf.printf "Indirect recursion test 5: [";
    let first = [ ("S", ['a']); ("A", ['a'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->AS", ['a']);
                      ("A->a", ['a']); ("A->", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text indirect_left_recursion_grammar5) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are epsilon productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive true);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are unit productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation transformed#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
    Printf.printf "]\n"

  let testLeftFactoring1 () =
    Printf.printf "Left factoring test 1: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 false);
    Printf.printf "]\n"
    
  let testLeftFactoring2 () =
    Printf.printf "Left factoring test 2: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example2) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 false);
    Printf.printf "]\n"
    
  let testLeftFactoring3 () =
    Printf.printf "Left factoring test 3: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example3) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 false);
    Printf.printf "]\n"
    
  let testLeftFactoring4 () =
    Printf.printf "Left factoring test 4: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example4) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 false);
    Printf.printf "]\n"
    
  let testLeftFactoring5 () =
    Printf.printf "Left factoring test 5: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example5) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 true);
    Printf.printf "]\n"
    
  let testLeftFactoring6 () =
    Printf.printf "Left factoring test 6: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example6) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 true);
    Printf.printf "]\n"

  let testUnitRemoval1 () =
    Printf.printf "Unit production removal test 1: [";
    let first = [ ("E", ['a'; 'b']); ("T", ['a'; 'b']); ("F", ['a'; 'b']); ("I", ['a'; 'b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('E', [dollar]); ('T', [dollar]); ('F', [dollar]); ('I', ['a'; 'b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("E->T", ['a'; 'b']);
                      ("T->F", ['a'; 'b']); 
                      ("F->I", ['a'; 'b']);
                      ("I->a", ['a']); ("I->b", ['b']); ("I->Ia", ['a'; 'b']); ("I->Ib", ['a'; 'b'])] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text unit_removal_example1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasUnitProductions true);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasUnitProductions false);
    Printf.printf "]\n"

  let testUnitRemoval2 () =
    Printf.printf "Unit production removal test 2: [";
    let first = [ ("A", ['a'; 'b'; 'c']); ("B", ['a'; 'b'; 'c']); ("C", ['a'; 'b'; 'c']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', [dollar]); ('B', [dollar]); ('C', [dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("A->B", ['a'; 'b'; 'c']); ("A->a", ['a']);
                      ("B->C", ['a'; 'b'; 'c']); ("B->b", ['b']);
                      ("C->A", ['a'; 'b'; 'c']); ("C->c", ['c'])] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text unit_removal_example2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasUnitProductions true);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasUnitProductions false);
    Printf.printf "]\n"

  let testUnitRemoval3 () =
    Printf.printf "Unit production removal test 3: [";
    let first = [ ("S", ['a'; 'b'; 'c']); ("A", ['a'; 'b']); ("B", ['a'; 'b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a'; dollar]); ('B', ['a'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->Aa", ['a'; 'b']); ("S->B", ['a'; 'b']); ("S->c", ['c']);
                      ("A->a", ['a']); ("A->bc", ['b']); ("A->B", ['a'; 'b']); 
                      ("B->A", ['a'; 'b';]); ("B->bb", ['b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text unit_removal_example3) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasUnitProductions true);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasUnitProductions false);
    Printf.printf "]\n"

  let testUnitRemoval4 () =
    Printf.printf "Unit production removal test 4: [";
    let first = [ ("S", ['a']); ("A", ['a']); ("B", ['b']); ("C", ['b'; 'd']); ("D", ['b']); ("E", ['b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['b'; 'd']); ('B', [dollar]); ('C', [dollar]); ('D', [dollar]); ('E', [dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->AC", ['a']);
                      ("A->a", ['a']); 
                      ("B->D", ['b']);
                      ("C->B", ['b']); ("C->d", ['d']);
                      ("D->E", ['b']);
                      ("E->b", ['b'])] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text unit_removal_example4) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasUnitProductions true);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasUnitProductions false);
    Printf.printf "]\n"
    
  let testUnitRemoval5 () =
    Printf.printf "Unit production removal test 5: [";
    let first = [ ("E", ['a'; 'b'; '(']); ("T", ['a'; 'b'; '(']); ("F", ['a'; 'b'; '(']); ("I", ['a'; 'b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('E', [')'; '+'; dollar]); ('T', [')'; '*'; '+'; dollar]); ('F', [')'; '*'; '+'; dollar]); ('I', ['0'; 'a'; '1'; 'b'; ')'; '*'; '+'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("E->T", ['a'; 'b'; '(']); ("E->E+T", ['a'; 'b'; '(']);
                      ("T->F", ['a'; 'b'; '(']); ("T->T*F", ['a'; 'b'; '(']); 
                      ("F->I", ['a'; 'b']); ("F->(E)", ['(']);
                      ("I->a", ['a']); ("I->b", ['b']); ("I->Ia", ['a'; 'b']); ("I->Ib", ['a'; 'b']); ("I->I0", ['a'; 'b']); ("I->I1", ['a'; 'b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text unit_removal_example5) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasUnitProductions true);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasUnitProductions false);
    Printf.printf "]\n"

  let testEmptyRemoval1 () =
    Printf.printf "Empty production removal test 1: [";
    let first = [ ("S", ['a'; 'b'; 'd']); ("A", ['b'; 'd'; epsilon]); ("B", ['b'; epsilon]); ("C", ['d'; epsilon]); ("D", ['d']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a'; 'b']); ('B', ['a'; 'b'; 'd']); ('C', ['a'; 'b'; dollar]); ('D', ['a'; 'b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->ABaC", ['a'; 'b'; 'd']);
                      ("A->BC", ['a'; 'b'; 'd']); 
                      ("B->b", ['b']); ("B->", ['a'; 'b'; 'd']);
                      ("C->D", ['d']); ("C->", ['a'; 'b'; dollar]); 
                      ("D->d", ['d']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text epsilon_removal_example1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasEmptyProductions true); 
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasEmptyProductions false); 
    Printf.printf "]\n"
    
  let testEmptyRemoval2 () =
    Printf.printf "Empty production removal test 2: [";
    let first = [ ("S", ['a'; 'b'; epsilon]); ("A", ['a'; epsilon]); ("B", ['b'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a'; 'b'; dollar]); ('B', ['b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->AB", ['a'; 'b'; dollar]);
                      ("A->AaA", ['a']); ("A->", ['a'; 'b'; dollar]); 
                      ("B->BbB", ['b']); ("B->", ['b'; dollar]) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text epsilon_removal_example2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasEmptyProductions true); 
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasEmptyProductions false); 
    Printf.printf "]\n"
    
  let testEmptyRemoval3 () =
    Printf.printf "Empty production removal test 3: [";
    let first = [ ("S", ['a'; 'b'; epsilon]); ("A", ['a'; epsilon]); ("B", ['b'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a'; 'b'; dollar]); ('B', ['b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->AB", ['a'; 'b'; dollar]);
                      ("A->aAA", ['a']); ("A->", ['a'; 'b'; dollar]); 
                      ("B->bBB", ['b']); ("B->", ['b'; dollar]) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text epsilon_removal_example3) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasEmptyProductions true); 
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasEmptyProductions false);
    Printf.printf "]\n"
    ;;
    
	let runAll =
		if Util.testing active "LL1Grammar" then begin
			testExample1 ();
			testExample2 ();
			testExample3 ();
			testExample4 ();
			testExample5 ();
			testExample6 ();
			testDissertation ();
			testNFGrammar ();
			testAccessible1 ();
			testAccessible2 ();
			testProductive1 ();
			testProductive2 ();
			cleanGrammar1 ();
			testDirectRecursion1 ();
			testDirectRecursion2 ();
			testIndirectRecursion1 ();
			testIndirectRecursion2 ();
			testIndirectRecursion3 ();
			testIndirectRecursion4 ();
			testIndirectRecursion5 ();
			testLeftFactoring1 ();
			testLeftFactoring2 ();
			testLeftFactoring3 ();
			testLeftFactoring4 ();
			testLeftFactoring5 ();
			testLeftFactoring6 ();
			testUnitRemoval1 ();
			testUnitRemoval2 ();
			testUnitRemoval3 ();
			testUnitRemoval4 ();
			testUnitRemoval5 ();
			testEmptyRemoval1 ();
			testEmptyRemoval2 ();
			testEmptyRemoval3 ()
		end
end

#endif
