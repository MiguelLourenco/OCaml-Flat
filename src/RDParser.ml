module RDParser =
struct

  (* given a head, returns the set of all its bodies according to the cfg's rules *)
	let bodiesOfHead h rl =
	  let open CFGSyntax in
		let rls = Set.filter (fun r -> r.head = h) rl in
			Set.map (fun r -> r.body) rls

  let rec tabCreator tabLevel =
    if tabLevel > 0 then "\t"^ tabCreator (tabLevel - 1) else ""

  class virtual parser =
    object(self)

    val arrayVar = "word"
    val currentCharVar = "currentChar"
    val currentIndexVar = "wordIndex"
      
    val getCharFun = "getChar"
    val parseErrorFun = "parseError"
    val matchCharFun = "matchChar"

    val virtual equality : string
    val virtual orOp : string
    val virtual functionArgsOpen : string
    val virtual functionArgsClose : string
    val virtual ifOpen : string 
    val virtual ifClose : string
    val virtual ifElseGuardOpen : string
    val virtual ifElseGuardClose : string
    val virtual expressionTermination : string
    val virtual return : string

    (**Method that will print includes/imports needed for the parser**)
    method virtual setupIncludes : string
    
    
    (**Method used to setup the variables for the parser.**)
  	(**Use values arrayVar, currentCharVar and currentIndexVar**)
  	(**to get correct variable names.**)
    method virtual setupVariables : string
    
    
    (**Method that prints the getCharFunction.**)
    (**This function consumes a symbol while parsing.**)
    method virtual getCharFunction : string
    
    
    (**Method that prints the match function.**)
    (**This function verifies if symbols match.**)
    (**Consumes the symbol if there is a match.**)
    (**Otherwise, calls error function.**)
    method virtual matchFunction : string
    
    
    (**Method that prints the error function.**)
    (**This function exists the program when called symbols match.**)
    method virtual errorFunction : string
    

    method printFunctions vars rep =
      if vars = Set.empty then ""
      else let (x,xs) = Set.cut vars in
        self#symbolFunction x rep ^
        (self#printFunctions xs rep)
 
    (**Method that prints the programs main function.**)
    method virtual mainFunction : string -> string
    

    method virtual createFun : string -> string -> string


    (*TODO Move to lower level*)
    method createFunCalls funs (rep:ContextFreeGrammar.t) =
      match funs with
      | [] -> []
      | x::xs when Set.belongs x rep.alphabet -> [matchCharFun ^ functionArgsOpen ^ "\'" ^ String.make 1 x ^ "\'" ^ functionArgsClose ^ expressionTermination] @ self#createFunCalls xs rep
      | x::xs when Set.belongs x rep.variables -> [String.make 1 x ^ functionArgsOpen ^ functionArgsClose ^ expressionTermination] @ self#createFunCalls xs rep
      | x::xs -> []

    (*TODO Move to lower level*)
    method createIfConds conditions =
      let p = String.make 1 '\'' in
      match conditions with
      | [] -> ""
      | [x] -> currentCharVar ^ equality ^ p ^ (String.make 1 x) ^ p
      | x::xs -> currentCharVar ^ equality ^ p ^ (String.make 1 x) ^ p ^ " " ^ orOp ^ " " ^ self#createIfConds xs
      
    (*TODO Move to lower level*)
    method createIf ifList tabLevel = 
      let rec createIf2 first ifList tabLevel =
        let rec createExpr exprList tabLevel =
          match exprList with 
          | [] -> ""
          | x::xs -> (tabCreator tabLevel) ^ x ^ "\n" ^ createExpr xs tabLevel
        in
        let tab = (tabCreator tabLevel) in
        match ifList with
        | [] -> ""
        | [(c,e)] -> tab ^ "else" ^ ifElseGuardOpen ^ "\n" ^ (createExpr e (tabLevel+1)) ^ tab ^ ifElseGuardClose
        | (c,e)::xs -> tab ^ (if first then "if" else "else if") ^ ifOpen ^ c ^ ifClose ^ ifElseGuardOpen ^ "\n" ^ 
                       (createExpr e (tabLevel+1)) ^
                       tab ^ (ifElseGuardClose) ^ "\n" ^ createIf2 false xs tabLevel
      in
      createIf2 true ifList tabLevel

  
    method symbolFunction s (rep:ContextFreeGrammar.t) =
    
      let rec getNextTerminals ruleBody = 
        match ruleBody with
        | [] -> Set.empty
        | x::xs -> if Set.belongs x rep.alphabet
                   then Set.make [x]
                   else if Set.belongs x rep.variables 
                        then Set.flatMap (fun rb -> getNextTerminals rb) (bodiesOfHead x rep.rules)
                        else Set.empty
      in

      let rules = bodiesOfHead s rep.rules in
      let emptyRules = Set.filter (fun r -> List.length r = 0) rules in
      let nEmptyRules = Set.filter (fun r -> List.length r <> 0) rules in
      let nEmptyRules2 = (List.map (fun r -> self#createFunCalls r rep) (Set.toList nEmptyRules)) in
      let nEmptyTerminalSet = Set.map (getNextTerminals) nEmptyRules in
(*      let equalityStrings = Set.toList (Set.map (fun t -> currentCharVar() ^ equality() ^ (String.make 1 t)) nEmptyTerminalSet) in*)
      let mergedMap = List.map2 (fun a b -> (self#createIfConds (Set.toList a),b)) (Set.toList nEmptyTerminalSet) nEmptyRules2 in
      Printf.printf "Current Var: %c\n" s;
      List.iter (fun (a,b) -> Printf.printf "\t%s" a;
                              Printf.printf "\n";
                              List.iter (fun c -> Printf.printf "\t\t%s\n" c) b
      ) mergedMap;
      let mergedMap = mergedMap @ [("",[if Set.size emptyRules = 0 then (parseErrorFun ^ functionArgsOpen ^ functionArgsClose ^  expressionTermination) else (return ^ expressionTermination)])] in
      self#createFun (String.make 1 s) (self#createIf mergedMap 1)


    method virtual build : ContextFreeGrammar.t -> string

  end
end
