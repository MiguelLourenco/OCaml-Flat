module RDParserOCaml =
struct

include RDParser

  class parser =
    object(self) inherit RDParserNeedRecursiveFunDeclaration.parser as super

    val equality = "="
    val orOp = "||"
    val functionArgsOpen = "("
    val functionArgsClose = ")"
    val ifOpen = "("
    val ifClose = ")"
    val ifElseGuardOpen = "("
    val ifElseGuardClose = ")"
    val expressionTermination = ";"
    val return = "()"
    
    method setupIncludes = ""


    method setupVariables = 
      Printf.sprintf {|
        let %s = ref [||]
        let %s = ref 'l'
        let %s = ref 0
      |} arrayVar currentCharVar currentIndexVar
      
    
    method getCharFunction = 
      Printf.sprintf {|
        let %s() =
          if ( !%s < (Array.length !%s) - 1 )
          then %s := !%s + 1;
          %s := !%s.(!%s)
              |} (getCharFun)
                (currentIndexVar) (arrayVar)
                (currentIndexVar) (currentIndexVar)
                currentCharVar (arrayVar) (currentIndexVar)
        
        
    method createFunCalls funs (rep:ContextFreeGrammar.t) =
      match funs with
      | [] -> []
      | x::xs when Set.belongs x rep.alphabet -> [matchCharFun ^ functionArgsOpen ^ "\'" ^ String.make 1 x ^ "\'" ^ functionArgsClose ^ expressionTermination] @ self#createFunCalls xs rep
      | x::xs when Set.belongs x rep.variables -> [String.lowercase_ascii (String.make 1 x) ^ functionArgsOpen ^ functionArgsClose ^ expressionTermination] @ self#createFunCalls xs rep
      | x::xs -> []


    method matchFunction = 
      Printf.sprintf {|
		let %s t =
			if (!%s = t)
			then (%s())
			else (%s())
      |} matchCharFun currentCharVar getCharFun parseErrorFun

      
    method errorFunction = 
      Printf.sprintf {|
		let %s() =
			Printf.printf "Error parsing! %s = %%c\n" !%s;
			exit 1
      |} parseErrorFun currentCharVar currentCharVar


    method createIfConds conditions =
      let p = String.make 1 '\'' in
      match conditions with
      | [] -> ""
      | [x] -> "!" ^ currentCharVar ^ equality ^ p ^ (String.make 1 x) ^ p
      | x::xs -> "!" ^ currentCharVar ^ equality ^ p ^ (String.make 1 x) ^ p ^ " " ^ orOp ^ " " ^ self#createIfConds xs
      

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
        | (c,e)::xs -> tab ^ (if first then "if" else "else if") ^ ifOpen ^ c ^ ifClose ^ " then " ^ ifElseGuardOpen ^ "\n" ^ 
                       (createExpr e (tabLevel+1)) ^
                       tab ^ (ifElseGuardClose) ^ "\n" ^ createIf2 false xs tabLevel
      in
      createIf2 true ifList tabLevel
    
    
    method mainFunction i = 
      Printf.sprintf {|
		let explode s = List.init (String.length s) (String.get s)

		let main () =
			%s := Array.of_list (explode Sys.argv.(1));
			%s := Array.append (!%s) ([|'$'|]);
			%s := !%s.(0);
			%s();
			if !%s = '$'
			then Printf.printf "Parsing OK!"
			else %s();
			exit 0

		let _ = main()
	  |} arrayVar arrayVar arrayVar
                  currentCharVar arrayVar
                  (String.lowercase_ascii i)
                  currentCharVar
                  parseErrorFun

    method createFun name contents =
      (*Does not need let, inherited class deals with it*)
      Printf.sprintf {|
		%s () =
			%s
      |} (String.lowercase_ascii name) contents

  end
end

