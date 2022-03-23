module RDParserNeedRecursiveFunDeclaration =
struct

  class virtual parser =
    object(self) inherit RDParserWithDeclarations.parser as super
    
      method printFunctions vars rep =
        let first = true in
        let rec printFunctionsX vars rep first =
          if vars = Set.empty then ""
          else let (x,xs) = Set.cut vars in
            (if first then "let rec " else "and ") ^
            self#symbolFunction x rep ^
            (printFunctionsX xs rep false)
        in
        printFunctionsX vars rep first
    
      method build (rep:ContextFreeGrammar.t) =
        Util.stripHead (self#setupIncludes ^
        self#setupVariables ^
        self#getCharFunction ^
        self#errorFunction ^
        self#matchFunction ^
        (self#printFunctions rep.variables rep) ^
        (self#mainFunction (String.make 1 rep.initial)))
    
  end

end
