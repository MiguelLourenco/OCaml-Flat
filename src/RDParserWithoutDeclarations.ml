module RDParserWithoutDeclarations =
struct

  class virtual parser =
    object(self) inherit RDParser.parser as super
    
      method build (rep:ContextFreeGrammar.t) =
        self#setupIncludes ^
        self#setupVariables ^
        self#getCharFunction ^
        self#errorFunction ^
        self#matchFunction ^
        (self#printFunctions rep.variables rep) ^
        (self#mainFunction (String.make 1 rep.initial))
    
  end
end
