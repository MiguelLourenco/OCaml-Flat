module RDParserNeedFunDeclaration =
struct

  class virtual parser =
    object(self) inherit RDParserWithDeclarations.parser as super
    
      (***Method that prints function declarations*)
      method functionDeclarations vars =
        let rec printDecl vars =
          if vars = Set.empty then ""
          else let (x,xs) = Set.cut vars in
            Printf.sprintf {|void %c();|} x ^ "\n" ^ printDecl xs
          in
      printDecl vars ^ "\n"
    
      method build (rep:ContextFreeGrammar.t) =
        Util.stripHead (self#setupIncludes ^
        self#setupVariables ^
        self#functionDeclarations rep.variables ^
        self#getCharFunction ^
        self#errorFunction ^
        self#matchFunction ^
        (self#printFunctions rep.variables rep) ^
        (self#mainFunction (String.make 1 rep.initial)))
    
  end

end
