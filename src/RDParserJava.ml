module RDParserJava =
struct

include RDParser

  class parser =
    object(self) inherit RDParserWithoutDeclarations.parser as super

    val equality = "=="
    val orOp = "||"
    val functionArgsOpen = "("
    val functionArgsClose = ")"
    val ifOpen = "("
    val ifClose = ")"
    val ifElseGuardOpen = "{"
    val ifElseGuardClose = "}"
    val expressionTermination = ";"
    val return = "return"

    method setupIncludes = ""
    
    method setupVariables =
  	  Printf.sprintf {|
		static char[] %s;
		static char %s;
		static int %s;
      |} arrayVar currentCharVar currentIndexVar
 

    method getCharFunction =
      Printf.sprintf {|
		static char %s() {
			return %s[%s++];
		}
      |} (getCharFun) (arrayVar) (currentIndexVar)
    
    method matchFunction =
      Printf.sprintf {|
		static void %s(char t) {
			if(%s == t)
				%s = %s();
			else %s();
		}
      |} (matchCharFun) (currentCharVar) (currentCharVar) (getCharFun) (parseErrorFun)
    
    
    method errorFunction = 
      Printf.sprintf {|
		static void %s() {
			System.out.println("Error parsing! %s = " + %s);
			System.exit(1);
		}
      |} (parseErrorFun) (currentCharVar) (currentCharVar)


    method mainFunction initialVar = 
      Printf.sprintf {|
		public static void main(String[] args) {
			%s = 0;
			%s = (args[0].toString()+'$').toCharArray(); //Add terminal symbol
			%s = %s();
			%s();
			if(%s == '$')
				System.out.println("Parsing OK!");
			else %s();
		}
    |} currentIndexVar arrayVar (currentCharVar) (getCharFun) (initialVar) (currentCharVar) (parseErrorFun)


    method createFun name contents =
      Printf.sprintf {|
		static void %s () {
			%s
		}
      |} name contents

	method build (rep:ContextFreeGrammar.t) =
		"public class Main {" ^
			super#build rep ^
		"}"

  end
end
