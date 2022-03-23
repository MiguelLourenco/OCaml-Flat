module RDParserC =
struct

include RDParser

  class parser =
    object(self) inherit RDParserNeedFunDeclaration.parser as super

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


    method setupIncludes =
  	  Printf.sprintf {|
		#include <stdio.h>
		#include <stdlib.h>
		#include <string.h>
      |}
    
    
    method setupVariables =
      Printf.sprintf {|
		char* %s;
		char %s;
		int %s;
		|} arrayVar currentCharVar currentIndexVar
    
    
    method getCharFunction =
      Printf.sprintf {|
		char %s(){
			return %s[%s++];
		}
      |} (getCharFun) (arrayVar) (currentIndexVar)
    
    
    method matchFunction =
      Printf.sprintf {|
		void %s(char t) {
			if(%s == t) {
				%s = %s();
			}
			else {
				%s();
			}
		}
      |} (matchCharFun) (currentCharVar) (currentCharVar) (getCharFun) (parseErrorFun)
    
    
    method errorFunction = 
      Printf.sprintf {|
		void %s() {
			printf("Error parsing! %s = %%c\n", %s);
			exit(1);
		}
      |} (parseErrorFun) (currentCharVar) (currentCharVar)
    
      
    method mainFunction initialVar = 
      Printf.sprintf {|
		int main(int argc, char* argv[]){
		  char termChar = '$'; 
		  char* tmp = strcat( argv[1], &termChar);
			%s = tmp;
			%s = %s();
			%s();
			if(%s == '$'){
				printf("Parsing OK!\n");
			} else {
				%s();
			}
			return 0;
		}
    |} (arrayVar) (currentCharVar) (getCharFun) (initialVar) (currentCharVar) (parseErrorFun)
    

    method createFun name contents =
      Printf.sprintf {|
        void %s() {
        %s
        }
      |} name contents

  end
end
