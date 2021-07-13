(*
 * LearnOCaml.ml
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
 *  Written by Artur Miguel Dias, Rita Macedo (amd, rm)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Added semantic validation of the student's solution.
 * jun/2021 (amd) - Added support for typed student answers in the file
 *                  "template.ml". Managed to get rid of all the technical
 *                  in the file "template.ml". Many improvements in the
 *                  implementation.
 * mar/2021 (amd, rm) - Initial version
 *)

(*
 * Description: This module helps in the use of the OCamlFLAT library
 * inside Learn-OCaml (the platform for learning the OCaml language). The goal
 * is to support FLAT related exercises inside Learn-OCaml. 
 *  
 * As for FLAT exercises developed for current text-based interaction of
 * Learn-OCaml, our current approach is to avoid changing Learn-OCaml
 * itself.
 * 
 * The current solution comprehends:
 *
 * - A technique that allows the OCamlFlat library to be available inside the
 *   exercise environment of Learn-OCaml (via the file "prepare.ml").
 *   
 * - The idea of reducing FLAT model analysis to OCaml function analysis. This
 *   allows us to represent FLAT exercises using what is available in
 *   Learn-OCaml, without changing anything in Learn-OCaml.
 *   
 * - This module, which supplies some functions that helps in
 *   the creation by hand of FLAT exercises following the conventions
 *   of Learn-OCaml.
 *   
 * - A translator from the OCamlFLAT exercise format to the Learn-OCaml
 *   exercise format. The translator generates a directory containing all
 *   the usual files: "template.ml", "solution.ml", "meta.json", etc.
 *)

module type LearnOCamlSig =
sig
	val setOCamlFlatDir : string -> unit
	val setLearnOCamlDir : string -> unit
	val setLearnOCamlTarget : string -> unit
	val processAnswer : Model.model
					-> Exercise.exercise -> (string * int) list
	val decl2json : string -> JSon.t
	val generateExerciseDir : JSon.t -> JSon.t -> bool -> unit
end

module LearnOCaml : LearnOCamlSig =
struct
	(* ----- Dir/File management ----- *)
	let oCamlFlatDir = ref ""
	let learnOCamlDir = ref ""
	let learnOCamlTarget = ref ""		

	let setOCamlFlatDir dirname =
		oCamlFlatDir := Util.handleHomeDir dirname

	let setLearnOCamlDir dirname =
		learnOCamlDir := Util.handleHomeDir dirname

	let setLearnOCamlTarget filename =
		learnOCamlTarget := filename

	let initialize () =
		if !oCamlFlatDir = "" then
			begin
				setOCamlFlatDir
					"~/work/OCamlFlat";
				setLearnOCamlDir
					"~/work/learn-test/my-learn-ocaml-repository/exercises";
				setLearnOCamlTarget
					"default"
			end

	let libFile () =
		!oCamlFlatDir ^ "/lib/OCamlFlat.ml"

	let targetDir () =
		!learnOCamlDir ^ "/" ^ !learnOCamlTarget
	
	let targetFile fileName =
		targetDir () ^ "/" ^ fileName
	
	let adjust txt =
		Util.stripHead txt
 
	let createTargetDir () =
		ignore (Sys.command ("mkdir -p " ^ targetDir ()) )

	let createTargetFile fileName text =
		let co = open_out (targetFile fileName) in
		let text = adjust text in
		begin
			output_string co text;
			close_out co
		end
	
	(* ----- Utility functions ----- *)
	let processUnitTest m expected w =
			(Util.word2str w, expected, m#accept w = expected)

	let semanticValidation (m: Model.model) =
		m#errors

	let convertSemanticValidationResult mesg =
			(mesg, 0)

	let processUnitTests (m: Model.model) (e: Exercise.exercise) =
		let open Exercise in
		let rep = e#representation in
			List.map (processUnitTest m true) (Set.toList rep.inside)
			@
			List.map (processUnitTest m false) (Set.toList rep.outside)
			
	let convertUnitTestResult (word, acceptance, passed) =
		let ar = if acceptance then "Acceptance" else "Rejection" in
		let pf = if passed then "passed" else "failed" in
		let points = if passed then 1 else 0 in
			(ar ^ " of word \"" ^ word ^ "\" " ^ pf, points)

	let processProperty m p =
		(p, m#checkProperty p)
	
	let processProperties (m: Model.model) (e: Exercise.exercise) =
		let open Exercise in
		let rep = e#representation in
		let props = Set.toList rep.properties in
			List.map (processProperty m) props	
		
	let convertPropertyResult (property, passed) =
		let pf = if passed then "passed" else "failed" in
		let points = if passed then 3 else 0 in
			("Property \"" ^ property ^ "\" " ^ pf, points)

	let finalResult0 res =
		[("###  Checking for semantic errors...", -999)]
		@ res
		@ [("### Errors found", -999)]

	let finalResult1 res1 res2 =
		[]
		@ [("###  Checking for semantic errors...", -999)]
		@ [("### Checking unit tests...", -999)]
		@ res1
		@ [("### Checking properties...", -999)]
		@ res2
		@ [("### Done", -999)]

	let processAnswer (m: Model.model) (e: Exercise.exercise) =
		let semanticValidationResults = semanticValidation m in
		let res0 = List.map convertSemanticValidationResult semanticValidationResults in
		if res0 <> [] then
			finalResult0 res0
		else
			let unitTestsResults = processUnitTests m e in
			let res1 = List.map convertUnitTestResult unitTestsResults in
			let propertyResults = processProperties m e in
			let res2 = List.map convertPropertyResult propertyResults in
				finalResult1 res1 res2

	let kind2designation kind =
		match kind with
		| "FiniteAutomaton" -> FiniteAutomaton.modelDesignation
		| "RegularExpression" -> RegularExpression.modelDesignation
		| "ContextFreeGrammar" -> ContextFreeGrammar.modelDesignation
		| "FiniteEnumeration" -> FiniteEnumeration.modelDesignation
		| _ -> kind

	let completeJSon kind j  =
		match kind with
		| "RegularExpression" ->
			JSon.JAssoc [("re", j)]
		| "FiniteEnumeration" ->
			JSon.JAssoc [("words", j)]
		| _ -> j

	let decl2json s =
		Util.show s;
		try
			let a = String.index_from s 0 ':' in
			let b = String.index_from s a '.' in
			let c = String.index_from s b '=' in
			let kind = String.trim (String.sub s (a+1) (b-a-1)) in
			let ocamlExp = String.sub s (c+1) (String.length s -c-1) in
			let jExp = JSon.parseOon ocamlExp in
			JSon.show jExp;
			let mainJSon = completeJSon kind jExp in
			let jHead = Entity.toJSon (Entity.dummyId (kind2designation kind)) in
				JSon.append jHead mainJSon
		with _ ->
			JSon.JNull
	
	(* ----- FILE descr.html ----- *)
	let fileName =
		"descr.html"
	
	let contents (exercise: JSon.t) =
		Printf.sprintf
	{zzz|
		<h3> %s </h3>
		<p> %s </p>
	|zzz}
		(JSon.fieldString exercise "description")
		(JSon.fieldString exercise "problem")
	
	let generateFile_Descr (exercise: JSon.t) =
		let text = contents exercise in
			createTargetFile fileName text

	(* ----- FILE meta.json ----- *)
	let fileName =
		"meta.json"
		
	let contents (exercise: JSon.t) =
		Printf.sprintf
	{zzz|
		{ "learnocaml_version" : "1",
		  "kind"               : "exercise",
		  "stars"              : 0,
		  "title"              : "%s"
		}
	|zzz}
		(JSon.fieldString exercise "description")
	
	let generateFile_Meta (exercise: JSon.t) =
		let text = contents exercise in
			createTargetFile fileName text

	(* ----- FILE prelude.ml ----- *)
	let fileName =
		"prelude.ml"

	let contents =
	{zzz|
		(* Some code is loaded in the toplevel before your code. *)
		let greetings = "Hello world!"
	|zzz}

	let generateFile_Prelude_old () =
		let text = contents in
			createTargetFile fileName text

	let generateFile_Prelude (solution: Model.model) =
		let text = solution#xTypeDeclString in
			createTargetFile fileName text
	
	(* ----- FILE prepare.ml ----- *)
	let fileName =
		"prepare.ml"

	let generateFile_Prepare () =
		let cmd = "cp -a " ^ libFile () ^ " " ^ targetFile fileName in
			ignore (Sys.command cmd)

	(* ----- FILE solution.ml ----- *)
	let fileName =
		"solution.ml"

	let contentsJSon (solution: Model.model) =
		Printf.sprintf
	{zzz|
		let solution = {| %s |}
	|zzz}
		(JSon.toStringN 2 solution#toJSon)

	let contents (solution: Model.model) =
		solution#toDisplayString "solution"
	
	let generateFile_Solution (solution: Model.model) useJSon =
		let text =
			if useJSon then contentsJSon solution
			else contents solution
		in
			createTargetFile fileName text

	(* ----- FILE template.ml ----- *)
	let fileName =
		"template.ml"

	let contentsJSon (solution: Model.model) =
		Printf.sprintf
	{zzz|
		(* Write your solution below, by using the provided example as a template *)

		let solution = {| %s |}
	|zzz}
		(JSon.toStringN 2 solution#example)

	let contents (solution: Model.model) =
		Printf.sprintf
	{zzz|
		(* Write your solution below, by using the provided example as a template *)
		%s|zzz}
		((PolyModel.json2model solution#example)#toDisplayString "solution")

	let generateFile_Template (solution: Model.model) useJSon =
		let text =
			if useJSon then contentsJSon solution
						else contents solution
		in
			createTargetFile fileName text

	
	(* ----- FILE test.ml ----- *)
	let fileName =
		"test.ml"
	
	let exercisePart (exercise: JSon.t) =
		Printf.sprintf
	{zzz|
		let exercise = {| %s |}
	|zzz}
		(JSon.toStringN 2 exercise)

	let handleAnswerPartJSon =
		Printf.sprintf
	{zzz|
		let handleAnswer (): Learnocaml_report.t =
			test_variable_property
				[%%ty: string]
				"solution"
				(fun solution ->
					checkAnswer
						(PolyModel.text2model solution)
						(new Exercise.exercise (Arg.Text exercise))
				)
	|zzz}
	
	let handleAnswerPart (solution: Model.model) =
		Printf.sprintf
	{zzz|
		let handleAnswer (): Learnocaml_report.t =
			test_variable_property
				[%%ty: %s]
				"solution"
				(fun solution ->
					checkAnswer
						((new %s.model (Arg.RepresentationX solution)):
										%s.model :> Model.model)
						(new Exercise.exercise (Arg.Text exercise))
				)
	|zzz}
	solution#xTypeName
	solution#moduleName
	solution#moduleName
		
	let contents exerciseText handleAnswerText =
		Printf.sprintf
	{zzz|
		open Test_lib
		open Report
		%s
		let convertResult (diagnostic, points) =
			match points with
			| _ when points > 0 ->
				Message ([Text diagnostic], Success points)
			| -999 ->
				Message ([Break; Text diagnostic], Informative)
			| _ ->
				Message ([Text diagnostic], Failure)
		
		let checkAnswer (m: Model.model) (e: Exercise.exercise) =
			let res = LearnOCaml.processAnswer m e in
				List.map convertResult res
		%s
		let () =
			set_result @@
			ast_sanity_check code_ast @@
			handleAnswer
	|zzz}
		exerciseText
		handleAnswerText

	let generateFile_Test (exercise: JSon.t) (solution: Model.model) useJSon =
		let ex = exercisePart exercise in
		let hs =
			if useJSon then handleAnswerPartJSon
			else handleAnswerPart solution
		in
		let text = contents ex hs
		in
			createTargetFile fileName text	

	(* ----- generateExerciseDir ----- *)
	
	let generateExerciseDir exercise solution useJSon =
		let solution: Model.model = PolyModel.json2model solution in
			initialize ();
			createTargetDir ();
			generateFile_Descr exercise;
			generateFile_Meta exercise;
			generateFile_Prelude solution;
			generateFile_Prepare ();
			generateFile_Solution solution useJSon;
			generateFile_Template solution useJSon;
			generateFile_Test exercise solution useJSon
end


module LearnOCamlTests =
struct
	let active = false

	let prepare () =
		LearnOCaml.setOCamlFlatDir "~/work/OCamlFlat";
		LearnOCaml.setLearnOCamlDir "~/work/OCamlFlat/lo";
		LearnOCaml.setLearnOCamlTarget "default"
	
	let test0 () =
		prepare();
		let exercise = Examples.jsonExample "exer_astar" in
		let solution = Examples.jsonExample "dfa_astar" in
			LearnOCaml.generateExerciseDir exercise solution false
			
	let test1 () =
		prepare();
		let exercise = Examples.jsonExample "exer_astar" in
		let solution = Examples.jsonExample "re_astar" in
			LearnOCaml.generateExerciseDir exercise solution false
	
	let fe_colors = {| {
		kind : "finite enumeration",
		description : "this is an example",
		name : "colors",
		words : ["Red", "Yellow", "Blue"]
	} |}

	let test2 () =
		prepare();
		let exercise = Examples.jsonExample "exer_balanced" in
		let solution = Examples.jsonExample "cfg_balanced" in
			LearnOCaml.generateExerciseDir exercise solution false
			
	let test3 () =
		prepare();
		let exercise = Examples.jsonExample "exer_astar" in
		let solution = JSon.parse fe_colors in
			LearnOCaml.generateExerciseDir exercise solution false

	let decl = {|
		let solution: FiniteAutomaton.tx =
		{
			alphabet = ['a'];
			states = ["START"];
			initialState = "START";
			transitions = [("START", 'a', "START")];
			acceptStates = ["START"]
		} |}
		
	let decl2 = {|
		let solution: RegularExpression.tx =
			"z*"
	|}
		
	let decl3 = {|
		let solution: ContextFreeGrammar.tx =
		{
			alphabet = ['0'; '1'];
			variables = ['S'; 'P'];
			initial = 'S';
			rules = [	"S -> 1S0 | P";
						"P -> 0P1 | ~" ]
		}
	|}
		
	let decl4 = {|
		let solution: FiniteEnumeration.tx =
			["A"; "B"; "C"; "D"; "E"]
	|}

	let test4 () =
		let j = LearnOCaml.decl2json decl3 in
			JSon.show j
	
	let runAll =
		if active then (
			Util.header "LearnOCamlTests";
			test0 ()
		)
end

(*
Learnocaml_report.t:

	type report = item list

	and item =
	  | Section of text * report
	  (** A titled block that groups subreports *)
	  | Message of text * status
	  (** Basic report block *)

	and status =
	  | Success of int (** With given points *)
	  | Failure (** With missed points *)
	  | Warning (** A student error without influence on the grade *)
	  | Informative (** A message for the student *)
	  | Important (** An important message *)

	and text = inline list

	and inline =
	  | Text of string (** A word *)
	  | Break (** Line separator *)
	  | Code of string (** For expressions *)
	  | Output of string (** For output *)
*)
