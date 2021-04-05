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
 * mar/2021 (amd, rm) - Initial version
 *)

(*
 * Description: Some interface functions to use this library in the context
 *    of Learn-OCaml. Conversion of OCamlFlat exercises to the Learn-OCaml
 *    format (with generation of an exercise directory). 
 *)

module type LearnOCamlSig =
sig
	val setOCamlFlatDir : string -> unit
	val setLearnOCamlDir : string -> unit
	val setLearnOCamlTarget : string -> unit
	val generateUnitTests : Exercise.exercise -> (string * bool * string * string) list
    val checkProperties : Model.model -> Exercise.exercise -> (bool * string) list
	val generateExerciseDir : JSon.t -> JSon.t -> unit
end

module LearnOCaml :LearnOCamlSig  =
struct
	(* ----- Dir/File management ----- *)
	let oCamlFlatDir = ref ""
	let learnOCamlDir = ref ""
	let learnOCamlTarget = ref ""

	let setOCamlFlatDir dirname =
		oCamlFlatDir := dirname

	let setLearnOCamlDir dirname =
		learnOCamlDir := dirname

	let setLearnOCamlTarget filename =
		learnOCamlTarget := filename

	let initialize () =
		if !oCamlFlatDir = "" then
			let homeDir = Sys.getenv("HOME") in (* learn-ocaml requires lazy init *)
			begin
				setOCamlFlatDir
					(homeDir ^ "/work/OCamlFlat");
				setLearnOCamlDir
					(homeDir ^ "/work/learn-test/my-learn-ocaml-repository/exercises");
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
		Util.stripHead txt 2
 
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

	let generateUnitTests (ex: Exercise.exercise) =
		let open Exercise in
		let rep = ex#representation in
		let inside = Util.words2strings (Set.toList rep.inside) in
		let outside = Util.words2strings (Set.toList rep.outside) in
			List.map (fun w -> (w, true, "", "")) inside
			@
			List.map (fun w -> (w, false, "", "")) outside
	
	let checkProperty m p =
		if m#checkProperty p then (true, p)
		else (false, p)
	
	let checkProperties (m: Model.model) (ex: Exercise.exercise) =
		let open Exercise in
		let rep = ex#representation in
		let props = Set.toList rep.properties in
			List.map (checkProperty m) props

	(* ----- FILE descr.html ----- *)
	let fileName =
		"descr.html"
	
	let contents exercise =
		Printf.sprintf
	{zzz|
		<h3> %s </h3>
		<p> %s </p>
	|zzz} (JSon.field_string exercise "description")
		(JSon.field_string exercise "problem")
	
	let generateFile_Descr exercise =
		let text = contents exercise in
			createTargetFile fileName text

	(* ----- FILE meta.json ----- *)
	let fileName =
		"meta.json"
		
	let contents exercise =
		Printf.sprintf
	{zzz|
		{ "learnocaml_version" : "1",
		  "kind"               : "exercise",
		  "stars"              : 0,
		  "title"              : "%s"
		}
	|zzz} (JSon.field_string exercise "description")
	
	let generateFile_Meta exercise =
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

	let generateFile_Prelude () =
		let text = contents in
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

	let contents solution =
		Printf.sprintf
	{zzz|
		let solution = {|%s|}

		let model =
			PolyModel.text2model solution

		let accept_sol x = 
			model#accept (Util.str2word x)		
	|zzz} (JSon.to_string_n 2 solution)
	
	let generateFile_Solution solution =
		let text = contents solution in
			createTargetFile fileName text

	(* ----- FILE template.ml ----- *)
	let fileName =
		"template.ml"

	let contents =
	{zzz|
		let solution = {|
			"write solution here"
		|}

		let model =
			PolyModel.text2model solution

		let accept_sol x = 
			model#accept (Util.str2word x)		
	|zzz}

	let generateFile_Template () =
		let text = contents in
			createTargetFile fileName text

	
	(* ----- FILE test.ml ----- *)
	let fileName =
		"test.ml"

	let contents exercise =
		Printf.sprintf
	{zzz|
		open Test_lib
		open Report

		let exercise = {|%s|}
		
		let convertPropertyResult (b, p) =
			if b then [
				Message ([Text ("Property " ^ p ^ " passed")], Success 1)
			]
			else [
				Message ([Text "Wrong value"], Failure);
				Message ([Text ("Property " ^ p ^ " failed")], Informative)
			]
			
		let exer =
			new Exercise.exercise (Arg.Text exercise)

		let handleUnitTests () =
			test_function_1
				[%%ty: string -> bool]
				"accept_sol"
				(LearnOCaml.generateUnitTests exer)
				
		let handleProperties () =
			test_variable_property
				[%%ty: Model.model]
				"model"
				(fun m ->
					let results = LearnOCaml.checkProperties m exer in
						Util.flatMap convertPropertyResult results
				)

		let () =
		  set_result @@
		  ast_sanity_check code_ast @@
		  fun () -> (handleUnitTests() @ handleProperties())  
	
	|zzz} (JSon.to_string_n 2 exercise) ;;

	let generateFile_Test exercise =
		let text = contents exercise in
			createTargetFile fileName text	
	
	let generateExerciseDir exercise solution =
		initialize ();
		createTargetDir ();
		generateFile_Descr exercise;
		generateFile_Meta exercise;
		generateFile_Prelude ();
		generateFile_Prepare ();
		generateFile_Solution solution;
		generateFile_Template ();
		generateFile_Test exercise

end

module LearnOCamlTests =
struct
	let active = false

	let test0 () =
		let exercise = Examples.jsonExample "exer_astar" in
		let solution = Examples.jsonExample "dfa_astar" in
			LearnOCaml.generateExerciseDir exercise solution
			
	(* LearnOCamlTests.test0 ();; *)

	let runAll =
		if active then (
			Util.header "LearnOCamlTests";
			test0 ()
		)
end

(*
Handy reference:

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
