open BasicTypes
open CFGSyntax
open ContextFreeGrammar

module type LRGrammarSig =
sig
	type t = ContextFreeGrammar.t
	type tx = ContextFreeGrammar.tx
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation: t
				method representationx : tx
				method validate : unit

				method tracing: unit
				method isRegular: bool
				method accept: word -> bool
				method first: word -> symbol Set.t
			  method follow: symbol -> symbol Set.t
			  method lookahead: CFGSyntax.rule -> symbol Set.t

				method acceptWithTracing: word -> unit
				method generate: int -> words

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
								
			(* LR parsing *)
				method startRules : rules

			end
end

module LRGrammar: LRGrammarSig  =
struct
	type t = ContextFreeGrammar.t
	type tx = ContextFreeGrammar.tx
	
	(* only a random example *)
	let startRules (cfg: t): rules =
		let initial = cfg.initial in
		let rules = cfg.rules in
			Set.filter (fun {head=h; body=_} -> h = initial) rules
 
 
	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit ContextFreeGrammar.model arg as super

	(* only a random example *)
			method startRules : rules =
				startRules (self#representation)
	end
end


module LRGrammarTests: sig end =
struct
	let active = false

	let test0 () =
		let m = new LRGrammar.model (Arg.Predef "cfg_simple") in
		let j = m#toJSon in
			JSon.show j

	let test1 () =
		let m = new LRGrammar.model (Arg.Predef "cfg_simple") in
		let r = m#startRules in
			Util.println ["*** Rules for the start symbol:"];
			CFGSyntax.show r

	let runAll =
		if Util.testing active "LRGrammar" then begin
			test0 ();
			test1 ()	
		end
end


