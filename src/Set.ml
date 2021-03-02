(*
 * Set.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Polymorphic sets. Naive implementation.
 *
 * TODO: Improve the implementation or move to the functorized sets of
 * the ocaml standard library.
 *)

module type SetSig =
sig
	(*type 'a t = 'a list*)
	type 'a t (* opaque *)
	val make : 'a list -> 'a t
	val toList : 'a t -> 'a list
	val empty : 'a t
	val size : 'a t -> int
	val belongs : 'a -> 'a t -> bool
	val union : 'a t -> 'a t -> 'a t
	val nth : 'a t -> int -> 'a
	val add : 'a -> 'a t -> 'a t
	val remove : 'a -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val subset : 'a t -> 'a t -> bool
	val map : ('a -> 'b) -> 'a t -> 'b t
	val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
	val filter : ('a -> bool) -> 'a t -> 'a t
	val for_all : ('a -> bool) -> 'a t -> bool
	val exists : ('a -> bool) -> 'a t -> bool
	val flatten : 'a t t -> 'a t
	val flatMap : ('a -> 'b t) -> 'a t -> 'b t
	val iter : ('a -> unit) -> 'a t -> unit
	val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
	val combinations : 'a t -> 'b t -> ('a * 'b) t
	val star : 'a list t -> int -> 'a list t
	val allDistinct : ('a -> 'b) -> 'a t -> bool
	val hasDuplicates : 'a t -> bool
	val validate : 'a list -> string -> 'a t
	val historicalFixedPoint : ('a t -> 'a t) -> ('a t) -> 'a t
	val historicalFixedPointTracing : ('a t -> 'a t) -> ('a t) -> 'a t list
	val test: unit -> int list list
end

module Set : SetSig =
struct
	type 'a t = 'a list
	let make (l: 'a list): 'a t = List.sort_uniq compare l
	let toList (s: 'a t): 'a list = s

	let empty: 'a t = []
	let size (s: 'a t): int = List.length s
	let belongs (v: 'a) (s: 'a t): bool = List.mem v s
	let union (s1: 'a t) (s2: 'a t): 'a t = make (s1 @ s2)
	let nth (s: 'a t) (n: int) : 'a = List.nth s n
	let add (v: 'a) (s: 'a t): 'a t = make (v :: s)
	let remove (v: 'a) (s: 'a t): 'a t = List.filter (fun x -> x <> v) s
	let inter (s1: 'a t) (s2: 'a t): 'a t = List.filter (fun x -> belongs x s2) s1
	let diff (s1: 'a t) (s2: 'a t): 'a t = List.filter (fun x -> not (belongs x s2)) s1
	let subset (s1: 'a t) (s2: 'a t): bool = List.for_all (fun x -> belongs x s2) s1

	let map f s = make (List.map f s)
	let mapi f s = make (List.mapi f s)
	let filter f s = List.filter f s
	let for_all f s = List.for_all f s
	let exists f s = List.exists f s
	let flatten ss = make (List.flatten ss)
	let flatMap f s = flatten (List.map f s)
	let iter f s = List.iter f s
	let partition f s = let (a, b) = List.partition f s in (a, b)	(* <- already ordered *)
	let combinations (s1: 'a t) (s2: 'b t): ('a * 'b) t =
		flatMap (fun x -> List.map (fun y -> (x,y)) s2) s1	(* <- already ordered *)
	let starOne (s: 'a list t) (n: int) (l:'a list): 'a list t = (* private auxiliary *)
		let z = n - (List.length l) in
		let sel = filter (fun k -> List.length k <= z) s in
			map (fun k -> k @ l) sel
	let star (s: 'a list t) (n: int): 'a list t =
		Util.fixedPoint (fun x -> union x (flatMap (starOne x n) s)) [[]]

	let allDistinct f s = size s = size (map f s)
	let hasDuplicates (s: 'a t): bool = size s <> size (make s)
	let validate (l: 'a list) (culprit: string): 'a t =
		if hasDuplicates l
			then Error.error culprit "Repetitions in set" empty
			else make l

	let historicalFixedPoint (f: 'a t -> 'a t) (x: 'a t): 'a t =
		let rec historicalFixedPointX (f: 'a t -> 'a t) (x: 'a t) (acum: 'a t): 'a t =
			let next = f x in
			let newAcum = union x acum in
			if acum = newAcum then x
			else historicalFixedPointX f next newAcum
		in
			historicalFixedPointX f x empty

	let historicalFixedPointTracing (f: 'a t -> 'a t) (x: 'a t): 'a t list =
		let rec historicalFixedPointX (f: 'a t -> 'a t) (x: 'a t) (acum: 'a t) (trace: 'a t list): 'a t list =
			let next = f x in
			let newTrace = trace@[next] in
			let newAcum = union x acum in
			if acum = newAcum then trace
			else historicalFixedPointX f next newAcum newTrace
		in
			historicalFixedPointX f x empty [x]

	let test (): int list list =	(* Set.test ();; *)
		toList (star (make[ [1]; [2;3]]) 4)
end

module type UPSetSig = (* unordered pair set *)
sig
	type 'a t
	val make : ('a * 'a) list -> 'a t
	val toList : 'a t -> ('a * 'a) list
	val empty : 'a t
	val size : 'a t -> int
	val belongs : 'a * 'a -> 'a t -> bool
	val union : 'a t -> 'a t -> 'a t
	val add : 'a * 'a -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val subset : 'a t -> 'a t -> bool
	val map : ('a * 'a -> 'b * 'b) -> 'a t -> 'b t
	val filter : ('a * 'a -> bool) -> 'a t -> ('a * 'a) Set.t
	val for_all : ('a * 'a -> bool) -> 'a t -> bool
	val exists : ('a * 'a -> bool) -> 'a t -> bool
	val flatten : 'a t t -> 'a t
	val flatMap : ('a -> 'b t) -> 'a t -> 'b t
	val iter : ('a * 'a -> unit) -> 'a t -> unit
	val partition : ('a * 'a -> bool) -> 'a t -> ('a * 'a) Set.t * ('a * 'a) Set.t
	val combinations : 'a t -> 'b t -> ('a * 'b) t
	val star : 'a list t -> int -> 'a list t
	val allDistinct : ('a * 'a -> 'b) -> 'a t -> bool
	val hasDuplicates : 'a t -> bool
	val validate : ('a * 'a) list -> string -> 'a t
	val test: unit -> (int * int) list
end

module UPSet : UPSetSig =
struct
	type 'a t = ('a*'a) Set.t

	(* invariant: a < b for all pairs (a,b) *)
	let ord (a,b) = if a < b then (a, b)		(* keep *)
					else if a > b then (b, a)	(* swap *)
					else failwith "UPSet.ord"	(* error *)

	let make (l: ('a*'a) list): 'a t =
		let l1 = List.filter (fun (a,b) -> a <> b) l in
		let l2 = List.map ord l1 in
			Set.make l2
	let toList (s: 'a t): ('a*'a) list = Set.toList s

	let empty: 'a t = Set.empty
	let size (s: 'a t): int = Set.size s
	let belongs (v: 'a*'a) (s: 'a t): bool = Set.belongs (ord v) s
	let union (s1: 'a t) (s2: 'a t): 'a t = Set.union s1 s2
	let add (v: 'a*'a) (s: 'a t): 'a t = Set.add (ord v) s
	let inter (s1: 'a t) (s2: 'a t): 'a t = Set.inter s1 s2
	let diff (s1: 'a t) (s2: 'a t): 'a t = Set.diff s1 s2
	let subset (s1: 'a t) (s2: 'a t): bool = Set.subset s1 s2

	let map f (s: 'a t) = make (Set.toList (Set.map f s))
	let filter f (s: 'a t) = Set.filter f s
	let for_all f (s: 'a t) = Set.for_all f s
	let exists f (s: 'a t) = Set.exists f s
	let flatten (ss: 'a t t) = failwith "UPSet.flatten"
	let flatMap f (s: 'a t) = failwith "UPSet.flatMap"
	let iter f (s: 'a t) = Set.iter f s
	let partition f (s: 'a t) = Set.partition f s
	let combinations (s1: 'a t) (s2: 'b t): ('a * 'b) t = failwith "UPSet.combinations"
	let star (s: 'a list t) (n: int): 'a list t = failwith "UPSet.star"

	let allDistinct f (s: 'a t) = Set.allDistinct f s
	let hasDuplicates (s: 'a t): bool = Set.hasDuplicates s
	let validate (l: ('a*'a) list) (culprit: string): 'a t = failwith "UPSet.validate"
	let test () =	(* UPSet.test ();; *)
		toList (make [(1,1);(1,2);(2,2);(3,2);(3,2);(2,3)])
end
