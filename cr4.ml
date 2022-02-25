(* Varying levels of elegance in code *)

let working_negate_all (nums: int list) : int list = 
  List.map (fun x -> -x) nums ;;

let simple_negate_all (nums: int list) : int list = 
  List.map (~-) nums ;;

let best_negate_all: int list -> int list = 
  List.map (~-) ;;

(* A set contains multiple elements, but cannot contain any duplicates! *)
module type SET = 
  sig
    (* Custom errors for this set *)
    exception EmptyString
    (* Type of string sets *)
    type set
    (* An empty set *)
    val empty : set
    (* Returns true if set is empty, false otherwise *)
    val is_empty : set -> bool
    (* Adds string to existing set (if not already a member) *)
    val add : string -> set -> set
    (* Union of two sets *)
    val union : set -> set -> set
    (* Intersection of two sets *)
    val intersection : set -> set -> set
    (* Returns true iff string is in set *)
    val member: string -> set -> bool
  end;;


(* Implementation of STRING_SET as list of strings *)
module StringSet : SET =
  struct
    exception EmptyString

    type set = string list

    let empty = []

    let is_empty set = (set = [])

    let member = List.mem

    let add elt set =
      if elt = "" then raise EmptyString
      else if List.mem elt set then set
      else elt :: set

    let union = List.fold_right add

    let rec intersection set1 set2 =
      match set1 with
      | [] -> []
      | hd :: tl -> let tlint = intersection tl set2 in
      if member hd set2 then add hd tlint
      else tlint

    let count : set -> int = List.length
  end ;;

module type INT_QUEUE =
    sig
      (* queue      - The data type of our queue
      EmptyQueue    - a custom exception
      empty_queue   - a default “empty queue” constant
      enqueue       - takes an int and a queue and outputs a new queue with the int put last in the queue
      dequeue       - takes a queue and outputs a tuple of the 
                      last element of the queue and the queue with that last element removed *)
    end

module IntQueue : INT_QUEUE =
    struct 
      (* Implement the above structure! *)
    end;;




(* A set contains multiple elements, but cannot contain any duplicates! *)
module type POLYMORPHIC_SET = 
  sig
    (* Type of sets *)
    type 'a set
    (* An empty set *)
    val empty : 'a set
    (* Returns true if set is empty, false otherwise *)
    val is_empty : 'a set -> bool
    (* Adds string to existing set (if not already a member) *)
    val add : 'a -> 'a set -> 'a set
    (* Union of two sets *)
    val union : 'a set -> 'a set -> 'a set
    (* Intersection of two sets *)
    val intersection : 'a set -> 'a set -> 'a set
    (* Returns true iff element is in set *)
    val member: 'a -> 'a set -> bool
  end;;

(* Implementation of POLYMORPHIC_SET as a list*)
module PolymorphicSet : POLYMORPHIC_SET =
  struct
    type 'a set = 'a list
    
    let empty : 'a set = []
    
    let is_empty (set: 'a set) : bool = (set = empty)
    
    let add (elt: 'a) (set: 'a set) : 'a set =
      if List.mem elt set then set
      else elt :: set
    
    let union (set1: 'a set) (set2: 'a set) : 'a set = List.fold_right add set1 set2
    
    let member = List.mem

    let rec intersection set1 set2 =
      match set1 with
      | [] -> []
      | hd :: tl -> let tlint = intersection tl set2 in
      if member hd set2 then add hd tlint
      else tlint
  end ;;



(* Functors!!! *)
module type SPECIFIC_SET_PARAMTERS =
  sig
      type t
      val max_size : int
      val equal : t -> t -> bool
  end;;

module type SPECIFIC_SET = 
  sig
    (* Type of set element *)
    type element
    (* Type of set *)
    type set
    (* An empty set *)
    val empty : set
    (* Returns true if set is empty, false otherwise *)
    val is_empty : set -> bool
    (* Adds string to existing set (if not already a member) *)
    val add : element -> set -> set
    (* Union of two sets *)
    val union : set -> set -> set
    (* Intersection of two sets *)
    val intersection : set -> set -> set
    (* Returns true iff string is in set *)
    val member: element -> set -> bool
  end;;


module MakeSpecificSet (Params : SPECIFIC_SET_PARAMTERS) 
                      : (SPECIFIC_SET with type element = Params.t) =
  struct
    type element = Params.t
    type set = element list

    let empty = []

    let is_empty set = (set = [])

    let member = List.mem

    let count : set -> int = List.length

    let add elt set =
      if (count set >= Params.max_size) || (List.exists (Params.equal elt) set) then set
      else elt :: set

    let union = List.fold_right add

    let rec intersection set1 set2 =
      match set1 with
      | [] -> []
      | hd :: tl -> let tlint = intersection tl set2 in
      if member hd set2 then add hd tlint
      else tlint
  end ;;

(* Example usage *)
module MyModule (Input : SPECIFIC_SET_PARAMTERS) : SPECIFIC_SET =
  MakeSpecificSet(Input) ;;

module SampleInput : SPECIFIC_SET_PARAMTERS =
  struct
    type t = int
    let max_size = 5

    (* private helper function *)
    let modulo5 x =
      let result = x mod 5 in
      if result >= 0 then result
      else result + 5

    let equal = fun x y -> modulo5 x = modulo5 y
  end;;

module Mod5Module = MyModule(SampleInput);;


(* PROBLEM: RESTRICTED QUEUE *)

module type RESTRICTED_QUEUE_PARAMETERS =
  sig
    type t
    val max_elements: int
    val is_allowed: t -> bool 
  end

  (* 
    t - the type of the element in the queue
    max_elements - the maximum number of elements allowed in the queue
    is_allowed - takes in an element and returns whether or not it's allowed to be added to the queue
  *)

module RestrictedQueueParams = 
  struct
    (* Parameters that will generate a queue module that takes in integers, allows up to 10 values,
        and only allows integers that are positive  *)
  end

module type RESTRICTED_QUEUE =
  sig
    type element
    type queue
    exception EmptyQueue
    val empty_queue : queue
    val enqueue : element -> queue -> queue
    val dequeue : queue -> element * queue
  end

(* module CreateRestrictedQueue (Params: RESTRICTED_QUEUE_PARAMETERS) 
                            : (RESTRICTED_QUEUE with type element = Params.t) =
  struct 
    (* Implement the above structure! *)
  end;; *)