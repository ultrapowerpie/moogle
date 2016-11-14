(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) =
struct
  open Order
  type elt = C.t
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs =
    match xs with
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs =
    match xs with
      | [] -> [x]
      | y::ys -> (match C.compare x y with
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs =
    match xs with
      | [] -> []
      | x::xs1 -> (match C.compare y x with
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys =
    match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x =
    match xs with
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs =
    match xs with
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left (fun a x -> f x a) e

  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string =
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  module D = Dict.Make(
    struct
      type key = C.t
      type value = bool
      let compare = C.compare
      let string_of_key = C.string_of_t
      let string_of_value v = ""

      (* Use these functions for testing. See TESTING EXPLANATION. *)

      (* Generate a key. The same key is always returned *)
      let gen_key () = C.gen ()

      (* Generate a random key. *)
      let gen_key_random () = C.gen_random ()

      (* Generates a key greater than the argument. *)
      let gen_key_gt (k:key) () = C.gen_gt k ()

      (* Generates a key less than the argument. *)
      let gen_key_lt (k:key) () = C.gen_lt k ()

      (* Generates a key between the two arguments. Return None if no such
       * key exists. *)
      let gen_key_between (small:key) (big:key) () =
        C.gen_between small big ()

      (* Generates a random value. *)
      let gen_value () = true

      (* Generates a random (key,value) pair *)
      let gen_pair () = (gen_key_random (), true)
  end)

  type elt = D.key
  type set = D.dict
  let empty = D.empty

  let is_empty (s:set) : bool =
    match D.choose s with
      Some v -> false
    | None -> true

  let insert (e:elt) (s:set) : set = D.insert s e true

  (* same as insert x empty *)
  let singleton (e:elt) : set = D.insert D.empty e true

  let union (s1:set) (s2:set) : set =
    D.fold (fun k v s -> D.insert s k true) s1 s2

  let intersect (s1:set) (s2:set) : set =
    D.fold (fun k v s -> if (D.member s k) then D.insert s k true else s) s1 s2

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  let remove (e:elt) (s:set) : set = D.remove s e

  (* returns true iff the element is in the set *)
  let member (s:set) (e:elt) : bool = D.member s e

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  let choose (s:set) : (elt * set) option =
    match D.choose s with
      None -> None
    | Some (k,v,s) -> Some (k,s)

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  let rec fold (f:elt -> 'a -> 'a) (u:'a) (s:set) : 'a =
    match D.choose s with
      None -> u
    | Some (k,v,s) -> fold f (f k u) s

  (* functions to convert our types to a string. useful for debugging. *)
  let string_of_set (s:set) : string = D.string_of_dict s
  let string_of_elt (e:elt) : string = D.string_of_key e

  (* TESTING FUNCTIONS IMPLEMENTATION *)

  (* adds a list of elts to a set in left-to-right order *)
  let insert_list (s: set) (lst: elt list) : set =
    List.fold_left (fun s e -> insert e s) s lst

  (* adds a list of elts to a set in right-to-left order *)
  let insert_list_reversed (s: set) (lst: elt list) : set =
    List.fold_right (fun e s -> insert e s) lst s

  (* generates an elt list with n distinct keys in increasing order *)
  let generate_elt_list (size: int) : elt list =
    let rec helper (size: int) (current: elt) : elt list =
      if size <= 0 then []
      else
        let new_current = (C.gen_gt current ()) in
        new_current :: (helper (size - 1) new_current)
    in
    helper size (C.gen ())

  (* generates an elt list with keys in random order *)
  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_choose () =
    let rec check_elt_set (s:set) : unit =
      match choose s with
        None -> assert(is_empty s) ;
      | Some (e,s1) -> assert(member s e);
                       assert(not (member s1 e));
                       check_elt_set s1
    in
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    check_elt_set s1 ;
    ()

  let test_member () =
    test_insert();
    test_remove();
    ()

  let test_fold () =
    let s0 = empty in
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    assert((fold (fun k i -> 1) 0 s0) = 0);
    assert ((fold (fun k i -> i+1) 0 s1) = 100);
    ()

  let test_union () =
    let elts1 = generate_random_list 100 in
    let s1 = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2 = insert_list empty elts2 in
    let s12 = insert_list s1 elts2 in
    assert((union s1 s2) = s12);
    ()

  let test_intersect () =
    let elts1 = generate_random_list 100 in
    let s1 = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2 = insert_list empty elts2 in
    let list_set = List.fold_left
      (fun s e -> if List.mem e elts1 then insert e s else s) empty elts2 in
    assert((intersect s1 s2) = list_set);
    ()

  let test_is_empty () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    assert(is_empty empty);
    assert(not (is_empty s1));
    ()

  let test_singleton () =
    let e = C.gen_random() in
    let s1 = singleton e in
    match choose s1 with
      None -> assert(false);
    | Some (e, s0) -> assert(is_empty s0);
    assert(member s1 e) ;
    ()


  (* runs our tests. See TESTING EXPLANATION *)
  let run_tests () =
    test_insert() ;
    test_remove() ;
    test_choose() ;
    test_member() ;
    test_fold() ;
    test_union();
    test_intersect();
    test_is_empty();
    test_singleton();
    ()

  (* implement the rest of the functions in the signature! *)

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  (* add your test functions to run_tests *)
  let run_tests () =
    ()
end



(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;

(* Create a set of ints using our DictSet functor
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)
(*
module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;
*)


(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use our dictionary implementation when your are
   * finished. *)
  (* ListSet (C) *)
  DictSet (C)
