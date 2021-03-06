open String ;;
open Util ;;
open CrawlerServices ;;
open Order ;;
open Pagerank ;;


module MoogleRanker
  (* = InDegreeRanker (PageGraph) (PageScore) *)

     = RandomWalkRanker (PageGraph) (PageScore) (struct
       let do_random_jumps = Some 0.20
       let num_steps = 10000
     end)


(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s =
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 *
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =

  let rec insert_dict (wl:string list) (l:link) (d:WordDict.dict)
      : WordDict.dict =
    match wl with
      [] -> d
    | hd::tl ->
      let hd = String.lowercase hd in
      (* print hd; *)
      match WordDict.lookup d hd with
        None -> insert_dict tl l (WordDict.insert d hd (LinkSet.singleton l))
      | Some s -> insert_dict tl l (WordDict.insert d hd (LinkSet.insert l s))
  in
  (* if size of the visited set = n, maximum # of links have been reached *)
  match LinkSet.fold (fun x i -> i+1) 0 visited == n with
    true -> d
  | false ->
  (* take a link from the frontier *)
    match LinkSet.choose frontier with
    | None -> d
    | Some (l, frontier) ->
    (* check if the link was visited *)
      match LinkSet.member visited l with
      | true -> crawl n frontier visited d
      | false ->
      (* add the link to the visited set *)
        match CrawlerServices.get_page l with
        | None -> crawl n frontier visited d
        | Some p ->
        (* add the links on the new page to the frontier *)
          let visited = LinkSet.insert l visited in
          let frontier = List.fold_left (fun s e -> LinkSet.insert e s)
            frontier p.links in
          crawl n frontier visited (insert_dict p.words l d)
;;

let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
