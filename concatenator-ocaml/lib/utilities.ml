(*
  Utilities
*)

(* path splitters *)
let path_splitters = [ '\\'; '/' ]

(* Function to split a string by multiple delimiters *)
let split_by_delimiters delimiters str =
  let rec split_aux s acc =
    try
      let delimiter = List.find (fun d -> String.contains s d) delimiters in
      let idx = String.index s delimiter in
      let before_delim = String.sub s 0 idx in
      let after_delim = String.sub s (idx + 1) (String.length s - idx - 1) in
      split_aux after_delim (acc @ [ before_delim ])
    with Not_found -> acc @ [ s ]
  in
  split_aux str []

(* list contains any *)
let contains_any list1 list2 = List.exists (fun x -> List.mem x list2) list1
