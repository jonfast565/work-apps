(*
 * File Manager 
 *)

(* Splits up a file's path *)
let split_file_path filepath =
  let dir = Filename.dirname filepath in
  let base = Filename.basename filepath in
  let ext = Filename.extension filepath in
  (dir, base, ext)

(* Create a channel and use it to read a file *)
let read_file filename = Core.In_channel.read_all filename

(* read multiple files at once *)
let read_files paths = List.map read_file paths

(* Create a channel and use it to write a file *)
let write_file filename content =
  Core.Out_channel.write_all filename ~data:content

let file_test path = Sys.file_exists path && Sys.is_regular_file path
let folder_test path = Sys.is_directory path

(* walks the directory tree for files *)
let rec walk_files path ~filter =
  Logs.info (fun m -> m "Walking files in: %s" path);
  try
    let paths =
      Sys.readdir path
      |> Array.map (fun x -> Filename.concat path x)
      |> Array.to_list
    in
    let directories = List.filter (fun p -> folder_test p && filter p) paths in
    let files = List.filter (fun p -> file_test p && filter p) paths
    and internal_files = List.map (fun p -> walk_files p ~filter) directories in
    List.append files (List.flatten internal_files)
  with Sys_error e ->
    Logs.err (fun m -> m "%s (%s)" e path);
    []

(* creates a function for a file extension filter *)
let file_extension_filter (extension : string) path =
  if Sys.is_directory path then true
  else
    let _, _, extn = split_file_path path in
    extn = extension

(* creates a function for an excluded folders filter *)
let excluded_folders_filter folder_parts path =
  if not (Sys.is_directory path) then true
  else
    let dir, _, _ = split_file_path path in
    let splitted_dir =
      Utilities.split_by_delimiters Utilities.path_splitters dir
    in
    not (Utilities.contains_any folder_parts splitted_dir)

(* get the filters to be applied and turns them into one function *)
let get_concatenator_filters extension folder_parts =
  let filter =
    [ file_extension_filter extension; excluded_folders_filter folder_parts ]
  in
  fun path -> List.for_all (fun y -> y) (List.map (fun x -> x path) filter)
