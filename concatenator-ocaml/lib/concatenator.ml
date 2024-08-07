(* concat routine *)
let concat_files path extension exclude_folders =
  let default_filters =
    File_manager.get_concatenator_filters extension exclude_folders
  in
  let files = File_manager.walk_files path ~filter:default_filters in
  let read_files = File_manager.read_files files in
  String.concat "\n" read_files
