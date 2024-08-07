(*
  Command Line
*)

open Cmdliner

type arguments = {
  path : string;
  excluded_folders : string list;
  extension : string;
  output_folder : string;
  output_filename : string;
}

let path =
  let doc = "Path to search for files (any path in Windows or Unix world)" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)

let excluded_folders =
  let doc = "Folders to exclude from search (optional, comma separated)" in
  Arg.(value & opt_all string [] & info ["e"; "exclude"] ~docv:"FOLDER" ~doc)

let extension =
  let doc = "File extension to search for (include a `.` at the beginning of the filename)" in
  Arg.(required & opt (some string) None & info ["x"; "extension"] ~docv:"EXT" ~doc)

let output_folder =
  let doc = "Output folder for results (not including the filename)" in
  Arg.(required & opt (some string) None & info ["o"; "output"] ~docv:"FOLDER" ~doc)

let output_filename =
  let doc = "Output filename (including extension)" in
  Arg.(required & opt (some string) None & info ["f"; "filename"] ~docv:"FILE" ~doc)

let args_t =
  Term.(const (fun path excluded_folders extension output_folder output_filename ->
    { path; excluded_folders; extension; output_folder; output_filename })
    $ path
    $ excluded_folders
    $ extension
    $ output_folder
    $ output_filename)

let info =
  let doc = "Search for files and process them" in
  let man = [
    `S Manpage.s_description;
    `P "This program searches for files with a specific extension, \
        excluding specified folders, and processes them.";
  ] in
  Cmd.info "concatenator" ~version:"1.0" ~doc ~man
  
let run_with_args run_args =
  exit (Cmd.eval (Cmd.v info (Term.(const run_args $ args_t))))


let get_path args = args.path
let get_excluded_folders args = args.excluded_folders
let get_extension args = args.extension
let get_output_folder args = args.output_folder
let get_output_filename args = args.output_filename

let print_args args =
  Logs.info (fun m -> m "Path: %s" (get_path args));
  Logs.info (fun m -> m "Extension: %s" (get_extension args));
  Logs.info (fun m ->
      m "Excluded Folders: %s"
        (String.concat ", " (get_excluded_folders args)));
  Logs.info (fun m -> m "Output Folder: %s" (get_output_folder args));
  Logs.info (fun m -> m "Output Filename: %s" (get_output_filename args))
