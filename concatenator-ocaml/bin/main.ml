open Concatenator_lib

let concat_with_args args =
  Command_line.print_args args;
  let path = Command_line.get_path args
  and extension = Command_line.get_extension args
  and excluded_folders = Command_line.get_excluded_folders args in
  Logs.info (fun m -> m "Concatting files...");
  Concatenator.concat_files path extension excluded_folders

let write_output_file concatted args =
  Logs.info (fun m -> m "Writing output file");
  let output_folder = Command_line.get_output_folder args
  and output_filename = Command_line.get_output_filename args in
  let output_path = Filename.concat output_folder output_filename in
  try
    File_manager.write_file output_path concatted;
    Logs.info (fun m -> m "%s" "Done!")
  with Sys_error e -> Logs.err (fun m -> m "%s (%s)" e output_path)

let runnable args = 
  Command_line.print_args args;
  let concatted = concat_with_args args in
  write_output_file concatted args

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  Logs.info (fun m -> m "%s" "--- Concatenator ---");
  Command_line.run_with_args runnable
