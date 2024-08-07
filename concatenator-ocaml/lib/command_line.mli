type arguments = {
  path : string;
  excluded_folders : string list;
  extension : string;
  output_folder : string;
  output_filename : string;
}
[@@deriving accessors]

val get_path : arguments -> string
val get_excluded_folders : arguments -> string list
val get_extension : arguments -> string
val get_output_folder : arguments -> string
val get_output_filename : arguments -> string
val run_with_args : (arguments -> unit) -> 'a
val print_args : arguments -> unit
