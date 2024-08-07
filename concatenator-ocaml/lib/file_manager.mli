val read_file : string -> string
val read_files : string list -> string list
val write_file : string -> string -> unit
val walk_files : string -> filter:(string -> bool) -> string list
val get_concatenator_filters : string -> string list -> string -> bool
