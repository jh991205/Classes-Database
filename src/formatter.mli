(** Formats the list of classes to be printed and shown to the user. *)

val print_col : string -> string list -> unit
(** [print_col h l] prints the column using print formatting and
    ANSITerminal. The header [h] is the heading of the column, and the
    each string from [l] makes up for the rest of the remaining rows. *)
