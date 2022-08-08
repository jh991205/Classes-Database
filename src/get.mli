(** This module is used to take and output information from the Cornell
    Class Roster API. *)

val body : string Lwt.t
(** [body] outputs the information about the Class Roster *)