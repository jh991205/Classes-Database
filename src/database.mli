(** Representation of static class roster database data. This module
    represents the data ... *)
type timings =
  | Early_Morning
  | Morning
  | Lunch
  | Afternoon
  | Evening

type instructor
(** The abstract type of values representing an instructor *)

type meeting
(** The abstract type of values representing meeting information *)

type section
(** The abstract type of values representing sections *)

type t
(** The abstract type of values representing class information *)

type enrollGroup

val t_of_json : Yojson.Basic.t -> t

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the course that [j] represents. Requires: [j] is a
    valid JSON course representation.*)

val from_json_roster : string -> bool
(** [from_json_roster j] is true if j is one of the courses in
    roster.json. j is a name of a course class info that [j] represents.*)

val find_prereq : t -> string
(** [find_prereq a] is the identifier of the pre reqs in class
    information [a]. *)

val find_credits : enrollGroup -> int
(** [find_credits a] is the identifier of the credits in class
    information [a]. *)

val find_forbidden_overlap : t -> string
(** [find_forbbidden_overlap a] is the identifier of the forbidden
    overlaps in class information [a]. *)

val find_location : section -> string
(** [find_location a] is the identifier of the locations in class
    information [a]. *)

val find_description : t -> string
(** [find_description a] is the identifier of the descriptions in class
    information [a]. *)

val conv_time : string -> timings
val time_sort : string -> string -> string list

val credits_from_json_roster : string -> int
(** [credits_from_json_roster] finds the credits of a course by the
    course name. *)

val sum_of_scores : (string * float) list -> float
val sum_of_credits : (string * float) list -> float
val calculate_gpa : (string * float) list -> float
