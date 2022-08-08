(** This module involves parsing commands and interacting with users
    with respect to majors. *)

(** The type [command] represents a user command. *)
type command =
  | Major of string
  | Quit
  | College of string
  | Course of string
  | Not_interested
  | Goback
  | Doublemajor of string list

type libcourses = {
  course : string;
  category : string;
  college : string;
}

type lib = {
  description : string;
  examples : libcourses list;
}
(** The type [lib] reprsents the liberal studies entry.*)

type engr = {
  descr : string;
  example : string list;
}

type major = {
  id : string;
  college : string;
  math_courses : string list;
  science_courses : string list;
  cs_courses : string list;
  engrd : string list;
  engri : engr list;
  advisor_appr_elec : string list;
  major_elec : string list;
  liberal_studies : lib list;
  fws : engr list;
  pe : engr list;
}
(** The type [engr] represents any json array with field for description
    and a field for a list.*)

exception Empty
exception Malformed
exception WrongMajor

exception NoNumber
(** Raised when a string has no integer in it and it is assumed it
    should. *)

val parse : string -> command
(** [parse str] parse a user's input into a [command], as follows. The
    first word of str becomes the verb. The rest becomes the object
    phrase.

    Requires: [str] contains only alphabetic and space characters.

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if the command is malformed. A command is
    {i malformed} if the verb is not one of "quit", "major",
    "notinterested", "college" or "course".*)

val maj_coll_exist : string -> string -> bool
(** [from_json req_major req_college] determines if the major and the
    college typed in is in the majors list. If it does not, raises
    exception WrongMajor. *)

val doub : string -> string -> string -> unit
val check_dobmajor_coll : string -> string -> string -> bool

val print_engrd : string -> string list -> unit
(** [print_engrd name input] parse a user's input into a [command], as
    follows. If the user inputs any answer other than "yes" or "no". The
    function allows him to input another input and prints an error
    message for him. If the user inputs "no", the interface does not
    print him the input. If the user inputs "yes", the interface prints
    the name followed by input. *)

val print_string_list_new_line : string list -> unit
(** [print_string_list_new_line lst] prints each element of [lst]
    individually on a line each where each entry is separated from the
    other by a comma. Example: print_string_list ["5","6"] prints 5 6*)

val print_string_list : string list -> unit
(** [print_string_list lst] prints each element of [lst] individually on
    a line each where each entry is separated from the other by a comma.
    If there is only one entry, it gets printed without a newline
    Example: print_string_list ["5"] prints 5 print_string_list
    ["5","6"] prints 5, 6*)

val cour_info : libcourses -> string
(** [cour_info cour] returns a string with information on that course.
    The string includes information on the name of the course, its
    category and college. Requires: [cour] is listed in one of the
    liberal studies courses. Example: cour_info "ANSC 4140" returns cour
    ANSC 4140 is in category KCM and is in college CALS.*)

val print_descr : lib list -> string
(** [print_descr lst] returns the description listed for that lst.
    Example: print_descr liberal_studies returns "You need at least 18
    credits of lib studies with at least 6 courses."*)

val examples_strlst : libcourses list -> string list
(** [examples_strlst lst] returns a list where each element is a string
    representation for that course assuming the course is listed in
    liberal studies. Requires: [lst] is a list of exmaples in
    Liberal-studies. *)

val return_examples : lib list -> string list
(** [return_examples lst] returns a list representing the examples for
    Liberal_studies. Requires: [lst] is a reprsentation for examples
    field in Liberal_studies *)

val print_lib : string -> lib list -> unit
(** [print_lib name input] parse a user's input into a [command], as
    follows. If the user inputs any answer other than "yes" or "no". The
    function allows him to input another input and prints an error
    message for him. If the user inputs "no", the interface does not
    print him the input. If the user inputs "yes", the interface prints
    the name followed by input. Requires: the input is Liberal_studies
    reprsentation *)

val liberal_lst : Yojson.Basic.t list -> libcourses list
(** [liberal_lst lst] returns a list representing the examples for
    Liberal_studies. It reads that info directly from the json file.
    Requires: [lst] is a reprsentation for examples field in
    Liberal_studies *)

val liberalstudies_info : Yojson.Basic.t list -> lib list
(** [liberalstudies_info lst] returns a list representing the array for
    Liberal_studies. It reads that info directly from the json file.
    Requires: [lst] is a reprsentation Liberal_studies *)

val engri_examples : Yojson.Basic.t list -> string list
(** [engri_examples lst] returns a list representing the exmaples field
    for fields like engri in the json file*)

val engri_lst : Yojson.Basic.t list -> engr list
(** [engri_lst lst] returns a list where each ehtry is a record with
    field of description and examples. It reads that info directly from
    the json file.*)

val engri_desc : engr list -> string
(** [engri_desc lst] returns the info stored in the description entry
    for fields like engri.*)

val enrg_example : engr list -> string list
(** [enrg_example lst] returns the info stored in the exmaple entry for
    fields like engri.*)

val print_engri : string -> engr list -> unit
(** [print_engri name input] parse a user's input into a [command], as
    follows. If the user inputs any answer other than "yes" or "no". The
    function allows him to input another input and prints an error
    message for him. If the user inputs "no", the interface does not
    print him the input. If the user inputs "yes", the interface prints
    the name followed by input. Requires: the input is designed to be
    the style of engri wuth a descripion field as a string and example
    field as a string list.*)

val courses_from_json : Yojson.Basic.t list -> string -> string -> bool
(** [courses_from_json lst major college] takes a major and college and
    checks if that major and college are valid which happens if they
    atre listed in the list passed in. If not, it returns a message to
    the user. Else, it prints the infomation for that major and college
    to the user.*)

val courses_from_json_state :
  Yojson.Basic.t list -> string -> string -> major
(** [courses_from_json_state lst major college] takes a major and
    college and checks if that major and college are valid which happens
    if they atre listed in the list passed in. If not, it returns a
    message to the user and raises an exception to show that. Else, it
    returns a record containing the infomation for that certain major.*)

val from_json_producing_state : string -> string -> major
(** [from_json major college] takes a major and college. If that major
    and college is not listed in the valid list of majors, it raises an
    exception. Otherwise, it returns a record containing the information
    for that major by calling courses_from_json_state.*)

val newcourses_gen : 'a list -> 'a list -> 'a list
(** [from_json lst1 lst2] returns the union of the 2 lists with no
    repeated elements.*)

val doub : string -> string -> string -> unit
(** [doub firstmajor secondmajor college] prints all the information
    needed for the user to major in those 2 majors in that college.
    Requires: both majors are valid .*)

val remove_spaces : string list -> string list
val capitalize_and_add : string list -> string list

val updated_roadmap : major -> string list -> unit
(** [updated_roadmap rec lst] takes [lst] representing list of courses
    and a record [rec] representing the information for some major. It
    outputs the new updated roadmap eliminating from the required
    courses in rec, the courses listed in [lst] that were taken by the
    user.*)

val return_first_char : string -> int -> int
(** [returnfirstchar str n] returns the index position of the first
    integer found in [str]. Raises: Failure [str] has no integer in it. *)

val doub_record : string -> string -> string -> major
(** [doub_record firstmaj secondmaj reqcollege] finds [firstmaj] offered
    in [reqcollege] in the list of courses and finds [secondmaj] offered
    in [reqcollege]. It outputs a record containing the information for
    that dounlemajor. Raises: WrongMajor if either major is not offered
    in [reqcollege] in the list of majors.*)
