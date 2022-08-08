open Yojson.Basic.Util
open Formatter

exception WrongMajor

(** [command] represents the user command *)
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
(** [libcourses] represents a libral study course*)

type lib = {
  description : string;
  examples : libcourses list;
}
(** [lib] represent libral studies *)

type engr = {
  descr : string;
  example : string list;
}
(** [engr] represents engineering courses *)

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
(** [major] represents a major *)

exception Empty
exception Malformed
exception NoNumber

let rec remove_spaces = function
  | h :: t -> if h = "" then remove_spaces t else h :: remove_spaces t
  | _ -> []

(** [parse] parses the user input to the command type *)
let parse str =
  if str = "" then raise Empty
  else
    let created_lst = remove_spaces (String.split_on_char ' ' str) in
    match created_lst with
    | [ y ] -> (
        match [ y ] with
        | [ "quit" ] -> Quit
        | [ "notinterested" ] -> Not_interested
        | ["goback"] -> Goback
        | _ -> raise Malformed)
    | [ x; y ] -> (
        match [ x ] with
        | [ "major" ] -> Major y
        | [ "college" ] -> College y
        | [ "course" ] -> Course y
        | _ -> raise Malformed)
    | x :: y -> (
        match [ x ] with
        | [ "doublemajor" ] -> Doublemajor y
        | _ -> raise Malformed)
    | _ -> raise Malformed

let csjson = Yojson.Basic.from_file "data/majors.json"

(** [print_string_list_new_line] print each string list element each on
    a new line, ends with a comma at the end of each line*)
let rec print_string_list_new_line lst =
  match lst with
  | [] -> ()
  | h :: (x :: y as t) ->
      print_endline "";
      print_string h;
      print_string ", ";
      print_string_list_new_line t
  | [ h ] ->
      print_endline "";
      print_string h

let rec print_string_list = function
  | [] -> ()
  | h :: (x :: y as t) ->
      print_string h;
      print_string ", ";
      print_endline "";
      print_string_list t
  | [ h ] -> print_string h

(** [print_engrd] takes one type of distribution of ENGRD and outputs
    the courses in this distribution *)
let rec print_engrd name input =
  Printf.printf
    "\n\nWould you like to print the required %s, 'yes' or 'no'? Or you can enter 'quit' to quit\n" name;
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "" ] ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\n\
             Error: Empty command. Try again with a valid command this \
             time.";
          print_engrd name input
      | [ "yes" ] ->
          print_endline "\nlovely!";
          print_col name input
      | [ "no" ] -> print_endline "\nas you wish!"
      | [ "quit" ] -> print_endline "\n Okay, bye!"; exit 0
      | _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\n\
            \ Error: Malformed command. Try again with a valid command \
             this time.";
          print_engrd name input)

(** [cour_info] returns the course' description, category and
    college as a string *)
let cour_info cour =
  "course " ^ cour.course ^ " is in category " ^ cour.category
  ^ " and is in college " ^ cour.college

(** [print_descr] prints the descriptions of the input courses *)
let print_descr lst =
  match lst with
  | [ h ] -> h.description
  | _ -> ""

(** [examples_strlst] returns a the list of the all the courses'
    description, category and college *)
let rec examples_strlst lst =
  match lst with
  | [] -> []
  | h :: t -> cour_info h :: examples_strlst t

(** [return_examples] returns the examples of ENGR *)
let return_examples lst =
  match lst with
  | [ h ] -> examples_strlst h.examples
  | _ -> []

let rec print_lib name input =
  Printf.printf
    "\n\nWould you like to print the required %s, 'yes' or 'no'? Or you can enter 'quit' to quit.\n" name;
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "" ] ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\n\
             Error: Empty command. Try again with a valid command this \
             time.";
          print_lib name input
      | [ "yes" ] ->
          print_endline "\nlovely!";
          print_endline (print_descr input);
          print_col name (return_examples input)
          (*Printf.printf "\n\n%s:" name; print_endline "";
            print_string_list (return_examples input)*)
      | [ "no" ] -> print_endline "\nas you wish!"
      | [ "quit" ] -> print_endline "\nOkay, bye!"; exit 0
      | _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\n\
            \ Error: Malformed command. Try again with a valid command \
             this time.";
          print_lib name input)

let rec liberal_lst lst =
  match lst with
  | [] -> []
  | h :: t ->
      {
        course = to_string (member "course" h);
        category = to_string (member "category" h);
        college = to_string (member "college" h);
      }
      :: liberal_lst t

let rec liberalstudies_info lst =
  match lst with
  | x :: y ->
      [
        {
          description = to_string (member "description" x);
          examples = liberal_lst (to_list (member "examples" x));
        };
      ]
      @ liberalstudies_info y
  | [] -> []

let rec engri_examples lst =
  match lst with
  | [] -> []
  | h :: t -> to_string h :: engri_examples t

let rec engri_lst lst =
  match lst with
  | h :: t ->
      {
        descr = to_string (member "description" h);
        example = engri_examples (to_list (member "examples" h));
      }
      :: engri_lst t
  | [] -> []

let engri_desc lst =
  match lst with
  | [ h ] -> h.descr
  | _ -> ""

let enrg_example lst =
  match lst with
  | [ h ] -> h.example
  | _ -> []

let rec engr_lst_generate lst =
  match lst with
  | [] -> []
  | h :: t -> h :: engr_lst_generate t

(** [check_major_coll] checks both the major and the college*)
let check_major_coll lst maj coll =
  match lst with
  | [] -> false
  | h :: t ->
      if
        h |> member "id" |> to_string = maj
        && h |> member "college" |> to_string = coll
      then true
      else false

let rec print_engri name input =
  Printf.printf
    "\n\nWould you like to print the required %s, 'yes' or 'no'? Or you can type 'quit' to quit.\n" name;
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "" ] ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\n\
             Error: Empty command. Try again with a valid command this \
             time.";
          print_engri name input
      | [ "yes" ] ->
          print_endline "\nlovely!\n\n";
          print_string (engri_desc input ^ "\n\n");
          print_col name (engr_lst_generate (enrg_example input))
      | [ "no" ] -> print_endline "\nas you wish!"
      | [ "quit" ] -> print_endline "\n Okay, bye!"
      | _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\n\
            \ Error: Malformed command. Try again with a valid command \
             this time.";
          print_engri name input)

(** [courses_from_json] prints the math, science, cs, engr, ..., etc.
    courses if the requested_major and the req_college is in the major,
    college list *)
let rec courses_from_json
    lst
    (requested_major : string)
    (req_college : string) : bool =
  match lst with
  | [] ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n Error : Unvalid major or college! Enter a valid one. \n";
      false
  | h :: t ->
      if
        h |> member "id" |> to_string = requested_major
        && h |> member "college" |> to_string = req_college
      then (
        let math_courses =
          h |> member "Math courses" |> to_list |> filter_string
        in
        let science_courses =
          h |> member "Science courses" |> to_list |> filter_string
        in
        let cs_courses =
          h |> member "CS courses" |> to_list |> filter_string
        in
        let engrd = h |> member "ENGRD" |> to_list |> filter_string in
        let engri = h |> member "ENGRI" |> to_list |> engri_lst in
        let advisor_elec =
          h
          |> member "Advisor Appr Electives"
          |> to_list |> filter_string
        in
        let major_elec =
          h |> member "Major Elective" |> to_list |> filter_string
        in
        let liberal_studies =
          h
          |> member "Liberal Studies"
          |> to_list |> liberalstudies_info
        in
        let fws = h |> member "FWS" |> to_list |> engri_lst in
        let pe = h |> member "PE" |> to_list |> engri_lst in
        print_col "Math Courses" math_courses;
        print_col "Science Courses" science_courses;
        print_col "CS Courses" cs_courses;
        print_engrd "Engineering Distrubution (Engrd) courses" engrd;
        print_engri "ENGRI Courses" engri;
        print_engrd "Advisor Approved Electives" advisor_elec;
        print_engrd "Major Electives" major_elec;
        print_lib "Liberal Studies" liberal_studies;
        print_engri "First-Year Writing Seminars" fws;
        print_engri "PE Classes" pe;
        print_endline "";
        true)
      else courses_from_json t requested_major req_college

(** [maj_coll_exist] checks if the req_major and req_college are in the
    major, college list*)
let maj_coll_exist (req_major : string) (req_college : string) : bool =
  let major_json_list = csjson |> member "majors" |> to_list in
  courses_from_json major_json_list req_major req_college

(** [courses_from_json_state] constructs a major if it is a valid major in
    the corresponding college and the major, college all exist in the
    major, college list*)
let rec courses_from_json_state
    lst
    (requested_major : string)
    (req_college : string) =
  match lst with
  | [] ->
      ANSITerminal.print_string
        [ ANSITerminal.magenta ]
        "\n Error : Unvalid major or college! Enter a valid one. \n";
      raise WrongMajor
  | h :: t ->
      if
        h |> member "id" |> to_string = requested_major
        && h |> member "college" |> to_string = req_college
      then
        let math_courses =
          h |> member "Math courses" |> to_list |> filter_string
        in
        let science_courses =
          h |> member "Science courses" |> to_list |> filter_string
        in
        let cs_courses =
          h |> member "CS courses" |> to_list |> filter_string
        in
        let engrd = h |> member "ENGRD" |> to_list |> filter_string in
        let engri = h |> member "ENGRI" |> to_list |> engri_lst in
        let advisor_elec =
          h
          |> member "Advisor Appr Electives"
          |> to_list |> filter_string
        in
        let major_elec =
          h |> member "Major Elective" |> to_list |> filter_string
        in
        let liberal_studies =
          h
          |> member "Liberal Studies"
          |> to_list |> liberalstudies_info
        in
        let fws = h |> member "FWS" |> to_list |> engri_lst in
        let pe = h |> member "PE" |> to_list |> engri_lst in
        {
          id = h |> member "id" |> to_string;
          college = h |> member "college" |> to_string;
          math_courses;
          science_courses;
          cs_courses;
          engrd;
          engri;
          advisor_appr_elec = advisor_elec;
          major_elec;
          liberal_studies;
          fws;
          pe;
        }
      else courses_from_json_state t requested_major req_college

(** [from_json_producing_state] constructs a major out of reqmajor and
    reqcollege *)
let from_json_producing_state reqmajor reqcollege =
  let major_json_list = csjson |> member "majors" |> to_list in
  courses_from_json_state major_json_list reqmajor reqcollege

let check_dobmajor_coll fmaj smaj reqcollege =
  let major_json_list = csjson |> member "majors" |> to_list in
  check_major_coll major_json_list fmaj reqcollege
  && check_major_coll major_json_list smaj reqcollege

let check_majcoll_with_lst fmaj reqcollege =
  let major_json_list = csjson |> member "majors" |> to_list in
  check_major_coll major_json_list fmaj reqcollege

(** [newcourses_gen] combines two course lists and produce a new course
    list without duplicates. *)
let newcourses_gen lst1 lst2 =
  let notshared = List.filter (fun x -> not (List.mem x lst1)) lst2 in
  lst1 @ notshared

(** [remove] removes [elem] from [lst]*)
let rec remove lst elem =
  match lst with
  | [] -> []
  | h :: x :: t ->
      if h = elem then x :: t
      else if x = elem then h :: t
      else h :: x :: remove t elem
  | [ h ] -> if h = elem then [] else [ h ]

(** [newcourses lst1 lst2] returns the updated list of the elemnts of lst1 but after removing from them any shared elements with lst2 *)
let rec newcourses lst1 lst2 =
  match lst2 with
  | [] -> lst1
  | h :: t ->
      if List.mem h lst1 then newcourses (remove lst1 h) t
      else newcourses lst1 t

let doub firstmaj secondmaj reqcollege =
  let first_major = from_json_producing_state firstmaj reqcollege in
  let second_major = from_json_producing_state secondmaj reqcollege in
  let first_math = first_major.math_courses in
  let second_math = second_major.math_courses in
  let first_science = first_major.science_courses in
  let second_science = second_major.science_courses in
  let first_cs_courses = first_major.cs_courses in
  let second_cs_courses = second_major.cs_courses in
  let first_engrd = first_major.engrd in
  let second_engrd = second_major.engrd in
  let first_engri = first_major.engri in
  let second_engri = second_major.engri in
  let first_adv = first_major.advisor_appr_elec in
  let second_adv = second_major.advisor_appr_elec in
  let first_elec = first_major.major_elec in
  let second_elec = second_major.major_elec in
  print_col "Math courses: " (newcourses_gen first_math second_math);
  print_col "Science courses: " (newcourses_gen first_science second_science);
  print_col "CS courses: " (newcourses_gen first_cs_courses second_cs_courses);
  print_engrd "Engineering Distrubution (Engrd) courses"
    (newcourses_gen first_engrd second_engrd);
  print_engri "ENGRI Courses" (newcourses_gen first_engri second_engri);
  print_engrd "Advisor Approved Electives"
    (newcourses_gen first_adv second_adv);
  print_engrd "Major Electives" (newcourses_gen first_elec second_elec);
  print_lib "Liberal Studies" first_major.liberal_studies;
  print_engri "First-Year Writing Seminars" first_major.fws;
  print_engri "PE Classes" first_major.pe;
  print_endline ""

(** [doub_record] returns the major representation of someone who is
    taking two majors *)
let doub_record firstmaj secondmaj reqcollege =
  let first_major = from_json_producing_state firstmaj reqcollege in
  let second_major = from_json_producing_state secondmaj reqcollege in
  let first_math = first_major.math_courses in
  let second_math = second_major.math_courses in
  let first_science = first_major.science_courses in
  let second_science = second_major.science_courses in
  let first_cs_courses = first_major.cs_courses in
  let second_cs_courses = second_major.cs_courses in
  let first_engrd = first_major.engrd in
  let second_engrd = second_major.engrd in
  let first_engri = first_major.engri in
  let second_engri = second_major.engri in
  let first_adv = first_major.advisor_appr_elec in
  let second_adv = second_major.advisor_appr_elec in
  let first_elec = first_major.major_elec in
  let second_elec = second_major.major_elec in
  {
    id = "";
    college = reqcollege;
    math_courses = newcourses_gen first_math second_math;
    science_courses = newcourses_gen first_science second_science;
    cs_courses = newcourses_gen first_cs_courses second_cs_courses;
    engrd = newcourses_gen first_engrd second_engrd;
    engri = newcourses_gen first_engri second_engri;
    advisor_appr_elec = newcourses_gen first_adv second_adv;
    major_elec = newcourses_gen first_elec second_elec;
    liberal_studies = first_major.liberal_studies;
    fws = first_major.fws;
    pe = first_major.pe;
  }

(** [fwspe] returns true if there are at least two same elements in lst1 and lst2 *)
let fwspe lst1 lst2 =
  let lst3 = List.filter (fun x -> List.mem x lst1) lst2 in
  if List.length lst3 >= 2 then true else false
(** [strip_chars s cs] returns string [s] but with characters removed if
    the character also appears in the string [cs] *)
(** [strip_chars s cs] returns string [s] but with characters removed if the character also appears in the string [cs] *)
let strip_chars s cs =
  let len = String.length s in
  let res = Bytes.create len in
  let rec aux i j =
    if i >= len then Bytes.to_string (Bytes.sub res 0 j)
    else if String.contains cs s.[i] then aux (succ i) j
    else begin
      Bytes.set res j s.[i];
      aux (succ i) (succ j)
    end
  in
  aux 0 0

(** [lower_no_space lst] turns every character in lst into lower case, and remove all the spaces in [lst] *)
let lower_no_space lst =
  let lst2 = List.map (fun x -> String.lowercase_ascii x) lst in
  List.map (fun y -> strip_chars y " ") lst2


(** [return_first_char str n] will return the integer value embedded in the string str at some location *)
let rec return_first_char str n =
  match int_of_string (String.make 1 str.[n]) with
  | exception Failure _ ->
      if String.length str - 1 > n then return_first_char str (n + 1)
      else raise NoNumber
  | _ -> n

let capitalize_and_add lst =
  let newlst = List.map (fun x -> String.uppercase_ascii x) lst in
  List.map
    (fun x ->
      let a = return_first_char x 0 in
      String.sub x 0 a ^ " " ^ String.sub x a (String.length x - a))
    newlst

let updated_roadmap a lst =
  let lst1 =
    newcourses (lower_no_space a.math_courses) lst
  in
  let lst2 =
    newcourses
      (lower_no_space a.science_courses)
      lst
  in
  let lst3 =
    newcourses (lower_no_space a.cs_courses) lst
  in
  print_col "Math Courses" (capitalize_and_add lst1);
  Stdlib.print_string "\n";
  print_col "Science Courses" (capitalize_and_add lst2);
  Stdlib.print_string "\n";
  print_col "CS Courses" (capitalize_and_add lst3)


