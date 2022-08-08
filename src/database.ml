open Yojson.Basic.Util

type timings =
  | Early_Morning
  | Morning
  | Lunch
  | Afternoon
  | Evening

(* "'early morning': before 10 A.M. 'morning': 10 A.M. to noon 'lunch
   time': noon to 2 P.M. 'after noon': 2 P.M. to 5:00 P.M. 'evening':
   after 5:00 P.M." *)

type time = {
  hour : int;
  minute : int;
  am_pm : string;
}

type instructor = {
  netid : string;
  first_name : string;
  middle_name : string;
  last_name : string;
}

type meeting = {
  time_start : string;
  time_end : string;
  instructors : instructor list;
}

type section = {
  component : string;
  section : string;
  meetings : meeting list;
  location : string;
}

type enrollGroup = {
  sections : section list;
  credits : int;
}
(* TODO: group type*)

type t = {
  course_number : string;
  course_title : string;
  course_name : string;
  subject : string;
  description : string;
  enrolledGroups : enrollGroup list;
  prereq : string;
  forbidden_overlap : string;
  catalogWhenOffered : string;
}

(** Get the string value corrresponding to the str keyword from the
    Yojson type j*)
let json_string j str : string = j |> member str |> to_string

let json_int j str : int = j |> member str |> to_int

let instructors_of_json j =
  {
    netid = json_string j "netid";
    first_name = json_string j "firstName";
    middle_name = json_string j "middleName";
    last_name = json_string j "lastName";
  }

let meeting_of_json j =
  {
    time_start = json_string j "timeStart";
    time_end = json_string j "timeEnd";
    instructors =
      j |> member "instructors" |> to_list
      |> List.map instructors_of_json;
  }

let section_of_json j =
  {
    component = json_string j "ssrComponent";
    section = json_string j "section";
    meetings =
      j |> member "meetings" |> to_list |> List.map meeting_of_json;
    location = json_string j "location";
  }

let enrolled_of_json j =
  {
    sections =
      j |> member "classSections" |> to_list |> List.map section_of_json;
    credits = json_int j "unitsMinimum";
  }

let t_of_json j =
  {
    course_name = json_string j "titleShort";
    course_number = json_string j "catalogNbr";
    course_title = json_string j "titleLong";
    subject = json_string j "subject";
    description = json_string j "description";
    enrolledGroups =
      j |> member "enrollGroups" |> to_list |> List.map enrolled_of_json;
    prereq = json_string j "catalogPrereqCoreq";
    forbidden_overlap = json_string j "catalogForbiddenOverlaps";
    catalogWhenOffered = json_string j "catalogWhenOffered";
  }

let parse j =
  try t_of_json j
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let rec print_string_list lst =
  match lst with
  | [] -> ()
  | h :: t ->
      Printf.printf
        ">::          Professor with netid %s has first name '%s', \
         middle name '%s', last name '%s' \n"
        h.netid h.first_name h.middle_name h.last_name;
      print_string_list t

let from_json json = parse json
let find_prereq f = f.prereq
let find_credits f = f.credits
let find_forbidden_overlap f = f.forbidden_overlap
let find_location f = f.location
let find_description f = f.description
let roster_json = Yojson.Basic.from_file "data/FA19 MATH.json"

let rec info_from_course lst req_course : bool =
  match lst with
  | [] -> false
  | h :: t ->
      (* The course's name is in the form MATH1920, CS3110, etc... *)
      let course_name =
        json_string h "subject" ^ json_string h "catalogNbr"
      in
      if course_name = req_course then (
        let jsonused = parse h in
        Printf.printf "Course Name: %s \n" jsonused.course_name;
        Printf.printf "Course Number: %s \n" jsonused.course_number;
        Printf.printf "Course Title: %s \n" jsonused.course_title;
        Printf.printf "Description: %s \n" jsonused.description;
        Printf.printf "Prerequisite: %s \n" jsonused.prereq;
        Printf.printf "Forbidden Overlap: %s \n"
          jsonused.forbidden_overlap;
        Printf.printf "Offered: %s \n" jsonused.catalogWhenOffered;

        true)
      else info_from_course t req_course

let from_json_roster req_course : bool =
  let major_json_list =
    roster_json |> member "data" |> member "classes" |> to_list
  in
  info_from_course major_json_list req_course

let conv_time time =
  match time with
  | "earlymorning" -> Early_Morning
  | "morning" -> Morning
  | "lunchtime" -> Lunch
  | "afternoon" -> Afternoon
  | "evening" -> Evening
  | _ -> failwith "Unsupported Time"

let make_time time =
  let split_time = String.split_on_char ':' time in
  let tail = List.tl split_time in
  {
    hour = int_of_string (List.hd split_time);
    minute = int_of_string (String.sub (List.hd tail) 0 2);
    am_pm = String.sub (List.hd tail) 2 2;
  }

(** Convert string representations of time into the type [time] *)
let classify_time time_start =
  let start = make_time time_start in
  match start.am_pm with
  | "AM" -> if start.hour < 10 then Early_Morning else Morning
  | "PM" ->
      if start.hour < 2 then Lunch
      else if start.hour < 5 then Afternoon
      else Evening
  | _ -> failwith "It has to be AM or PM"

let rec course_timing lst timing acc =
  match lst with
  | [] -> []
  | h :: t ->
      if List.length t >= 39 then
        let course = parse h in
        let enrolled_groups = course.enrolledGroups in
        let lectures =
          List.filter
            (fun x -> (List.hd x.sections).component = "LEC")
            enrolled_groups
        in
        let newacc =
          List.fold_left
            (fun acc lec ->
              let meeting = List.hd (List.hd lec.sections).meetings in
              if classify_time meeting.time_start = timing then
                course.course_name :: acc
              else acc)
            acc lectures
        in
        course_timing t timing newacc
      else acc

let time_filter timing json =
  let course_lst =
    json |> member "data" |> member "classes" |> to_list
  in
  let time = conv_time timing in
  course_timing course_lst time []

let time_sort subject timing =
  let cap_subject = String.uppercase_ascii subject in
  let json = Yojson.Basic.from_file "data/FA19 MATH.json" in
  time_filter timing json

let rec credits lst : int =
  match lst with
  | [] -> 0
  | h :: ts -> h.credits

let rec credits_from_course lst course_name_credits : int =
  match lst with
  | [] -> failwith "No such course"
  | h :: t ->
      (* The course's name is in the form MATH1920, CS3110, etc... *)
      let course_name =
        json_string h "subject" ^ json_string h "catalogNbr"
      in
      if course_name = course_name_credits then
        let jsonused = parse h in
        credits jsonused.enrolledGroups
      else credits_from_course t course_name_credits

let credits_from_json_roster req_course : int =
  let major_json_list =
    roster_json |> member "data" |> member "classes" |> to_list
  in
  credits_from_course major_json_list req_course

let rec sum_of_scores = function
  | (course, score) :: t ->
      let credits = credits_from_json_roster course |> float_of_int in
      (credits *. score) +. sum_of_scores t
  | [] -> 0.

let rec sum_of_credits = function
  | (course, score) :: t ->
      let credits = credits_from_json_roster course |> float_of_int in
      credits +. sum_of_credits t
  | [] -> 0.

let rec calculate_gpa_aux (lst : (string * float) list) : float =
  let change_upper =
    List.map (fun (str, fl) -> (String.uppercase_ascii str, fl)) lst
  in
  let total_scores = sum_of_scores change_upper in
  let total_credits = sum_of_credits change_upper in
  total_scores /. total_credits

let calculate_gpa = calculate_gpa_aux
