open Cohttp
open Lwt.Infix
open Cohttp_lwt_unix
open Core.Out_channel

exception Invalid

let edit term subject =
  "https://classes.cornell.edu/api/2.0/search/classes.json?roster="
  ^ term ^ "21&subject=" ^ subject

let body term subject =
  Client.get (Uri.of_string (edit term subject)) >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body -> body

let valid term subject =
  let json_string = Lwt_main.run (body term subject) in
  if String.sub json_string 11 5 = "error" then raise Invalid;
  if String.sub json_string 11 7 = "success" then
    write_all
      ("data/" ^ term ^ "21 " ^ subject ^ ".json")
      ~data:json_string;
  if String.sub json_string 11 7 = "success" then
    print_endline
      "Success,json file created in the data folder Program Quitting"

let rec update_attempt subject term =
  try valid term subject
  with Invalid -> (
    print_endline "update failed";
    print_endline "enter the intended subject again or type quit";
    match read_line () with
    | exception End_of_file -> ()
    | "quit" -> print_endline "exiting the program"
    | subject -> (
        print_endline "enter the term or quit";
        print_endline "type in FA/SP";
        match read_line () with
        | exception End_of_file -> ()
        | "quit" -> print_endline "exiting the program"
        | term -> update_attempt (String.uppercase_ascii subject) term))

let update subject =
  print_endline "enter the term or quit";
  print_endline "type in FA/SP";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> print_endline "exiting the program"
  | term -> update_attempt subject term

let main () =
  print_endline "Update database files";
  print_endline "enter the subject you want to update";
  print_endline "Use abreviations for subject";
  print_endline "Quit by typing quit";

  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> print_endline "exiting the program"
  | subject -> update (String.uppercase_ascii subject)

let () = main ()
