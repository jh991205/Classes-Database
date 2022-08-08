type column = {
  header : string;
  subj_lst : string list;
}

(** [cursor_to_mid str] moves the cursor to a position such that [str]
    ends up being horizontally centered in the window when it is
    printed. Returns the position [x] where the cursor is located *)
let cursor_to_mid str =
  (* CREDIT: got the interpretation in the line below of
     ANSITerminal.size from sss3336's project through Github search. *)
  let width, _ = ANSITerminal.size () in
  let x, y = ANSITerminal.pos_cursor () in
  let mid_x = (width / 2) - (String.length str / 2) in
  ANSITerminal.move_cursor mid_x y;
  mid_x

(** [print_row x str] prints a string as the row of a table starting
    from the x position given *)
let print_row x str =
  (*let _, y = ANSITerminal.pos_cursor () in ANSITerminal.move_cursor x
    y;*)
  let _ = cursor_to_mid str in
  ANSITerminal.print_string
    [ ANSITerminal.white; ANSITerminal.on_black ]
    str;
  Stdlib.print_string "\n"

(** [column_init h l] initializes a new [column] c with [h] as the
    header and [l] as the subj_list. *)
let column_init head lst = { header = head; subj_lst = lst }

let print_col head lst =
  let column = column_init head lst in
  Stdlib.print_string "\n";

  let mid_x = cursor_to_mid column.header in

  (* Print the header *)
  ANSITerminal.print_string
    [
      ANSITerminal.Underlined;
      ANSITerminal.Bold;
      ANSITerminal.black;
      ANSITerminal.on_white;
    ]
    column.header;
  Stdlib.print_string "\n\n";
  let _ = List.map (print_row mid_x) column.subj_lst in
  Stdlib.print_string "\n"
