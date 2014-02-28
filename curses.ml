open Curses

let main () =
  let win = initscr () in
  let y, x = getyx win in
  Printf.printf "y[%d], x[%d]\n" y x
  endwin ()


let _ = main ()
