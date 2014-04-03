let size = 4096
let map = Array.make size 0

let get n =
  Array.get map n

let set n x =
  Array.set map n x

let set_char n =
  match n with
  (* 0 *)
  | 0 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x90;
    set (n + 2) 0x90;
    set (n + 1) 0x90;
    set (n + 40) 0xF0
  (* 1 *)
  | 5 ->
    set (n + 4) 0x20;
    set (n + 3) 0x60;
    set (n + 2) 0x20;
    set (n + 1) 0x20;
    set (n + 0) 0x70
  (* 2 *)
  | 10 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x10;
    set (n + 2) 0xF0;
    set (n + 1) 0x80;
    set (n + 0) 0xF0
  (* 3 *)
  | 15 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x10;
    set (n + 2) 0xF0;
    set (n + 1) 0x10;
    set (n + 0) 0xF0
  (* 4 *)
  | 20 ->
    set (n + 4) 0x90;
    set (n + 3) 0x90;
    set (n + 2) 0xF0;
    set (n + 1) 0x10;
    set (n + 0) 0x10
  (* 5 *)
  | 25 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x80;
    set (n + 2) 0xF0;
    set (n + 1) 0x10;
    set (n + 0) 0xF0
  (* 6 *)
  | 30 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x80;
    set (n + 2) 0xF0;
    set (n + 1) 0x90;
    set (n + 0) 0xF0
  (* 7 *)
  | 35 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x10;
    set (n + 2) 0x20;
    set (n + 1) 0x40;
    set (n + 0) 0x40
  (* 8 *)
  | 40 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x90;
    set (n + 2) 0xF0;
    set (n + 1) 0x90;
    set (n + 0) 0xF0
  (* 9 *)
  | 45 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x90;
    set (n + 2) 0xF0;
    set (n + 1) 0x10;
    set (n + 0) 0xF0
  (* A *)
  | 50 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x90;
    set (n + 2) 0xF0;
    set (n + 1) 0x90;
    set (n + 0) 0x90
  (* B *)
  | 55 ->
    set (n + 4) 0xE0;
    set (n + 3) 0x90;
    set (n + 2) 0xE0;
    set (n + 1) 0x90;
    set (n + 0) 0xE0
  (* C *)
  | 60 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x80;
    set (n + 2) 0x80;
    set (n + 1) 0x80;
    set (n + 0) 0xF0
  (* D *)
  | 65 ->
    set (n + 4) 0xE0;
    set (n + 3) 0x90;
    set (n + 2) 0x90;
    set (n + 1) 0x90;
    set (n + 0) 0xE0
  (* E *)
  | 70 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x80;
    set (n + 2) 0xF0;
    set (n + 1) 0x80;
    set (n + 0) 0xF0
  (* F *)
  | 75 ->
    set (n + 4) 0xF0;
    set (n + 3) 0x80;
    set (n + 2) 0xF0;
    set (n + 1) 0x80;
    set (n + 0) 0x80
  | _ -> ()

let set_char_map () =
  for i = 0 to 0xF do
    begin
      set_char (i * 5)
    end
  done
