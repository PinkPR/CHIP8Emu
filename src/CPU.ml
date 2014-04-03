let reg_nb = 16
(* let pc = ref 0x200 *)
let regs = Array.make reg_nb 0
let reg_i = ref 0

let fetch pc =
  pc := !pc + 2;
  (Memory.get (!pc - 2) * 0x100)+ (Memory.get (!pc - 1))

let and_nb nb1 nb2 =
  if (nb1 = 0) || (nb2 = 0) then
    0
  else
    1

let or_nb nb1 nb2 =
  if (nb1 <> 0) || (nb2 <> 0) then
    1
  else
    0

let xor_nb nb1 nb2 =
  if (nb1 = 0) && (nb2 <> 0) then
    1
  else if (nb1 <> 0) && (nb2 = 0) then
    1
  else
    0

let rec draw_sprite x y i height ct =
  let line = Memory.get i
  in
    let rec draw_line value count =
      match value with
      | 0 -> ()
      | _ ->  if (value mod 2) <> 0 then
                begin
                  Screen.plot (x + count) (y + ct - 1) Screen.fg_color;
                  Array.set regs 0xF 1
                end;
                draw_line (value / 2) (count + 1)
    in
      draw_line line 0;
      if ct <> (height - 1) then
        draw_sprite x y (i + 1) height (ct + 1)

let print_state pc =
  Printf.printf "PC : %X\n" !pc;
  for i = 0 to 0xF do
    Printf.printf "REG[%X] : %X\n" i (Array.get regs i)
  done;
  Printf.printf "REG_I : %X\n\n" !reg_i

let rec call_subrt addr =
  let pc2 = ref 0 and
  ins = ref 0
  in
    pc2 := addr;
    ins := fetch pc2;
    while !ins <> 0x00EE do
      begin
        execute !ins pc2;
        ins := fetch pc2
      end
    done
and
execute instr pc =
  Printf.printf "INSTR : %x\n" instr;
  match instr with
  (* 0x00E0 Screen cleaning *)
  | 0x00E0 -> Screen.clear ()
  (* 0x00EE Subroutine return *)
  | 0x00EE -> ()
  (* 0x1NNN Jump on NNN*)
  | ins when (ins land 0xF000) = 0x1000 -> pc := ins land 0x0FFF
  (* 0x2NNN Calls subroutine at 0xNNN *)
  | ins when (ins land 0xF000) = 0x2000 ->
    call_subrt (ins land 0x0FFF)
  (* 0x3XKK Goes next instr if VX == KK *)
  | ins when (ins land 0xF000) = 0x3000 ->
    let sub reg value =
      if (Array.get regs reg) = value then
        pc := !pc + 2
    in
      sub ((ins land 0x0F00) / 0x0100) (ins land 0x00FF)
  (* 0x4XKK Goes next if VX <> KK *)
  | ins when (ins land 0xF000) = 0x4000 ->
    let sub reg value =
      if (Array.get regs reg) <> value then
        pc := !pc + 2
    in
      sub ((ins land 0x0F00) / 0x0100) (ins land 0x00FF)
  (* 0x5XY0 Goes next if VX == VY *)
  | ins when (ins land 0xF000) = 0x5000 ->
    let sub reg1 reg2 =
      if (Array.get regs reg1) = (Array.get regs reg2) then
        pc := !pc + 2
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00F0) / 0x0010)
  (* 0x6XKK Load KK in VX *)
  | ins when (ins land 0xF000) = 0x6000 ->
    let sub reg value =
      Array.set regs reg value
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00FF))
  (* 0x7XKK Load VX + KK in VX *)
  | ins when (ins land 0xF000) = 0x7000 ->
    let sub reg value =
      Array.set regs reg ((Array.get regs reg) + value);
      Array.set regs reg (0xFF land (Array.get regs reg))
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00FF))
  (* 0x8XY0 Loads VY in VX *)
  | ins when (ins land 0xF00F) = 0x8000 ->
    let sub reg1 reg2 =
      Array.set regs reg1 (Array.get regs reg2)
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00F0) / 0x0010)
  (* 0x8XY1 Loads VX || VY *)
  | ins when (ins land 0xF00F) = 0x8001 ->
    let sub reg1 reg2 =
      Array.set regs reg1 (or_nb (Array.get regs reg1) (Array.get regs reg2))
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00F0) / 0x0010)
  (* 0x8XY2 Loads VX && VY *)
  | ins when (ins land 0xF00F) = 0x8002 ->
    let sub reg1 reg2 =
      Array.set regs reg1 (and_nb (Array.get regs reg1) (Array.get regs reg2))
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00F0) / 0x0010)
  (* 0x8XY3 Loads VX ^ VY *)
  | ins when (ins land 0xF00F) = 0x8003 ->
    let sub reg1 reg2 =
      Array.set regs reg1 (xor_nb (Array.get regs reg1) (Array.get regs reg2))
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00F0) / 0x0010)
  (* 0x8XY4 Loads VX + VY in VX; VF set to 1 if overflow, 0 otherwise*)
  | ins when (ins land 0xF00F) = 0x8004 ->
    let sub reg1 reg2 =
      Array.set regs reg1 ((Array.get regs reg1) + (Array.get regs reg2));
      (if (Array.get regs reg1) > 0xFF then
        Array.set regs 0xF 1
      else
        Array.set regs 0xF 0);
      Array.set regs reg1 ((Array.get regs reg1) land 0xFF)
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00F0) / 0x0010)
  (* 0x8XY5 Load VX - VY in VX; VF set to 1 if VY > VX, 0 otherwise *)
  | ins when (ins land 0xF00F) = 0x8005 ->
    let sub reg1 reg2 =
      (if (Array.get regs reg1) < (Array.get regs reg2) then
        Array.set regs 0xF 1
      else
        Array.set regs 0xF 0);
      Array.set regs reg1 ((Array.get regs reg1) - (Array.get regs reg2))
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00F0) / 0x0010)
  (* 0x8XY6 Right shift on VX; puts output bit in VF *)
  | ins when (ins land 0xF00F) = 0x8006 ->
    let sub reg1 =
      Array.set regs 0xF ((Array.get regs reg1) mod 2);
      Array.set regs reg1 ((Array.get regs reg1) / 2)
    in
      sub ((ins land 0x0F00) / 0x0100)
  (* 0x8XY7 Loads VY - VX in VX; VF set to 1 if VX > VY, 0 otherwise *)
  | ins when (ins land 0xF00F) = 0x8007 ->
    let sub reg1 reg2 =
      (if (Array.get regs reg1) > (Array.get regs reg2) then
        Array.set regs 0xF 1
      else
        Array.set regs 0xF 0);
      Array.set regs reg1 ((Array.get regs reg2) - (Array.get regs reg1))
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00F0) / 0x0010)
  (* 0x8XYE Left shift on VX; puts output bit in VF *)
  | ins when (ins land 0xF00F) = 0x800E ->
    let sub reg1 =
      Array.set regs 0xF (((Array.get regs reg1) * 2) / 0x100);
      Array.set regs reg1 ((Array.get regs reg1) * 2);
      Array.set regs reg1 ((Array.get regs reg1) land 0xFF)
    in
      sub ((ins land 0x0F00) / 0x0100)
  (* 0x9XY0 Goes next instruction if VX <> VY *)
  | ins when (ins land 0xF000) = 0x9000 ->
    let sub reg1 reg2 =
      if (Array.get regs reg1) <> (Array.get regs reg2) then
        pc := !pc + 2
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00F0) / 0x0010)
  (* 0xANNN Loads NNN in I *)
  | ins when (ins land 0xF000) = 0xA000 ->
    reg_i := ins land 0x0FFF
  (* 0xBNNN Jumps at V0 + NNN *)
  | ins when (ins land 0xF000) = 0xB000 ->
    pc := (Array.get regs 0) + (ins land 0x0FFF)
  (* 0xCXKK Loads (Random & KK) in VX *)
  | ins when (ins land 0xF000) = 0xC000 ->
    let sub reg max =
      Random.self_init ();
      Array.set regs reg (Random.bits () land max)
    in
      sub ((ins land 0x0F00) / 0x0100) (ins land 0x00FF)
  (* 0xDXYN Draw sprite (see CHIP8 doc for more explanations) *)
  | ins when (ins land 0xF000) = 0xD000 ->
    let sub reg1 reg2 height =
      draw_sprite (Array.get regs reg1) (Array.get regs reg2) !reg_i height 0;
      Screen.sync ()
    in
      sub ((ins land 0x0F00) / 0x0100) ((ins land 0x00F0) / 0x0010) (ins land 0x000F)
  (* 0xEX9E Skips next instruction if key stored in VX is pressed*)
  (* TODO *)
  (* 0xEXA1 Skips next instruction if key stored in VX is not pressed *)
  (* TODO *)
  (* 0xFX0A Loads key pressed in VX *)
  | ins when (ins land 0xF0FF) = 0xF00A ->
    let sub reg =
      Array.set regs reg (Char.code (Graphics.read_key ()))
    in
      sub ((ins land 0x0F00) / 0x100)
  (* 0xFX1E Loads VX + I in I *)
  | ins when (ins land 0xF0FF) = 0xF01E ->
    let sub reg =
      reg_i := ((Array.get regs reg) + !reg_i);
      reg_i := (!reg_i land 0xFF)
    in
      sub ((ins land 0x0F00) / 0x100)
  (* 0xFX29 Loads the VX's character adress in I *)
  | ins when (ins land 0xF0FF) = 0xF029 ->
    reg_i := (Array.get regs ((ins land 0x0F00) / 0x100)) * 5
  (* 0xFX33 Stores BCD representation of VX at I, I+1, I+2 *)
  | ins when (ins land 0xF0FF) = 0xF033 ->
    let sub reg =
      Memory.set !reg_i ((Array.get regs reg) / 100);
      Memory.set (!reg_i + 1) ((Array.get regs reg) / 10);
      Memory.set (!reg_i + 2) (Array.get regs reg)
    in
      sub ((ins land 0x0F00) / 0x100)
  (* 0xFX55 Loads V0 .. VF with I ... I + 0xF *)
  | ins when (ins land 0xF0FF) = 0xF055 ->
    for i = 0 to 0xF do
      begin
        Array.set regs i (!reg_i + i)
      end
    done
  (* 0xFX65 Loads V0 .. VF with value at I ... I + 0xF *)
  | ins when (ins land 0xF0FF) = 0xF065 ->
    for i = 0 to 0xF do
      begin
        Array.set regs i (Memory.get (!reg_i + i))
      end
    done
  | _ -> print_string "Unknown instruction\n"

let run () =
  let pc = ref 0x200
  in
  while true do
    begin
      execute (fetch pc) pc
    end
  done
