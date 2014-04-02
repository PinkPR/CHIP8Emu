let load_file file =
  let f = Unix.openfile file [] 0o777 and
  i = ref 0 and
  c = ref 1 and
  s = String.create 1
  in
    while !c <> 0 do
    begin
      c := Unix.read f s 0 1;
      (if !c = 0 then
        begin
          Memory.set !i (Char.code (String.get s 0));
          i := !i + 1
        end);
    end
    done
