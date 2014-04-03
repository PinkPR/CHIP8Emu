let load_file file =
  let f = Unix.openfile file [] 0o777 and
  i = ref 0x200 and
  complete = ref false and
  c = ref 1 and
  ins = ref 0 and
  s = String.create 1
  in
    while !c <> 0 do
      begin
        c := Unix.read f s 0 1;
        if !c <> 0 then
          begin
            ins := !ins + (Char.code (String.get s 0));
            if !complete then
              begin
                Memory.set !i !ins;
                ins := 0;
                complete := false;
                i := !i + 1
              end
            else
              begin
                ins := Char.code (String.get s 0);
                ins := !ins * 0x100;
                complete := true
              end
          end
      end
    done
