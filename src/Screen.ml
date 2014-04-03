let bg_color = Graphics.black
let fg_color = Graphics.green

let xmul = 4
let ymul = 4

let sync () =
  Graphics.synchronize ()

let plot x y color =
  Graphics.set_color color;
  for i = 0 to xmul do
    for j = 0 to ymul do
      Graphics.plot (xmul * x + i) (ymul * y + j)
    done
  done

let clear () =
  Graphics.set_color bg_color;
  for i = 0 to 63 do
    for j = 0 to 31 do
      plot i j bg_color
    done
  done

let init () =
  Graphics.open_graph " 256x128";
  clear ()
