
let _ =
  Screen.init ();
  Memory.set_char_map ();
  FileLoader.load_file Sys.argv.(1);
  CPU.run ()
