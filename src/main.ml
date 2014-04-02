
let _ =
  Screen.init ();
  FileLoader.load_file Sys.argv.(1);
  CPU.run ()
