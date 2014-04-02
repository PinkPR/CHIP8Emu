SRC=  Memory.ml   \
      CPU.ml      \
      ISet.ml     \
      Screen.ml   \
      main.ml

lol: $(SRC)
	ocamlc graphics.cma $(SRC) -o lol
