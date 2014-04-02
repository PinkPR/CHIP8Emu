SC=   Memory.ml   \
      Screen.ml   \
      CPU.ml      \
      ISet.ml     \
      main.ml

SRC=$(addprefix src/, $(SC))

TARGET= chip8emu

$(TARGET): $(SRC)
	cd src; ocamlc graphics.cma $(SC) -o ../$(TARGET)

clean:
	rm -rf src/*.cm? $(TARGET)

.PHONY: clean
