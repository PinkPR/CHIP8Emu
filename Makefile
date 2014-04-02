SC=   Memory.ml   \
      FileLoader.ml \
      Screen.ml   \
      CPU.ml      \
      ISet.ml     \
      main.ml

SRC=$(addprefix src/, $(SC))

TARGET= chip8emu

$(TARGET): $(SRC)
	cd src; ocamlc unix.cma graphics.cma $(SC) -o ../$(TARGET)

clean:
	rm -rf src/*.cm? $(TARGET)

.PHONY: clean
