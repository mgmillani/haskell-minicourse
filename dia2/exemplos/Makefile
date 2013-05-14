HC = ghc
CFLAGS = -i$(SRC):$(MODULES) -odir $(OBJ) -hidir $(INTERFACE)
INTERFACE = interface
MODULES = modules
SRC = src
BIN = bin
OBJ = obj

%: $(SRC)/%.hs
	ghc --make $(CFLAGS) $< -o $(BIN)/$@.out

clean:
	rm -f $(OBJ)/*.o
	rm -f $(INTERFACE)/*.hi
	rm -f $(BIN)/*.out