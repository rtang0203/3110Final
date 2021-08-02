MODULES=dwt eval io lexer parser pretty serialize types state import export game_types
OBJECTS=$(MODULES:=.cmo)
GAME=rml_game.byte rml_interpreter.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build

build:
	$(OCAMLBUILD) $(OBJECTS) $(GAME)

all: build

clean:
	ocamlbuild -clean
