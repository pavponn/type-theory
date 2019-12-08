SOURCES=src/Main.hs
GEN_SOURCES=src/Lex.x src/Parser.y
GENERATED=src/Lex.hs src/Parser.hs
PACKAGE=hw0.zip

.PHONY: all run clean

all: parser

run:
	./parser

clean:
	rm -rf  src/*.hi src/*.o $(GENERATED)

parser: $(GENERATED) $(SOURCES)
	ghc -i./src -tmpdir . ./src/Main.hs -XBangPatterns -O2 -o parser
	
$(GENERATED): $(GEN_SOURCES) $(SOURCES)
	alex src/Lex.x -o src/Lex.hs
	happy src/Parser.y -o src/Parser.hs


