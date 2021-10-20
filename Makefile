.PHONY: all clean ihaskell

BIN := bin
CONTAINER := programming-in-haskell

all: clean $(wildcard src/*.hs)

src/%.hs: $(BIN)
	@ghc \
		-O2 \
		-no-keep-o-files \
		-no-keep-hi-files \
		-outputdir $< \
		-o "$</$(basename $(@F))" \
		$@

$(BIN):
	@mkdir -p $@

clean:
	@rm -rf $(BIN)

ihaskell: TAG := latest
ihaskell:
	@docker run \
		--name $(CONTAINER) \
		--rm \
		-p 8888:8888 \
		-v $(PWD):/home/jovyan/src \
		gibiansky/ihaskell:$(TAG)
