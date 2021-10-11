.PHONY: clean ihaskell

CONTAINER := programming-in-haskell

ihaskell: TAG=latest
ihaskell:
	docker run \
		--name $(CONTAINER) \
		--rm \
		-p 8888:8888 \
		-v $(PWD):/home/jovyan/src \
		gibiansky/ihaskell:$(TAG)

clean:
	docker rm -f $(CONTAINER) 2> /dev/null
