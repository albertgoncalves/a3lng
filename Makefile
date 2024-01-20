MAKEFLAGS += --silent
FLAGS = \
    -g \
    -nolabels \
    -strict-formats \
    -strict-sequence \
    -unboxed-types \
    -warn-error "+a"
MODULES = \
	ast \
	ir \
	compile \
	optimize \
	main
LINTS = $(foreach x,$(MODULES),build/$(x).ml)
SRCS = $(foreach x,$(MODULES),$(x).ml)

.PHONY: all
all: bin/main

.PHONY: clean
clean:
	rm -rf bin/
	rm -rf build/

.PHONY: run
run: all
	OCAMLRUNPARAM=b ./bin/main

$(LINTS): build/%.ml: src/%.ml
	mkdir -p build/
	cp $^ $@
	ocp-indent -i $@
	diff -q $^ $@ 2>&1 > /dev/null || cp $@ $^

bin/main: $(LINTS)
	mkdir -p bin/
	cd build/ && ocamlc $(FLAGS) $(SRCS) -o ../bin/main
