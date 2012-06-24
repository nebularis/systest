
LOGLEVEL ?= 2
REBAR=$(shell which rebar)
SOURCE_DIR=src
TEST_DIR=test
EBIN_DIR=ebin
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl) 
TEST_SOURCES=$(wildcard $(TEST_DIR)/*.erl)

MAIN_TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES))
TEST_TARGETS=$(patsubst $(TEST_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(TEST_SOURCES))

DEPS=$(shell erl -noshell -eval '[io:format("~p~n", [element(1, D)]) || D <- proplists:get_value(deps, element(2, file:consult("rebar.config")))], halt(0).').

## rules start here

ifeq ($(REBAR), '')
REBAR=bin/rebar/rebar
endif

.PHONY: all
all: clean test

.PHONY: clean
clean:
	$(REBAR) skip_deps=true clean -v $(LOGLEVEL)

.PHONY: dist-clean
dist-clean:
	rm -drf deps
	rm -drf bin

$(DEPS): $(REBAR)
	$(REBAR) get-deps -v $(LOGLEVEL)

$(MAIN_TARGETS): $(SOURCES) $(INCLUDES)
	$(REBAR) compile -v $(LOGLEVEL)

$(TEST_TARGETS): $(TEST_SOURCES)
	$(REBAR) skip_deps=true -C test.config compile -v $(LOGLEVEL)

.PHONE: test
test: $(MAIN_TARGETS) $(TEST_TARGETS)
	ERL_FLAGS="-pa ebin" $(REBAR) skip_deps=true systest -v $(LOGLEVEL)

bin/%:
	mkdir -p deps
	mkdir -p bin
	git clone -b systest https://github.com/hyperthunk/$*.git deps/$*
	PATH="bin:${PATH}" $(MAKE) -C deps/$*
	cp deps/$*/$* bin/$*

