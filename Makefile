## ----------------------------------------------------------------------------
##
## Copyright (c) 2005 - 2012 Nebularis.
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
## IN THE SOFTWARE.
## ----------------------------------------------------------------------------
LOGLEVEL ?= 0
REBAR=$(shell which rebar)
SOURCE_DIR=src
TEST_DIR=test
EBIN_DIR=ebin
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl) 
TEST_SOURCES=$(wildcard $(TEST_DIR)/*.erl)
BIN_FILE=priv/bin/systest

MAIN_TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES))
TEST_TARGETS=$(patsubst $(TEST_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(TEST_SOURCES))

DEPS=$(shell erl -noshell -eval '[io:format("~p~n", [element(1, D)]) || D <- proplists:get_value(deps, element(2, file:consult("rebar.config")))], halt(0).').

## rules start here

ifeq ($(REBAR), '')
REBAR=bin/rebar/rebar
endif

.PHONY: all
all: clean test

.PHONY: info
info:
	$(info SysTest $(shell git describe --abbrev=0))
	$(info $(shell rebar -V))
	$(info 3rd Party Dependencies: ${DEPS})

.PHONY: clean
clean:
	$(REBAR) skip_deps=true clean -v $(LOGLEVEL)

.PHONY: dist-clean
dist-clean: clean
	rm -drf deps
	rm -drf bin

$(DEPS): $(REBAR)
	$(REBAR) get-deps -v $(LOGLEVEL)

$(MAIN_TARGETS): $(SOURCES) $(INCLUDES)
	$(REBAR) compile -v $(LOGLEVEL)

$(TEST_TARGETS): $(TEST_SOURCES)
	$(REBAR) skip_deps=true -C test.config compile -v $(LOGLEVEL)

$(BIN_FILE): info
	ERL_FLAGS="-pa ebin" $(REBAR) skip_deps=true clean compile escriptize -v $(LOGLEVEL)

.PHONY: test
test: test-default test-error-handling

.PHONY: test-dependencies
test-dependencies: info $(MAIN_TARGETS) $(TEST_TARGETS)

.PHONY: test-default
test-default: $(BIN_FILE) test-dependencies
	ERL_FLAGS="-pa ebin" SYSTEST_PROFILE="$@" priv/bin/systest

.PHONY: test-error-handling
test-error-handling: test-dependencies
	ERL_FLAGS="-pa ebin" SYSTEST_PROFILE="$@" \
		$(REBAR) skip_deps=true systest -v $(LOGLEVEL)

bin/%:
	mkdir -p deps
	mkdir -p bin
	git clone -b systest https://github.com/hyperthunk/$*.git deps/$*
	PATH="bin:${PATH}" $(MAKE) -C deps/$*
	cp deps/$*/$* bin/$*
