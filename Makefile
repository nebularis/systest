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
LOGLEVEL ?= 2
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
all: clean $(BIN_FILE)

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

$(BIN_FILE): test
	ERL_FLAGS="-pa ebin" $(REBAR) skip_deps=true escriptize -v $(LOGLEVEL)

.PHONY: test
test: $(MAIN_TARGETS) $(TEST_TARGETS)
	ERL_FLAGS="-pa ebin" $(REBAR) skip_deps=true systest -v $(LOGLEVEL)

bin/%:
	mkdir -p deps
	mkdir -p bin
	git clone -b systest https://github.com/hyperthunk/$*.git deps/$*
	PATH="bin:${PATH}" $(MAKE) -C deps/$*
	cp deps/$*/$* bin/$*

