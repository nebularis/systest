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
VERBOSE ?= 'false'
REBAR=$(shell which rebar)
SOURCE_DIR=src
TEST_DIR=test
EBIN_DIR=ebin
DEPS=$(shell erl -noshell -eval '[io:format("~p~n", [element(1, D)]) || D <- proplists:get_value(deps, element(2, file:consult("rebar.config")))], halt(0).').

## rules start here

ifndef $(REBAR)
REBAR=bin/rebar
endif

ifneq ($(VERBOSE), 'false')
NOISE=-L framework -L operator
else
NOISE=
endif

.PHONY: all
all: escriptize

.PHONY: info
info: $(REBAR)
	$(info SysTest $(shell git describe --abbrev=0))
	$(info $(shell $(REBAR) -V))
	$(info 3rd Party Dependencies: ${DEPS})

.PHONY: clean
clean:
	$(REBAR) skip_deps=true clean -v $(LOGLEVEL)

.PHONY: dist-clean
dist-clean: clean
	rm -rf deps
	rm -rf bin
	rm -rf priv/bin/systest*

compile: $(REBAR)
	$(REBAR) get-deps compile -v $(LOGLEVEL)

.PHONY: escriptize
escriptize: compile
	ERL_FLAGS="-pa ebin" \
	    $(REBAR) skip_deps=true mv_test_beams escriptize -v $(LOGLEVEL)

.PHONY: verify
verify:
	ERL_FLAGS="-pa ebin" \
	    $(REBAR) skip_deps=true clean compile xref -v 4

.PHONY: eunit
eunit:
	rm -rf .eunit
	$(REBAR) skip_deps=true -C test.config eunit -v $(LOGLEVEL)

.PHONY: test-compile
test-compile: $(REBAR)
	$(REBAR) skip_deps=true -C test.config compile mv_test_beams -v $(LOGLEVEL)

.PHONY: test
test: eunit test-default test-error-handling

.PHONY: test-dependencies
test-dependencies: test-compile

.PHONY: test-default
test-default: test-dependencies
	ERL_FLAGS="-pa ebin -pa test-ebin" \
	    priv/bin/systest -P $@ $(NOISE)

.PHONY: test-error-handling
test-error-handling: test-dependencies
	ERL_FLAGS="-pa ebin -pa test-ebin" \
		priv/bin/systest -A -a error_test -P $@ $(NOISE)

bin/%:
	mkdir -p deps
	mkdir -p bin
	git clone -b systest https://github.com/hyperthunk/$*.git deps/$*
	PATH="bin:${PATH}" $(MAKE) -C deps/$*
	cp deps/$*/$* bin/$*
