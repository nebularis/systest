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
NO_COVER ?= 'false'
SOURCE_DIR=src
TEST_DIR=test
EBIN_DIR=ebin
DEPS=$(shell erl -noshell -eval '[io:format("~p~n", [element(1, D)]) || D <- proplists:get_value(deps, element(2, file:consult("rebar.config")))], halt(0).')
LATEST_STABLE=$(shell git log stable --oneline -1 --format="%h")
SYSTEST_PROFILE ?= ''

## rules start here

REBAR=bin/rebar

ifneq ($(VERBOSE), 'false')
NOISE=-L framework -L operator -L sut -L process
else
NOISE=
endif

ifneq ($(NO_COVER), 'false')
COVER=-w
else
COVER=--cover-dir=.test
endif

define systest
	ERL_LIBS="deps:${ERL_LIBS}" \
	ERL_FLAGS="-pa ebin -pa .test" \
		priv/bin/systest -a $(1) -P $(1) $(NOISE) $(COVER) $(2)
endef

.PHONY: all
all: escriptize

.PHONY: info
info: $(REBAR)
	$(info SysTest $(shell git describe --abbrev=0 ${LATEST_STABLE}))
	$(info $(shell $(REBAR) -V))
	$(info 3rd Party Dependencies: ${DEPS})

.PHONY: clean
clean:
	$(REBAR) skip_deps=true clean -v $(LOGLEVEL)

.PHONY: doc
doc:
	$(REBAR) skip_deps=true doc -v $(LOGLEVEL)

.PHONY: publish-wiki
publish-wiki:
	$(REBAR) skip_deps=true publish-wiki -v $(LOGLEVEL)

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
	    $(REBAR) skip_deps=true escriptize -v $(LOGLEVEL)

.PHONY: verify
verify:
	ERL_FLAGS="-pa ebin" \
	    $(REBAR) skip_deps=true clean compile xref -v 4

.PHONY: eunit
eunit:
	$(REBAR) skip_deps=true -C test.config eunit -v $(LOGLEVEL)

.PHONY: test-compile
test-compile: $(REBAR)
	$(REBAR) skip_deps=true -C test.config test-compile -v $(LOGLEVEL)

.PHONY: test
test: escriptize eunit test-default test-error-handling test-time-traps

.PHONY: test-dependencies
test-dependencies: escriptize test-compile

.PHONY: test-default
test-default: test-dependencies
	$(call systest,$@,$(FLAGS))

.PHONY: test-error-handling
test-error-handling: test-dependencies
	$(call systest,$@,-c $(FLAGS))

.PHONY: test-time-traps
test-time-traps: test-dependencies
	$(call systest,test-error-handling,-Z systest_error_handling_SUITE:timetrap_failure -ci)

.PHONY: test-profile
ifneq ($(SYSTEST_PROFILE), '')
test-profile: test-dependencies
	$(call systest,$(SYSTEST_PROFILE),$(FLAGS))
else
test-profile:
	$(error you need to specify a SYSTEST_PROFILE to run this target)
endif

bin/%:
	mkdir -p deps
	mkdir -p bin
	git clone -b systest https://github.com/hyperthunk/$*.git deps/$*
	PATH="bin:${PATH}" $(MAKE) -C deps/$*
	cp deps/$*/$* bin/$*
