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
SYSTEST_VERBOSE ?= 'false'
SYSTEST_NO_COVER ?= 'false'
SYSTEST_EXE ?= ./priv/bin/systest

ifneq ($(SYSTEST_VERBOSE), 'false')
NOISE=-L framework -L operator -L sut -L process
else
NOISE=
endif

ifneq ($(SYSTEST_NO_COVER), 'false')
COVER=-w
else
COVER=--cover-dir=.test
endif

define systest
	ERL_LIBS="deps:${ERL_LIBS}" \
	ERL_FLAGS="-pa ebin -pa .test" \
		$(SYSTEST_EXE) -a $(1) -P $(1) $(NOISE) $(COVER) $(2)
endef

.PHONY: test-profile
ifneq ($(SYSTEST_PROFILE), '')
test-profile: test-dependencies
	$(call systest,$(SYSTEST_PROFILE),$(SYSTEST_FLAGS))
else
test-profile:
	$(error you need to specify a SYSTEST_PROFILE to run this target)
endif

