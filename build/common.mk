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
SYSTEST_EXE ?= $(abspath ./priv/bin/systest)
SYSTEST_REQUIRES ?=
SYSTEST_DEPENDS ?=$(SYSTEST_EXE) $(SYSTEST_REQUIRES)
SYSTEST_DEPS ?= $(abspath $(SYSTEST_EXE)/../../../deps)
SYSTEST_PROFILE ?=
SYSTEST_ERL_LIBS ?= $(shell echo $$ERL_LIBS)
MY_ERL_LIBS="${SYSTEST_DEPS}:${SYSTEST_ERL_LIBS}"
SYSTEST_ERL_FLAGS ?= -pa ebin -pa .test
SYSTEST_ENV ?= 
SYSTEST_COVER ?= --cover-dir=.test
SYSTEST_DUMP ?= 'false'

ifneq ($(SYSTEST_LOGGING), 'false')
OPERATOR_LOG=-L operator
SYSTEST_QUIET=
else
OPERATOR_LOG=
endif

ifneq ($(SYSTEST_VERBOSE), 'false')
SYSTEM_LOG=-L framework -L sut -L process
SYSTEST_QUIET=
else
SYSTEM_LOG=
endif

ifneq ($(SYSTEST_DUMP), 'false')
DUMP=-X
else
DUMP=
endif

ifneq (,$(findstring s,$(MAKEFLAGS)))
SYSTEST_QUIET=-q
SYSTEM_LOG=
OPERATOR_LOG=
DUMP=
else
SYSTEST_QUIET=
endif

NOISE=$(SYSTEST_QUIET) $(SYSTEM_LOG) $(OPERATOR_LOG) $(DUMP)

ifneq ($(SYSTEST_NO_COVER), 'false')
COVER=-w
else
COVER=$(SYSTEST_COVER)
endif

define systest
	ERL_LIBS=$(MY_ERL_LIBS) \
	ERL_FLAGS="${SYSTEST_ERL_FLAGS}" \
	$(SYSTEST_ENV) $(SYSTEST_EXE) $(if $(1),-a $(1) -P $(1),) $(NOISE) $(COVER) $(2)
endef

.PHONY: test-profile
ifneq ($(SYSTEST_PROFILE), '')
test-profile: $(SYSTEST_DEPENDS)
	$(call systest,$(SYSTEST_PROFILE),$(SYSTEST_FLAGS))
else
test-profile:
	$(error you need to specify a SYSTEST_PROFILE to run this target)
endif

.PHONY: test-shell
ifneq ($(SYSTEST_TARGET), '')
test-shell: $(SYSTEST_DEPENDS)
	$(call systest,$(SYSTEST_PROFILE),-s -z $(SYSTEST_TARGET) $(SYSTEST_FLAGS))
else
test-shell:
	$(error, you need to specify a SYSTEST_TARGET to run this target)
endif
