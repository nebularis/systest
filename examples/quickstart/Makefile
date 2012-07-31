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
TOPDIR_1=$(abspath ../../../)/
TOPDIR=$(abspath ../..)/
REBAR=$(addprefix $(TOPDIR),bin/rebar)
SYSTEST=$(addprefix $(TOPDIR),priv/bin/systest)
LOGLEVEL ?= 0
VERBOSE ?= 'false'

ifneq ($(VERBOSE), 'false')
NOISE=-L framework -L operator
else
NOISE=
endif

.PHONY: test-compile
test-compile: $(REBAR)
	ERL_LIBS="${TOPDIR_1}:${ERL_LIBS}" $(REBAR) compile -v $(LOGLEVEL)

.PHONY: test-compile
test: test-compile $(SYSTEST)
	ERL_LIBS="${TOPDIR_1}:${ERL_LIBS}" $(SYSTEST) $(NOISE)

$(REBAR): $(SYSTEST)

$(SYSTEST):
	make -C ${TOPDIR}
