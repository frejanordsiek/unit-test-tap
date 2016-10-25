# unit-test-tap - scheme unit testing framework with TAP output
# Copyright (C) 2016 Freja Nordsiek
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA  02110-1301  USA


# AWK file to remove docstrings and guile module declaration and its
# associated command
STRIP_GUILE_AWK_FILE = build-aux/strip_guile_specific.awk
STRIP_GUILE = $(AWK) -f $(top_srcdir)/$(STRIP_GUILE_AWK_FILE)

# Install directories for scm and go files.

scmdir = $(GUILE_SITE_SCM)
godir = $(GUILE_SITE_GO)

# Need to make sure scheme files are installed before go files so
# that the go files are newer in the installation directores (otherwise
# they wont be used). This is done following the method described at
#
# http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html

guile_install_go_files = install-goDATA
$(guile_install_go_files): install-scmDATA


# GUILD compile flags.
GUILD_FLAGS = -Wunsupported-warning -Wunbound-variable -Warity-mismatch \
		-Wduplicate-case-datum -Wbad-case-datum -Wformat


# Sources and targets

BUILT_SOURCES = \
	src/r7rs/unit-test-tap.scm \
	src/r6rs/unit-test-tap.scm

SOURCES = src/unit-test-tap.scm

GOBJECTS = src/unit-test-tap.go


# Tests

TESTS = test/unit-testing.test
TESTSOURCES = $(TESTS:%.test=%.scm)

TEST_LOG_DRIVER = \
	env AM_TAP_AWK='$(AWK)' \
	$(SHELL) $(top_srcdir)/build-aux/tap-driver.sh --ignore-exit --comments


# Files to build and/or install

scm_DATA = $(SOURCES)
if COMPILE_SCM
go_DATA = $(GOBJECTS)
else
go_DATA =
endif

# Documentation

info_TEXINFOS = doc/unit-test-tap.texi


# Packaging and cleaning

EXTRA_DIST = $(SOURCES) $(TESTSOURCES) \
	$(STRIP_GUILE_AWK_FILE)

CLEANFILES = $(GOBJECTS) $(BUILT_SOURCES) \
	$(srcdir)/doc/unit-test-tap.info $(TESTS) \
MAINTAINERCLEANFILES = Makefile.in


# Build templates

src/r7rs/unit-test-tap.scm: src/unit-test-tap.scm src/r7rs/header.scm.in
	cp $(top_srcdir)/src/r7rs/header.scm.in $@
	$(STRIP_GUILE) $< | $(SED) -e 's/^/    /g' >> $@
	echo '    )' >> $@

src/r6rs/unit-test-tap.scm: src/unit-test-tap.scm src/r6rs/header.scm.in
	cp $(top_srcdir)/src/r6rs/header.scm.in $@
	$(STRIP_GUILE) $< | $(SED) -e 's/^/  /g' >> $@
	echo '  )' >> $@

SUFFIXES = .scm .go .test

.scm.go:
	$(GUILD) compile $(GUILD_FLAGS) -L $(top_srcdir)/src -o $@ $^

# shell scripts to run the respective scheme file through guile
.scm.test: src/unit-test-tap.scm
	echo '#!$(SHELL)' > $@
	echo "$(GUILE) --no-auto-compile -L $(top_srcdir)/src -s $^" >> $@
	chmod +x $@
