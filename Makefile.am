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

# Install directories for Guile scm and go, and Vicare sls and fasl
# files.

guilescmdir = $(GUILE_SITE_SCM)
guilegodir = $(GUILE_SITE_GO)
vicaredir = $(VICARE_SITE)

# Need to make sure scheme files are installed before go files so
# that the go files are newer in the installation directores (otherwise
# they wont be used). This is done following the method described at
#
# http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html

guile_install_go_files = install-guilegoDATA
$(guile_install_go_files): install-guilescmDATA


# GUILD compile flags.
GUILD_FLAGS = -Wunsupported-warning -Wunbound-variable -Warity-mismatch \
		-Wduplicate-case-datum -Wbad-case-datum -Wformat
VICARE_FLAGS = -O2

# Sources and targets

R6RS_TARGET = src/r6rs/unit-test-tap.scm
R7RS_TARGET = src/r7rs/unit-test-tap.scm

BUILT_SOURCES = $(R6RS_TARGET) $(R7RS_TARGET)

SOURCES = src/unit-test-tap.scm

GOBJECTS = src/unit-test-tap.go

VICARE_SLS_OBJECTS = src/r6rs/unit-test-tap.sls
VICARE_FASL_OBJECTS = src/r6rs/unit-test-tap.fasl
VICAREOBJECTS = $(VICARE_SLS_OBJECTS) $(VICARE_FASL_OBJECTS)


# Tests

R6RS_TEST_TARGET = test/tests-r6rs.scm
R7RS_TEST_TARGET = test/tests-r7rs.scm
R6RS_TESTSOURCES = test/tests-r6rs-header.scm test/tests-base.scm
R7RS_TESTSOURCES = test/tests-r7rs-header.scm test/tests-base.scm

check_DATA = $(R6RS_TEST_TARGET) $(R7RS_TEST_TARGET)

GUILE_TESTS = test/guile.test test/guile-r6rs.test
CHICKEN_TESTS = test/chicken-r7rs.test
VICARE_TESTS = test/vicare-r6rs.test
ALL_TESTS = $(GUILE_TESTS) $(CHICKEN_TESTS) $(VICARE_TESTS)

TESTS =
if USE_GUILE
TESTS += $(GUILE_TESTS)
endif
if USE_CHICKEN
TESTS += $(CHICKEN_TESTS)
endif
if USE_VICARE
TESTS += $(VICARE_TESTS)
endif

TEST_LOG_DRIVER = \
	env AM_TAP_AWK='$(AWK)' \
	$(SHELL) $(top_srcdir)/build-aux/tap-driver.sh --ignore-exit --comments


# Files to build and/or install

if USE_GUILE
guilescm_DATA = $(SOURCES)
guilego_DATA = $(GOBJECTS)
else
guilescm_DATA =
guilego_DATA =
endif

if USE_VICARE
vicare_DATA = $(VICAREOBJECTS)
else
vicare_DATA =
endif


# Documentation

info_TEXINFOS = doc/unit-test-tap.texi


# Packaging and cleaning

EXTRA_DIST = $(SOURCES) $(TESTSOURCES) \
	$(R6RS_TESTSOURCES) $(R7RS_TESTSOURCES) \
	$(STRIP_GUILE_AWK_FILE)

CLEANFILES =  $(BUILT_SOURCES) $(GOBJECTS) $(VICAREOBJECTS) \
	$(srcdir)/doc/unit-test-tap.info $(ALL_TESTS) \
	$(R6RS_TEST_TARGET) $(R7RS_TEST_TARGET)
MAINTAINERCLEANFILES = Makefile.in


# Build templates

# Built sources

src/r7rs/unit-test-tap.scm: $(SOURCES) src/r7rs/header.scm.in
	cp $(top_srcdir)/src/r7rs/header.scm.in $@
	$(STRIP_GUILE) $< | $(SED) -e 's/^/    /g' >> $@
	echo '    ))' >> $@

src/r6rs/unit-test-tap.scm: $(SOURCES) src/r6rs/header.scm.in
	cp $(top_srcdir)/src/r6rs/header.scm.in $@
	$(STRIP_GUILE) $< | $(SED) -e 's/^/  /g' >> $@
	echo '  )' >> $@

# Tests - scheme files

$(R6RS_TEST_TARGET): $(R6RS_TESTSOURCES)
	cat $^ > $@

$(R7RS_TEST_TARGET): $(R7RS_TESTSOURCES)
	cat $^ > $@

# Tests - shell scripts

test/guile.test: $(R6RS_TEST_TARGET) src/unit-test-tap.scm
	echo '#!$(SHELL)' > $@
	echo "$(GUILE) --no-auto-compile -L $(top_srcdir)/src -s $<" >> $@
	chmod +x $@

test/guile-r6rs.test: $(R6RS_TEST_TARGET) $(R6RS_TARGET)
	echo '#!$(SHELL)' > $@
	echo "$(GUILE) --no-auto-compile -L $(top_srcdir)/src/r6rs -s $<" >> $@
	chmod +x $@

test/chicken-r7rs.test: $(R7RS_TARGET) $(R7RS_TEST_TARGET)
	echo '#!$(SHELL)' > $@
	echo "$(CHICKEN_CSI) -R r7rs -b -w -q $^" >> $@
	chmod +x $@

test/vicare-r6rs.test: $(R6RS_TEST_TARGET) $(VICARE_FASL_OBJECTS)
	echo '#!$(SHELL)' > $@
	echo "$(VICARE) -L `dirname $(VICARE_FASL_OBJECTS)` --r6rs-script $<" >> $@
	chmod +x $@

SUFFIXES = .scm .go .sls .fasl

# Compile
.scm.go:
	$(GUILD) compile $(GUILD_FLAGS) -L $(top_srcdir)/src -o $@ $^

.sls.fasl:
	$(VICARE) $(VICARE_FLAGS) --compile-library $^ -o $@

.scm.sls:
	cp $^ $@

