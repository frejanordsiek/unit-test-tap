dnl  Configuration script for unit-test-tap that produces configure with
dnl  the help of autoconf.

define(UNITTESTTAP_CONFIGURE_COPYRIGHT,[[

unit-test-tap - scheme unit testing framework with TAP output
Copyright (C) 2016 Freja Nordsiek

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

]])

AC_INIT([unit-test-tap], [0.1], [Freja Nordsiek <fnordsie at gmail dt com>])

AC_CONFIG_MACRO_DIR([m4])
m4_include([m4/guile.m4])
m4_include([m4/guile_additional.m4])

AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([gnu dist-xz -Wall -Werror])

AC_COPYRIGHT(UNITTESTTAP_CONFIGURE_COPYRIGHT)

dnl make the target guile version settable
AC_ARG_WITH([guile],
AS_HELP_STRING([--with-guile=X.Y], [Guile effective version to target @<:@default=2.2@:>@. If not found, it will look for 2.2 and then 2.0.]),
[],[with_guile=2.2])
AC_MSG_CHECKING([for target Guile effective version (--with-guile)])
AC_MSG_RESULT([$with_guile])

dnl Conditionally compile scheme files to go files
AC_ARG_ENABLE([compile_scm],
AS_HELP_STRING([--enable-compile-scm], [compile .scm files to .go files @<:@default=yes@:>@]),
[case "${enableval}" in
  yes) compile_scm=yes ;;
  no)  compile_scm=no ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-compile-scm]) ;;
esac],[compile_scm=yes])
AM_CONDITIONAL([COMPILE_SCM], [test x$compile_scm = xyes])
AC_MSG_CHECKING([for whether to compile scheme files to go files (--enable-compile-scm)])
AC_MSG_RESULT([$compile_scm])


AC_PROG_INSTALL
AC_PROG_AWK
AC_PROG_SED


GUILE_PKG([$with_guile 2.2 2.0])
GUILE_PROGS

dnl get the site dir for scheme and go files.
GUILE_SITE_SCM_DIR
GUILE_SITE_GO_DIR

dnl Check all required modules.

GUILE_MODULE_REQUIRED([rnrs base])
GUILE_MODULE_REQUIRED([rnrs control])
GUILE_MODULE_REQUIRED([rnrs lists])
GUILE_MODULE_REQUIRED([rnrs io ports])
GUILE_MODULE_REQUIRED([rnrs io simple])
GUILE_MODULE_REQUIRED([rnrs exceptions])
GUILE_MODULE_REQUIRED([rnrs programs])





AC_CONFIG_FILES([Makefile])
AC_REQUIRE_AUX_FILE([tap-driver.sh])
AC_OUTPUT
