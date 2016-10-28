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

dnl Give the options to allow targetting specific implementations.

dnl Target Guile
AC_ARG_WITH([guile],
AS_HELP_STRING([--with-guile], [target Guile @<:@default=no@:>@]),
[case "${withval}" in
  yes) with_chicken=yes ;;
  no)  with_chicken=no ;;
  *) AC_MSG_ERROR([bad value ${withval} for --with-chicken]) ;;
esac],[with_guile=no])
AC_MSG_CHECKING([for wheter to target Guile (--with-guile)])
AC_MSG_RESULT([$with_guile])
AM_CONDITIONAL([USE_GUILE], [test x$with_guile = xyes])

dnl Target a specific version of Guile
AC_ARG_WITH([guile-version],
AS_HELP_STRING([--with-guile-version=X.Y], [Guile effective version to target @<:@default=2.2@:>@ If not found, it will look for 2.2 and then 2.0.]),
[],[version_guile=2.2])
AC_MSG_CHECKING([for target Guile effective version (--with-guile-version)])
AC_MSG_RESULT([$version_guile])

dnl Conditionally Guile compile scheme files to go files
AC_ARG_ENABLE([guile-compile],
AS_HELP_STRING([--enable-guile-guile], [compile .scm files to .go files @<:@default=yes@:>@ for Guile]),
[case "${enableval}" in
  yes) compile_guile=yes ;;
  no)  compile_guile=no ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-guile-compile]) ;;
esac],[compile_guile=yes])
if test x$with_guile = xno ; then
  compile_guile=no
fi
AM_CONDITIONAL([COMPILE_GUILE], [test x$compile_guile = xyes])
AC_MSG_CHECKING([for whether to compile scheme files to go files for Guile (--enable-guile-compile)])
AC_MSG_RESULT([$compile_guile])

dnl Target chicken
AC_ARG_WITH([chicken],
AS_HELP_STRING([--with-chicken], [target Chicken @<:@default=no@:>@]),
[case "${withval}" in
  yes) with_chicken=yes ;;
  no)  with_chicken=no ;;
  *) AC_MSG_ERROR([bad value ${withval} for --with-chicken]) ;;
esac], [with_chicken=no])
AC_MSG_CHECKING([for whether to target Chicken (--with-chicken)])
AC_MSG_RESULT([$with_chicken])
AM_CONDITIONAL([USE_CHICKEN], [test x$with_chicken = xyes])


AC_PROG_INSTALL
AC_PROG_AWK
AC_PROG_SED


dnl Look for Guile if that is enabled.
if test x$with_guile = xyes ; then
  AC_MSG_NOTICE([checking for Guile with the required libraries])

  dnl Check for GUILE
  GUILE_PKG([$version_guile 2.2 2.0])
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
fi


dnl Look for Chicken with R7RS extension if that is enabled.
if test x$with_chicken = xyes ; then
  AC_MSG_NOTICE([checking for Chicken with R7RS])
  AC_PATH_PROG(CHICKEN, [chicken])
  AC_PATH_PROG(CHICKEN_CSI, [csi])
  if test "$CHICKEN" = "" -o "$CHICKEN_CSI" = "" ; then
    AC_MSG_FAILURE([could not find Chicken])
  fi
  AC_MSG_CHECKING([checking for the chicken r7rs extension])
  _junk=`$CHICKEN_CSI -R r7rs -q -b -w -e "(display \"yes\")"`
  if test $? != "0" ; then
    _junk=no
  fi
  AC_MSG_RESULT([$_junk])
  if test "$_junk" = "no" ; then
      AC_MSG_FAILURE([Chicken extension r7rs not available])
  fi
fi


AC_CONFIG_FILES([Makefile])
AC_REQUIRE_AUX_FILE([tap-driver.sh])
AC_OUTPUT
