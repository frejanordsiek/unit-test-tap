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
m4_include([m4/chicken.m4])
m4_include([m4/vicare.m4])

AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([subdir-objects info-in-builddir gnu dist-xz -Wall -Werror])

AC_COPYRIGHT(UNITTESTTAP_CONFIGURE_COPYRIGHT)

dnl Give the options to allow targetting specific implementations.

dnl Target Guile
AC_ARG_WITH([guile],
AS_HELP_STRING([--with-guile], [target Guile @<:@default=no@:>@]),
[case "${withval}" in
  yes) with_guile=yes ;;
  no)  with_guile=no ;;
  *) AC_MSG_ERROR([bad value ${withval} for --with-guile]) ;;
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

dnl Target Vicare
AC_ARG_WITH([vicare],
AS_HELP_STRING([--with-vicare], [target Vicare @<:@default=no@:>@]),
[case "${withval}" in
  yes) with_vicare=yes ;;
  no)  with_vicare=no ;;
  *) AC_MSG_ERROR([bad value ${withval} for --with-vicare]) ;;
esac], [with_vicare=no])
AC_MSG_CHECKING([for whether to target Vicare (--with-vicare)])
AC_MSG_RESULT([$with_vicare])
AM_CONDITIONAL([USE_VICARE], [test x$with_vicare = xyes])

AC_PROG_INSTALL
AC_PROG_AWK
AC_PROG_SED


dnl Look for Guile if that is enabled.
if test x$with_guile = xyes ; then
  AC_MSG_NOTICE([checking for Guile with the required libraries])

  dnl Check for GUILE
  GUILE_PKG([$version_guile 2.2 2.0])
  GUILE_PROGS
  GUILE_SITE_DIR

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
  CHICKEN_PROGS
  CHICKEN_EXTENSION_REQUIRED([r7rs])
fi

dnl Look for Vicare and find install directory
if test x$with_vicare = xyes ; then
  VICARE_PROGS
  VICARE_SITE_DIR
fi


AC_CONFIG_COMMANDS_POST([mkdir -p src src/r6rs src/r7rs \
                               src/vicare src/chicken \
                               test])

AC_CONFIG_FILES([Makefile])
AC_REQUIRE_AUX_FILE([tap-driver.sh])
AC_REQUIRE_AUX_FILE([vicare_site_dir.scm])
AC_OUTPUT
