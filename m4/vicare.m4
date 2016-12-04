## Autoconf macros for working with Vicare

## unit-test-tap - scheme unit testing framework with TAP output
## Copyright (C) 2016 Freja Nordsiek
##
## This library is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public
## License as published by the Free Software Foundation; either
## version 2.1 of the License, or any later version.
##
## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public
## License along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA  02110-1301  USA

## Index
## -----
##
## VICARE_PROGS -- set paths to vicare
## VICARE_SITE_DIR -- find site dir for installed scheme and fasl files


# VICARE_PROGS -- set paths to vicare
#
# Usage: VICARE_PROGS
#
# Finds the path to vicare and puts it in the variable VICARE.
#
# An error is produced if program vicare cannot be found.
AC_DEFUN([VICARE_PROGS],
 [AC_MSG_NOTICE([checking for Vicare])
  AC_PATH_PROG(VICARE, [vicare])
  if test "$VICARE" = "" ; then
    AC_MSG_FAILURE([could not find Vicare])
  fi
 ])

# VICARE_SITE_DIR -- find site dir for installed scheme and fasl files
#
# Usage: VICARE_SITE_DIR
#
# Finds the site dir for installed scheme and fasl files and stores it
# in the variable VICARE_SITE.
#
# The scheme script vicare_site_dir.scm to be in $ac_aux_dir, which
# returns the directory, or throws an error if it fails. At the present
# time, it only works for Vicare >= 0.4d0pre5.
AC_DEFUN([VICARE_SITE_DIR],
 [AC_REQUIRE([VICARE_PROGS])
  AC_MSG_CHECKING([checking for the vicare site directory where libraries are installed])
  VICARE_SITE=`$VICARE --r6rs-script $ac_aux_dir/vicare_site_dir.scm`
  if test $? != "0" ; then
    AC_MSG_FAILURE([Vicare libary directory could not be found])
  fi
  AC_MSG_RESULT([$VICARE_SITE])
  AC_SUBST([VICARE_SITE])
 ])
