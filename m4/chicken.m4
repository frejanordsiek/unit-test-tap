## Autoconf macros for working with Chicken

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
## CHICKEN_PROGS -- set paths to csi, csc, chicken, chicken-install,
##   chicken-uninstall, chicken-status, and chicken-profile
## CHICKEN_EXTENSION_REQUIRED -- fail if Chicken extension not available


# CHICKEN_PROGS -- set paths to the various Chicken programs
#
# Usage: CHICKEN_PROGS
#
# Finds the paths to sevaral chicken programs. The following
# variable-program pairs are set.
#
# An error is produced if program chicken cannot be found.
#
# CHICKEN_CSI -- csi
# CHICKEN_CSC -- csc
# CHICKEN -- chicken
# CHICKEN_INSTALL -- chicken-install
# CHICKEN_UNINSTALL -- chicken-uninstall
# CHICKEN_STATUS -- chicken-status
# CHICKEN_PROFILE -- -chicken-profile
AC_DEFUN([CHICKEN_PROGS],
 [AC_MSG_NOTICE([checking for Chicken])
  AC_PATH_PROG(CHICKEN, [chicken])
  if test "$CHICKEN" = "" ; then
    AC_MSG_FAILURE([could not find Chicken])
  fi
  AC_PATH_PROG(CHICKEN_CSI, [csi])
  AC_PATH_PROG(CHICKEN_CSC, [csc])
  AC_PATH_PROG(CHICKEN_INSTALL, [chicken-install])
  AC_PATH_PROG(CHICKEN_UNINSTALL, [chicken-uninstall])
  AC_PATH_PROG(CHICKEN_STATUS, [chicken-status])
  AC_PATH_PROG(CHICKEN_PROFILE, [chicken-profile])
 ])


# CHICKEN_EXTENSION_REQUIRED -- fail if Chicken extension not available
#
# Usage: CHICKEN_EXTENSION_REQUIRED(name)
#
# Looks for the given extension and fails if it is not found.
AC_DEFUN([CHICKEN_EXTENSION_REQUIRED],
 [AC_REQUIRE([CHICKEN_PROGS])
  AC_MSG_CHECKING([checking for the chicken extension $1])
  if test "$CHICKEN_CSI" = "" ; then
    AC_MSG_FAILURE([the chicken interpreter csi is required])
  fi
  _junk=`$CHICKEN_CSI -R $1 -q -b -w -e "(display \"yes\")"`
  if test $? != "0" ; then
    _junk=no
  fi
  AC_MSG_RESULT([$_junk])
  if test "$_junk" = "no" ; then
      AC_MSG_FAILURE([Chicken extension $1 not available])
  fi
 ])
