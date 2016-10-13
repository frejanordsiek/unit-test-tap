## Additional autoconf macros for working with Guile


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
## GUILE_SITE_SCM_DIR -- find site dir for scheme files
## GUILE_SITE_GO_DIR -- find site dir for compiled go files
## GUILE_SITE_EXTENSION_DIR -- find dir for compiled extensions

# Uses the Guile function %site-dir.
#
# Depends on $GUILE being set
AC_DEFUN([GUILE_SITE_SCM_DIR],
 [AC_REQUIRE([GUILE_PROGS])
  AC_MSG_CHECKING([for Guile site directory for scheme files])
  GUILE_SITE_SCM=`$GUILE -c "(display (%site-dir))"`
  if test $? != "0" -o "$GUILE_SITE_SCM" = ""; then
    AC_MSG_FAILURE([sitedir for scheme files not found])
  fi
  AC_MSG_RESULT([$GUILE_SITE_SCM])
  AC_SUBST([GUILE_SITE_SCM])
 ])

# Uses the Guile function %site-ccache-dir.
#
# Depends on $GUILE being set
AC_DEFUN([GUILE_SITE_GO_DIR],
 [AC_REQUIRE([GUILE_PROGS])
  AC_MSG_CHECKING([for Guile site directory for compiled go files])
  GUILE_SITE_GO=`$GUILE -c "(display (%site-ccache-dir))"`
  if test $? != "0" -o "$GUILE_SITE_GO" = ""; then
    AC_MSG_FAILURE([sitedir for go files not found])
  fi
  AC_MSG_RESULT([$GUILE_SITE_GO])
  AC_SUBST([GUILE_SITE_GO])
 ])


# Uses the Guile expression (cdr (assoc 'extensiondir %guile-build-info ))
#
# Depends on $GUILE being set
AC_DEFUN([GUILE_SITE_EXTENSION_DIR],
 [AC_REQUIRE([GUILE_PROGS])
  AC_MSG_CHECKING([for Guile extension directory])
  GUILE_SITE_EXTENSION=`$GUILE -c "(display (cdr (assoc 'extensiondir %guile-build-info)))"`
  if test $? != "0" -o "$GUILE_SITE_EXTENSION" = ""; then
    AC_MSG_FAILURE([extension directory not found])
  fi
  AC_MSG_RESULT([$GUILE_SITE_EXTENSION])
  AC_SUBST([GUILE_SITE_EXTENSION])
 ])
