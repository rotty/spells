#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="spells"

(test -f $srcdir/configure.ac \
  && test -f $srcdir/autogen.sh) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level $PKG_NAME directory"
    exit 1
}

aclocal
automake --add-missing
autoconf

./configure  --enable-maintainer-mode "$@"
