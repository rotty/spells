# Process this file with autoconf to produce a configure script.
AC_INIT(spells, 0.0.1, a.rottmann@gmx.at)

AC_CANONICAL_TARGET
AC_SUBST(TARGET, $target)
AC_SUBST(TARGET_CPU, $target_cpu)
AC_SUBST(TARGET_VENDOR, $target_vendor)
AC_SUBST(TARGET_OS, $target_os)

AM_INIT_AUTOMAKE

#
# Check type sizes
#
AC_CHECK_SIZEOF(short)
AC_SUBST(SIZEOF_SHORT, $ac_cv_sizeof_short)

AC_CHECK_SIZEOF(int)
AC_SUBST(SIZEOF_INT, $ac_cv_sizeof_int)

AC_CHECK_SIZEOF(long)
AC_SUBST(SIZEOF_LONG, $ac_cv_sizeof_long)

AC_CHECK_SIZEOF(long long)
AC_SUBST(SIZEOF_LONG_LONG, $ac_cv_sizeof_long_long)

AC_CHECK_SIZEOF(void *)
AC_SUBST(SIZEOF_POINTER, $ac_cv_sizeof_void_p)

AC_CHECK_SIZEOF(float)
AC_SUBST(SIZEOF_FLOAT, $ac_cv_sizeof_float)
AC_CHECK_SIZEOF(double)
AC_SUBST(SIZEOF_DOUBLE, $ac_cv_sizeof_double)

AC_CHECK_TYPE(size_t, [AC_CHECK_SIZEOF(size_t)
                       AC_SUBST(SIZEOF_SIZE_T, $ac_cv_sizeof_size_t)],
		      [AC_SUBST(SIZEOF_SIZE_T, [\#f])],)

AC_CONFIG_FILES(Makefile scheme/foreign/config.sls)
AC_OUTPUT
