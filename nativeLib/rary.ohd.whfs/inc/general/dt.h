#ifndef DT_H
#define DT_H

#define UTIME_MINYEAR (1901)
#define UTIME_MINMONTH (12)
#define UTIME_MINDAY (14)
#define UTIME_MAXYEAR (2038)
#define UTIME_MAXMONTH (01)
#define UTIME_MAXDAY (18)
                                                                                                                                                                                 
#define IS_VALID_UTIME(y,m,d) ((((y) > UTIME_MINYEAR) \
 || (((y) == UTIME_MINYEAR) && (((m) > UTIME_MINMONTH) \
  || (((m) == UTIME_MINMONTH) && ((d) >= UTIME_MINDAY))))) \
 && (((y) < UTIME_MAXYEAR) \
 || (((y) == UTIME_MAXYEAR) && (((m) < UTIME_MAXMONTH) \
  || (((m) == UTIME_MAXMONTH) && ((d) <= UTIME_MAXDAY))))))

#ifdef HAVE_INT64_TIMESTAMP
                                                                                                                                                                                 
#define DT_NOBEGIN      (-INT64CONST(0x7fffffffffffffff) - 1)
#define DT_NOEND        (INT64CONST(0x7fffffffffffffff))
                                                                                                                                                                                 
#else
                                                                                                                                                                                 
#ifdef HUGE_VAL
#define DT_NOBEGIN      (-HUGE_VAL)
#define DT_NOEND        (HUGE_VAL)
#else
#define DT_NOBEGIN      (-DBL_MAX)
#define DT_NOEND        (DBL_MAX)
#endif
#endif   /* HAVE_INT64_TIMESTAMP */

/* TMODULO()
 * Like FMODULO(), but work on the timestamp datatype (either int64 or float8).
 * We assume that int64 follows the C99 semantics for division (negative
 * quotients truncate towards zero).
 */
#ifdef HAVE_INT64_TIMESTAMP
#define TMODULO(t,q,u) \
do { \
    q = (t / u); \
    if (q != 0) t -= (q * u); \
} while(0)
#else
#define TMODULO(t,q,u) \
do { \
    q = ((t < 0) ? ceil(t / u): floor(t / u)); \
    if (q != 0) t -= rint(q * u); \
} while(0)
#endif
                                                                                                                                                                                 
/*
 * Date/time validation
 * Include check for leap year.
 */
#define isleap(y) (((y) % 4) == 0 && (((y) % 100) != 0 || ((y) % 400) == 0))

#ifdef HAVE_INT64_TIMESTAMP
                                                                                                                                                                                 
typedef int32 fsec_t;
                                                                                                                                                                                 
#else
                                                                                                                                                                                 
typedef double fsec_t;
                                                                                                                                                                                 
#define TIME_PREC_INV 1000000.0
#define JROUND(j) (rint(((double) (j))*TIME_PREC_INV)/TIME_PREC_INV)
#endif
#define TIMESTAMP_NOBEGIN(j)    do {j = DT_NOBEGIN;} while (0)
#define TIMESTAMP_NOEND(j)          do {j = DT_NOEND;} while (0)
#define TIMESTAMP_IS_NOBEGIN(j) ((j) == DT_NOBEGIN)
#define TIMESTAMP_IS_NOEND(j)   ((j) == DT_NOEND)
#define TIMESTAMP_NOT_FINITE(j) (TIMESTAMP_IS_NOBEGIN(j) || TIMESTAMP_IS_NOEND(j))

#endif
