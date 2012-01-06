// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// utilityFuncs.H
// Utility functions not bound to any class.
//
// Author: romberg
// ---------------------------------------------------------------------------

#ifndef utilityFuncs_H
#define utilityFuncs_H

#ifdef IDENT_H
static const char* const utilityFuncs_H_Id =
  "$Id: .utilityFuncs.H__temp21481,v 1.2 2003/05/28 15:12:52 fluke Exp $";
#endif

// On FreeBSD-2.2.7, timeb and the ftime() syscall are deprecated ...
// to use you also need to include sys/types.h
#include <sys/types.h>

#include <sys/timeb.h>


#ifdef __cplusplus
extern "C" {
#endif
/*
 * The version in HPs math library is broken so we use our own.
 */
#if defined(__hpux__) || defined(__hpux)
#define rint rint_
double rint_(double x);
#endif

/*
 * Returns the time and date specified in p, which can be in
 * almost any common format.  Returns -1 on error.
 *
 * Example calls:
 *  time_t t = get_date("tomorrow", NULL);
 *         t = get_date("Sep29 13:02", NULL);
 */
time_t get_date(const char *p, struct timeb *now);

/*
 * This implementation of the PWB library alloca function,
 * which is used to allocate space off the run-time stack so
 * that it is automatically reclaimed upon procedure exit
 */
#ifndef __GNUC__
void *alloca(unsigned int size);
#endif
#if __GNUC__ == 3 && (defined(__hpux__) || defined(__hpux))
#define alloca(size) __builtin_alloca (size)
#endif

/*
 * Daemonize a process
 */
void daemonize(void);

#ifdef __cplusplus
}
#endif

#endif
