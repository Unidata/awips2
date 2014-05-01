// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// utilityFuncs.C
// Utility functions not bound to any class.
//
// Author: romberg
// ---------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const utilityFuncs_C_Id =
  "$Id$";
#endif

#include "utilityFuncs.H"
#include <math.h>
#include <unistd.h>
#include <stdio.h>
#include <signal.h>
#include <sys/param.h>
#include <errno.h>
#include <sys/stat.h>
#include <stdlib.h>
#ifdef SIGTSTP
#include <sys/file.h>
#include <sys/ioctl.h>
#endif

// -- module -----------------------------------------------------------------
// Utility functions consist of several functions that are not bound
// to any particular class but yet are used by the support and other
// routines.
// ---------------------------------------------------------------------------

// -- global -----------------------------------------------------------------
// rint_()
// Rounds its argument to the nearest integer.  The version of this function
// that lives in the math library is broken so we will use this one.
// ---------------------------------------------------------------------------
double rint_(double x)
    {
    double c = ceil(x);
    double f = floor(x);

    if ( fabs(x - c) < fabs(x - f))
        return c;

    return f;
    }

// -- global -----------------------------------------------------------------
// daemonize()
//
// Daemonize a process.
// ---------------------------------------------------------------------------
// Based on function found in "Unix Network Programming" by Richard Stevens.
// ---------------------------------------------------------------------------
void daemonize(void)
    {
    // If not running in the background fork and exit the
    // parent which will put us in the background.  This makes
    // us the process group leader.
    if (fork() != 0)
        exit(0);

    setsid();

    if (fork() != 0)
        exit(0);

    // Close stdin out and error
    close(0);
    close(1);
    close(2);
    open("/dev/null", O_RDONLY);
    open("/dev/null", O_WRONLY);
    open("/dev/null", O_WRONLY);

    // Move to root "/"
    
    // Clear any inherited file mode creation mask.
    umask(0);
    }
