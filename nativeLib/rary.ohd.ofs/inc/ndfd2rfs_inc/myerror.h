/*****************************************************************************
 * myerror.h
 *
 * DESCRIPTION
 *    This file contains the code to handle error messages.  Instead of simply
 * printing the error to stdio, it allocates some memory and stores the
 * message in it.  This is so that one can pass the error message back to
 * Tcl/Tk or another GUI program when there is no stdio.
 *    In addition a version of sprintf is provided which allocates memory for
 * the calling routine, so that one doesn't have to guess the maximum bounds
 * of the message.
 *
 * HISTORY
 *    9/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef MYERROR_H
#define MYERROR_H

#ifdef MEMWATCH
  #include "memwatch.h"
#endif

void mallocSprintf (char **Ptr, char *fmt, ...);

void reallocSprintf (char **Ptr, char *fmt, ...);

/* If fmt == NULL return buffer and reset, otherwise add. */
/* You are responsible for free'ing the result of errSprintf(NULL). */
char * errSprintf (char *fmt, ...);

void preErrSprintf (char *fmt, ...);

#endif
