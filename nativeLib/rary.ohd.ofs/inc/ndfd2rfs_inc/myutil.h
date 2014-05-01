/*****************************************************************************
 * myutil.c
 *
 * DESCRIPTION
 *    This file contains some simple utility functions.
 *
 * HISTORY
 *   12/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef MYUTIL_H
#define MYUTIL_H

#include <stdio.h>
#include <time.h>

int reallocFGets (char **Ptr, int *LenBuff, FILE *fp);

double myRound (double data, signed char place);

void strTrim (char *str);

void strToUpper (char *str);

int GetIndexFromStr (char *arg, char **Opt, int *Index);

int myParseTime (char *is, time_t * AnsTime);

#endif
