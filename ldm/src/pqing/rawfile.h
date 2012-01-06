/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: rawfile.h,v 1.10 1996/09/06 21:24:25 davis Exp $ */

#ifndef _RAWFILE_H_
#define _RAWFILE_H_

#include <stddef.h>

extern int open_rawfile(const char *rawfname);

extern void write_rawfile(size_t nbytes, char *buf);

#endif /* !_RAWFILE_H_ */
