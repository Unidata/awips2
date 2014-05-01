/*
 *   Copyright 2007, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: state.h,v 1.1.2.1 2007/01/30 22:19:05 steve Exp $ */

#ifndef STATE_H_INCLUDED
#define STATE_H_INCLUDED

#include "timestamp.h"

#ifdef __cplusplus
extern "C" {
#endif

int
stateInit(
    const char* const		configPathname);

int
stateRead(
    timestampt* const		pqCursor);

int
stateWrite(
    const timestampt* const	pqCursor);

#ifdef __cplusplus
}
#endif

#endif
