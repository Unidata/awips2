/*
 *   Copyright 2003, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: xdr_data.h,v 1.1.2.1.10.1 2005/09/27 16:40:59 steve Exp $ */

#ifndef _XDR_DATA_H
#define	__XDR_DATA_H

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

void*	xd_getBuffer(size_t size);
void*	xd_getNextSegment(size_t size);
void	xd_reset();

#ifdef __cplusplus
}
#endif

#endif
