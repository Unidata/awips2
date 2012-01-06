/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: feedTime.h,v 1.1.10.1.4.1 2008/04/15 16:34:13 steve Exp $ */

#ifndef FEED_TIME_H
#define FEED_TIME_H

#include "ldm.h"
#include "timestamp.h"

#ifdef __cplusplus
extern "C" {
#endif

int         ft_get_time(prod_class_t* prod_class);
void        ft_set_time(const timestampt* time);

#ifdef __cplusplus
}
#endif

#endif
