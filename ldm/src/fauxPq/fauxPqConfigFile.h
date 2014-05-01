/*
 *   Copyright 1994, University Corporation for Atmospheric Research
 *   See top level COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: fauxPqConfigFile.h,v 1.1.20.1 2008/04/15 16:34:07 steve Exp $ */

#ifndef _CONFIG_FILE_H
#define _CONFIG_FILE_H

#include <stdlib.h>
#include "pq.h"

#ifdef __cplusplus 
extern "C" {
#endif

int cfOpen(const char* pathname);

int cfGetProduct(
    pq_match          mt,
    const prod_class_t* clss,
    prod_info*        info,
    void**            data,
    void**            prod,
    size_t*           len);

void cfFreeProduct(
    prod_info*        info,
    void**            data,
    void**            prod);

void cfDelay();

void cfClose();

#ifdef __cplusplus 
}
#endif

#endif /* !_CONFIG_FILE_H */
