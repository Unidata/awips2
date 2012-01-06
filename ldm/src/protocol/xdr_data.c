/*
 *   Copyright 2005, University Corporation for Atmospheric Research
 *   See file ../COPYRIGHT for copying and redistribution conditions.
 */
/* $Id: xdr_data.c,v 1.1.2.1.10.2 2005/09/27 16:40:59 steve Exp $ */

/*
 * This module attempts to increase the performance of the LDM by
 * minimizing the use of malloc(3) when XDR-decoding product-data.
 */

#include <stdlib.h>

#include "ulog.h"

#include "xdr_data.h"


static char*    buf = NULL;
static size_t   max = 0;
static size_t   used = 0;


/*
 * Returns a memory buffer of desired size.  A subsequent call to 
 * xd_getNextSegment() will return the same pointer.
 *
 * Arguments:
 *      size    The amount of memory, in bytes, needed.
 * Returns:
 *      NULL    Failure.
 *      else    Pointer to buffer for the data.
 */
void*
xd_getBuffer(size_t size)
{
    if (NULL == buf || size > max) {
        free(buf);

        buf = malloc(size);

        if (NULL != buf)
            max = size;
    }

    used = 0;

    return buf;
}


/*
 * Returns a pointer into the memory buffer for a data-segment of a desired
 * size.  Pointers returned by successive calls advance through contiguous space
 * until either xd_getBuffer() or xd_reset() is called.
 *
 * Arguments:
 *      size    The amount of memory, in bytes, needed.
 * Returns:
 *      NULL    Failure.
 *      else    Pointer to buffer for the data.
 */
void*
xd_getNextSegment(size_t size)
{
    void*               next;
    const size_t        newMax = used + size;

    if (NULL != buf && newMax <= max) {
        next = buf + used;
        used += size;
    }
    else {
        char*   newBuf = realloc(buf, newMax);

        if (NULL == newBuf) {
            next = NULL;
        }
        else {
            max = newMax;
            buf = newBuf;
            next = buf + used;
            used += size;
        }
    }

    return next;
}


/*
 * Resets this module.  A subsequent call to xd_getNextSegment() will return
 * a pointer to the start of the buffer.
 */
void
xd_reset()
{
    used = 0;
}
