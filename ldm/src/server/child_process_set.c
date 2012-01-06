/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: child_process_set.c,v 1.5.18.1 2009/07/22 21:14:39 steve Exp $ */

/* 
 * This module implements a set of child process identifiers -- so the LDM
 * can respond to IS_ALIVE inquiries.
 */

#include "ldmconfig.h"

#include <errno.h>
#include <search.h>
#include <stdlib.h>     /* malloc(), free() */
#include <sys/types.h>

#include "ulog.h"

#include "child_process_set.h"


static void*    root = NULL;
static unsigned count = 0;


static int compare(
    const void *elt1,
    const void *elt2)
{
    const pid_t pid1 = *(const pid_t*)elt1;
    const pid_t pid2 = *(const pid_t*)elt2;

    return
        pid1 > pid2
            ? 1
            : pid1 == pid2
                ?  0
                : -1;
}


/*
 * @return 0      if successful.
 * @return ENOMEM if out-of-memory.
 */
int
cps_add(
    pid_t pid)
{
    int    error;

    if (cps_contains(pid)) {
        error = 0;
    }
    else {
        pid_t *elt;

        elt = (pid_t*)malloc(sizeof(pid_t));

        if (elt == NULL) {
            error = ENOMEM;
        }
        else {
            *elt = pid;

            if (tsearch(elt, &root, compare) == NULL) {
                error = ENOMEM;
            }
            else {
                ++count;
                error = 0;
            }
        }
    }

    return error;
}


void
cps_remove(
    pid_t pid)
{
    if (root != NULL) {
        void  *node = tfind(&pid, &root, compare);

        if (node != NULL) {
            pid_t *elt = *(pid_t**)node;

            (void)tdelete(&pid, &root, compare);
            free(elt);
            count--;
        }
    }
}


int
cps_contains(
    pid_t pid)
{
    return
        root == NULL
            ? 0
            : tfind(&pid, &root, compare) != NULL;
}


unsigned
cps_count()
{
    return count;
}
