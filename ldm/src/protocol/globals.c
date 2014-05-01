/*
 *   Copyright 2005, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: globals.c,v 1.1.2.3 2007/02/12 20:38:54 steve Exp $ */

#include "ldmconfig.h"

#include <rpc/rpc.h>  /* svc_req */
#include <stddef.h>
#include <signal.h>   /* sig_atomic_t */

#include "ldm.h"
#include "globals.h"
#include "ldm.h"
#include "paths.h"
#include "pq.h"

const char*             conf_path = DEFAULT_ACLPATHNAME;
volatile sig_atomic_t   done = 0;
const char*             logfname = 0;
const char*             pqfname = DEFAULT_QUEUE;
pqueue*                 pq = NULL;

/*
 * Timeout for rpc calls:
 */
unsigned int            rpctimeo = DEFAULT_RPCTIMEO;

/*
 * Time to sleep in pq_suspend() and before retrying connects.
 */
unsigned int            interval = 30;       

/*
 * Shut down a service connection that has been idle this long.
 * The keepalive timeout (for the other end) is
 * inactive_timeo/2 - 2 * interval;
 */
const int               inactive_timeo = 720;  /* 12 m */

/*
 * In requests, set 'from' to 'toffset' ago, and it may get
 * adjusted by  pq_clss_setfrom();
 */
int                     max_latency = DEFAULT_OLDEST;
int                     toffset = TOFFSET_NONE;

/*
 * Calls exit() if the "done" global variable is set; othewise, returns 1 so
 * that it can be easily used in programming loops.
 *
 * Arguments:
 *      status  Exit status for the call to exit().
 * Returns:
 *      1
 */
int
exitIfDone(
    const int   status)
{
    if (done)
        exit(status);

    return 1;
}
