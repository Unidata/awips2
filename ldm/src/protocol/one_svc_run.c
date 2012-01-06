/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: one_svc_run.c,v 5.7.18.10 2007/02/22 19:43:16 steve Exp $ */

/* 
 * ldm server mainline program module
 */

#include "ldmconfig.h"

#include <errno.h>
#include <rpc/rpc.h>   /* svc_fdset */
#include <signal.h>    /* sig_atomic_t */
#include <string.h>
#include <sys/time.h>  /* fd_set */

#include "log.h"

#include "ldm.h"        /* one_svc_run() */
#include "autoshift.h"  /* asTimeToShift() */
#include "timestamp.h"
#include "globals.h"


/*
 * Like svc_run(3RPC) except that it runs on one socket until one of the 
 * following occurs:
 *   1) The socket is closed;
 *   2) The timeout expires without any activity;
 *   4) as_shouldSwitch() returns true.
 *
 * This function uses the "log" module to accumulate messages.
 *
 * Arguments:
 *      xp_sock         The connected socket.
 *      inactive_timeo  The maximum amount of time to wait with no activity on
 *                      the socket in seconds.
 *
 * Returns:
 *      0               Success.  as_shouldSwitch() is true.
 *      EBADF           The socket isn't open.
 *      EINVAL          Invalid timeout value.
 *      ECONNRESET      RPC layer closed socket.  The RPC layer also
 *                      destroyed the associated SVCXPRT structure; therefore,
 *                      that object must not be subsequently dereferenced.
 *      ETIMEDOUT       "inactive_timeo" time passed without any activity on 
 *                      the socket.
 */ 
int
one_svc_run(
    const int                   xp_sock,
    const unsigned              inactive_timeo) 
{
    timestampt                  timeout;
    timestampt                  stimeo;
    fd_set                      fds;
    int                         retCode;

    timeout.tv_sec = inactive_timeo;
    timeout.tv_usec = 0;
    stimeo = timeout;

    FD_ZERO(&fds);
    FD_SET(xp_sock, &fds);

    for (;;) {
        fd_set          readFds = fds;
        int             width = xp_sock + 1;
        timestampt      before;
        int             selectStatus;

        (void)set_timestamp(&before);

        selectStatus = select(width, &readFds, 0, 0, &stimeo);

        (void)exitIfDone(0);

        if (selectStatus == 0) {
            retCode = ETIMEDOUT;
            break;
        }

        if (selectStatus > 0) {
            /*
             * The socket is ready for reading.
             */
            svc_getreqsock(xp_sock);    /* process socket input */

            (void)exitIfDone(0);

            if (!FD_ISSET(xp_sock, &svc_fdset)) {
                /*
                 * The RPC layer closed the socket and destroyed the associated
                 * SVCXPRT structure.
                 */
                 log_add("one_svc_run(): RPC layer closed connection");
                 retCode = ECONNRESET;
                 break;
            }

            stimeo = timeout;   /* reset select(2) timeout */

            if (as_shouldSwitch()) {
                retCode = 0;
                break;
            }
        }
        else {
            if (errno != EINTR) {
                log_errno();
                log_add("one_svc_run(): select() error on socket %d", xp_sock);
                retCode = errno;
                break;
            }

            {
                timestampt      after;
                timestampt      diff;

                (void)set_timestamp(&after);

                /*
                 * Adjust select(2) timeout.
                 */
                diff = diff_timestamp(&after, &before);
                stimeo = diff_timestamp(&timeout, &diff);
            }
        } 
    }                                   /* indefinite loop */

    return retCode;
}
