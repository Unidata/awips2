/*
 * This module contains RPC helper functions.
 */

#include "ldmconfig.h"

#include <rpc/rpc.h>
#include <arpa/inet.h>

#include <rpc/pmap_clnt.h>
#include <rpc/pmap_prot.h>
#include <string.h>
#include <unistd.h>

#include "rpcutil.h"
#include "inetutil.h"
#include "ulog.h"


/*
 * Print reply error info
 *
 * This is derived from RPC 4.0 source.  It's here because at least one
 * implementation of the RPC function clnt_sperror() results in a segmentation
 * violation (SunOS 5.8).
 */
char*
clnt_errmsg(CLIENT* clnt)
{
    struct       rpc_err e;
    static char  buf[512];
    char        *str = buf;

    clnt_geterr(clnt, &e);

    (void) strcpy(str, clnt_sperrno(e.re_status));  
    str += strlen(str);

    switch (e.re_status) {
        case RPC_SUCCESS:
        case RPC_CANTENCODEARGS:
        case RPC_CANTDECODERES:
        case RPC_TIMEDOUT:     
        case RPC_PROGUNAVAIL:
        case RPC_PROCUNAVAIL:
        case RPC_CANTDECODEARGS:
        case RPC_SYSTEMERROR:
        case RPC_UNKNOWNHOST:
        case RPC_UNKNOWNPROTO:
        case RPC_PMAPFAILURE:
        case RPC_PROGNOTREGISTERED:
        case RPC_FAILED:
                break;

        case RPC_CANTSEND:
        case RPC_CANTRECV:
            (void) sprintf(str, "; errno = %s", strerror(e.re_errno)); 
            str += strlen(str);
            break;

        case RPC_VERSMISMATCH:
            (void) sprintf(str, "; low version = %lu, high version = %lu", 
                e.re_vers.low, e.re_vers.high);
            str += strlen(str);
            break;

        case RPC_AUTHERROR:
            (void) sprintf(str,"; why = ");
            str += strlen(str);
            (void) sprintf(str, "(authentication error %d)",
                (int) e.re_why);
            str += strlen(str);
            break;

        case RPC_PROGVERSMISMATCH:
            (void) sprintf(str, "; low version = %lu, high version = %lu", 
                e.re_vers.low, e.re_vers.high);
            str += strlen(str);
            break;

        default:        /* unknown */
            (void) sprintf(str, "; s1 = %lu, s2 = %lu", 
                    e.re_lb.s1, e.re_lb.s2);
            str += strlen(str);
            break;
    }

    (void) sprintf(str, "\n");

    return buf;
}


/*
 * Indicate whether or not the portmapper daemon is running on the local host.
 *
 * Returns:
 *  -1          Error.  errno set.
 *   0          Portmapper daemon is NOT running on the local host.
 *   1          Portmapper daemon is running on the local host.
 */
int
local_portmapper_running()
{
    static int status;
    static int cached = 0;

    if (!cached) {
        struct sockaddr_in addr;

        if (local_sockaddr_in(&addr)) {
            status = -1;
        }
        else {
            CLIENT*            client;
            int                socket = RPC_ANYSOCK;

            addr.sin_port = (short)htons((short)PMAPPORT);

            if ((client = clnttcp_create(&addr, PMAPPROG,
                    PMAPVERS, &socket, 50, 500)) == NULL) {
                status = 0;

                unotice("local_portmapper_running(): clnttcp_create() "
                    "failure: %s", clnt_spcreateerror(""));
                uinfo("Portmapper daemon is not running on local host");
            }
            else {
                status = 1;

                auth_destroy(client->cl_auth);
                clnt_destroy(client);
                (void)close(socket);
            }
        }

        cached = 1;
    }

    return status;
}
