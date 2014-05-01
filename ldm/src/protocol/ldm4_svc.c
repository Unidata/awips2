#include "ldmconfig.h"

#include <rpc/rpc.h>

#include "inetutil.h"
#include "ulog.h"

#include "ldm4.h"


/* $Id: ldm4_svc.c,v 5.7.18.1 2005/01/21 21:33:42 steve Exp $ */

/*
 * Handles incoming LDM-4 RPC NULLPROC requests (only).  This method is 
 * directly and repeatedly invoked by the RPC layer after svc_run(3NSL) is 
 * invoked.
 *
 * rqstp        The RPC request.
 * transp       The server-side RPC transport.
 */
void
ldmprog_4(struct svc_req *rqstp, register SVCXPRT *transp)
{
    if (rqstp->rq_proc == 0) {                          /* NULLPROC */
        unotice("ldmprog_4: ldmping from %s",
            hostbyaddr((struct sockaddr_in*)svc_getcaller(transp)));
        (void)svc_sendreply(transp, (xdrproc_t) xdr_void, (char *)NULL);
    }
    else {
        svcerr_noproc(transp);
    }
}
