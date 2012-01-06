/* @(#)svc_simple.c	2.2 88/08/01 4.0 RPCSRC */
/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)svc_simple.c 1.18 87/08/11 Copyr 1984 Sun Micro";
#endif

/* 
 * svc_simple.c
 * Simplified front end to rpc.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

#include "config.h"

#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <sys/socket.h>
#include "rpc.h"

extern bool_t pmap_unset(unsigned long program, unsigned long version);

static struct proglst {
	char *(*p_progname)(char*);
	int  p_prognum;
	int  p_procnum;
	xdrproc_t p_inproc, p_outproc;
	struct proglst *p_nxt;
} *proglst;
static void universal(struct svc_req *rqstp, SVCXPRT *xp);
static SVCXPRT *transp;
static struct proglst *pl;

int
registerrpc(
	int prognum,
	int versnum,
	int procnum,
	char *(*progname)(),
	xdrproc_t inproc,
	xdrproc_t outproc)
{
	
	if (procnum == NULLPROC) {
		(void) fprintf(stderr,
		    "can't reassign procedure number %lu\n", NULLPROC);
		return (-1);
	}
	if (transp == 0) {
		transp = svcudp_create(RPC_ANYSOCK);
		if (transp == NULL) {
			(void) fprintf(stderr, "couldn't create an rpc server\n");
			return (-1);
		}
	}
	(void) pmap_unset((unsigned long)prognum, (unsigned long)versnum);
	if (!svc_register(transp, (unsigned long)prognum, (unsigned long)versnum, 
	    universal, IPPROTO_UDP)) {
	    	(void) fprintf(stderr, "couldn't register prog %d vers %d\n",
		    prognum, versnum);
		return (-1);
	}
	pl = (struct proglst *)malloc(sizeof(struct proglst));
	if (pl == NULL) {
		(void) fprintf(stderr, "registerrpc: out of memory\n");
		return (-1);
	}
	pl->p_progname = progname;
	pl->p_prognum = prognum;
	pl->p_procnum = procnum;
	pl->p_inproc = inproc;
	pl->p_outproc = outproc;
	pl->p_nxt = proglst;
	proglst = pl;
	return (0);
}

static void
universal(
	struct svc_req *rqstp,
	SVCXPRT *xp)
{
	int prog, proc;
	char *outdata;
	char xdrbuf[UDPMSGSIZE];
	struct proglst *plp;

	/* 
	 * enforce "procnum 0 is echo" convention
	 */
	if (rqstp->rq_proc == NULLPROC) {
		if (svc_sendreply(xp, (xdrproc_t)xdr_void, (char *)NULL)
		    == FALSE) {
			(void) fprintf(stderr, "xxx\n");
			exit(1);
		}
		return;
	}
	prog = (int)rqstp->rq_prog;
	proc = (int)rqstp->rq_proc;
	for (plp = proglst; plp != NULL; plp = plp->p_nxt)
		if (plp->p_prognum == prog && plp->p_procnum == proc) {
			/* decode arguments into a CLEAN buffer */
			bzero(xdrbuf, sizeof(xdrbuf)); /* required ! */
			if (!svc_getargs(xp, plp->p_inproc, xdrbuf)) {
				svcerr_decode(xp);
				return;
			}
			outdata = (*(plp->p_progname))(xdrbuf);
			if (outdata == NULL && 
			    plp->p_outproc != (xdrproc_t)xdr_void)

				/* there was an error */
				return;
			if (!svc_sendreply(xp, plp->p_outproc, outdata)) {
				(void) fprintf(stderr,
				    "trouble replying to prog %d\n",
				    plp->p_prognum);
				exit(1);
			}
			/* free the decoded arguments */
			(void)svc_freeargs(xp, plp->p_inproc, xdrbuf);
			return;
		}
	(void) fprintf(stderr, "never registered prog %d\n", prog);
	exit(1);
}

