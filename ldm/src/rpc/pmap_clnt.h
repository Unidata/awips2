/* @(#)pmap_clnt.h	2.1 88/07/29 4.0 RPCSRC; from 1.11 88/02/08 SMI */
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

#ifndef __PMAP_CLNT_H__
#define __PMAP_CLNT_H__

/*
 * pmap_clnt.h
 * Supplies C routines to get to portmap services.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

#include "pmap_prot.h"		/* for pmaplist */

/*
 * Usage:
 *	success = pmap_set(program, version, protocol, port);
 *	success = pmap_unset(program, version);
 *	port = pmap_getport(address, program, version, protocol);
 *	head = pmap_getmaps(address);
 *	clnt_stat = pmap_rmtcall(address, program, version, procedure,
 *		xdrargs, argsp, xdrres, resp, tout, port_ptr)
 *		(works for udp only.) 
 * 	clnt_stat = clnt_broadcast(program, version, procedure,
 *		xdrargs, argsp,	xdrres, resp, eachresult)
 *		(like pmap_rmtcall, except the call is broadcasted to all
 *		locally connected nets.  For each valid response received,
 *		the procedure eachresult is called.  Its form is:
 *	done = eachresult(resp, raddr)
 *		bool_t done;
 *		char* resp;
 *		struct sockaddr_in raddr;
 *		where resp points to the results of the call and raddr is the
 *		address if the responder to the broadcast.
 */

typedef bool_t (*resultproc_t)(void*, struct sockaddr_in*);

#define pmap_set	my_pmap_set
extern bool_t		pmap_set(
	unsigned long program,
	unsigned long version,
	int protocol,
	unsigned short port);
#define pmap_unset	my_pmap_unset
extern bool_t		pmap_unset(unsigned long program, unsigned long version);
#define pmap_getmaps	my_pmap_getmaps
extern struct pmaplist	*pmap_getmaps(struct sockaddr_in *address);
#define pmap_rmtcall	my_pmap_rmtcall
extern enum clnt_stat	pmap_rmtcall(
	struct sockaddr_in *addr,
	unsigned long prog,
	unsigned long vers,
	unsigned long proc,
	xdrproc_t xdrargs,
	char* argsp,
	xdrproc_t xdrres,
	char* resp,
	struct timeval tout,
	unsigned long *port_ptr);
#define clnt_broadcast	my_clnt_broadcast
extern enum clnt_stat	clnt_broadcast(
	unsigned long		prog,		/* program number */
	unsigned long		vers,		/* version number */
	unsigned long		proc,		/* procedure number */
	xdrproc_t	xargs,		/* xdr routine for args */
	char*		argsp,		/* pointer to args */
	xdrproc_t	xresults,	/* xdr routine for results */
	char*		resultsp,	/* pointer to results */
	resultproc_t	eachresult);	/* call with each result obtained */
#define pmap_getport	my_pmap_getport
extern unsigned short		pmap_getport(
	struct sockaddr_in *address,
	unsigned long program,
	unsigned long version,
	unsigned protocol);
#define getrpcport	my_getrpcport
extern int			getrpcport(
	char*		host,
	unsigned long	prognum,
	unsigned long	versnum,
	unsigned	proto);

#endif
