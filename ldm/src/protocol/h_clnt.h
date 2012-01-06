/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *
 *  Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of UCAR/Unidata not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  UCAR makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.  It is
 * provided with no support and without obligation on the part of UCAR
 * Unidata, to assist in its use, correction, modification, or enhancement.
 *
 */
/* $Id: h_clnt.h,v 1.74.18.1 2005/06/24 21:05:20 steve Exp $ */

/* 
 * 
 * Interface
 */
#ifndef _H_CLNT_H
#define _H_CLNT_H

#include <sys/time.h> /* struct timeval */
#include <rpc/rpc.h>  /* CLIENT */
#ifndef IPPROTO_IP
#include <netinet/in.h> /* struct sockaddr_in */
#endif

#ifndef HOSTNAMESIZE
#define HOSTNAMESIZE 64
#endif


/*
 * This is the control point for whether the
 * clnt_nbtcp package gets used for
 * tcp clnts.
 */
#ifdef CLNT_NBTCP
#define LDM_NBTCP (IPPROTO_MAX + IPPROTO_TCP)
#define LDM_TCP	LDM_NBTCP
#else
#define LDM_TCP	IPPROTO_TCP
#endif


/*
 * In h_clnt_call, set the timeout param to this
 * to indicate that you wish to use the 
 * h_timeo timeout in the h_clnt struct.
 */
#define USE_H_TIMEO ((unsigned int)-1)

/*
 * Don't set any alarms and set selects to
 * alrm.h TIMEO_TV_INF
 */
#define TIMEO_INF	0

/*
 * Representation of the state of a remote service
 * from the client side.
 */
typedef enum {
	H_NONE = 0,   /* ground state (empty) */
	NAMED,      /* initialized, a service is defined */
	ADDRESSED,  /* we got an network address (nameservice okay) */
	PMAP_CLNTED,	/* CLNT handle to remote portmap or rpcbind */
	MAPPED,  	/* and we got remote address (port) */
	H_CLNTED,  /* clnt side handle */
	RESPONDING  /* clnt is responding okay */
} remote_state;


/*
 * Similar in some ways to clnt_simple.c:callrpc_private,
 * and in others to the CLNT handle,
 * this structure caches info needed to as we climb
 * the steps to a sucessful call.
 */ 
typedef struct {
	remote_state state;
	/* the next four fields define the service which you want to monitor */
	char remote[HOSTNAMESIZE+1];
	unsigned long prog;
	unsigned long vers;
	unsigned int  prot;
	/* as the state moves up, these become valid */
	struct sockaddr_in addr; /* struct netbuf *svcaddr */
	CLIENT *pmap_clnt;
	struct rpc_err rpcerr;
	unsigned short port;
	CLIENT *clnt;
	/* timing info */
	unsigned int h_timeo;
	struct timeval begin;
	struct timeval elapsed;
	/* error message cache */
#define H_CLNT_ERRMSG_SIZE 256
	char errmsg[H_CLNT_ERRMSG_SIZE];
} h_clnt;

#define set_h_timeout(hcp, to)	((hcp)->h_timeo = (to))
#define get_h_timeout(hcp)	((hcp)->h_timeo)

#ifdef __cplusplus
extern "C" {
#endif

extern char *s_remote_state( remote_state state );

extern const char *s_hclnt_sperrno( h_clnt *hcp );

extern int
h_clntfileno(
	h_clnt *hcp );

char *
h_clnt_name(
	h_clnt *hcp);

extern void free_h_pmap( h_clnt *hcp );

extern void close_h_clnt( h_clnt *hcp );

extern void free_h_clnt(h_clnt *hcp );

extern remote_state init_h_clnt(
	h_clnt *hcp,
	const char *remote,
	unsigned long program,
	unsigned long version,
	unsigned int protocol );

extern h_clnt *new_h_clnt(
	const char *remote,
	unsigned long program,
	unsigned long version,
	unsigned int protocol );

extern enum clnt_stat h_clnt_call(
	h_clnt *hcp,
	u_long proc,
	xdrproc_t xargs,
	caddr_t argsp,
	xdrproc_t xres,
	caddr_t resp,
	unsigned int timeout );

extern h_clnt *open_h_clnt(
	const char *remote,
	unsigned long program,
	unsigned long version,
	unsigned int protocol,
	unsigned int timeout);

extern remote_state
h_xprt_turn(h_clnt *hcp,
	const char *remote,
	SVCXPRT *xprt,
	u_int sendsz,
	u_int recvsz);

extern int
h_clnt_flush(
	h_clnt *hcp,
	bool_t block,
	unsigned int timeo,
	enum clnt_stat *rpc_statp);

enum clnt_stat
h_callrpc(
	char *remote,
	u_long program,
	u_long version,
	u_long proc,
	xdrproc_t xargs,
	caddr_t argsp,
	xdrproc_t xres,
	caddr_t resp,
	unsigned int timeout);

#ifdef __cplusplus
}
#endif

#endif /* !_H_CLNT_H */
