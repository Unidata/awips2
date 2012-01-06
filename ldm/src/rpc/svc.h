/* @(#)svc.h	2.2 88/07/29 4.0 RPCSRC; from 1.20 88/02/08 SMI */
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

/*
 * svc.h, Server-side remote procedure call interface.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

#ifndef __SVC_HEADER__
#define __SVC_HEADER__

#include "types.h"
#include "rpc_msg.h"	/* struct rpc_msg */

#include <sys/time.h>	/* FD_SETSIZE */

/*
 * This interface must manage two items concerning remote procedure calling:
 *
 * 1) An arbitrary number of transport connections upon which rpc requests
 * are received.  The two most notable transports are TCP and UDP;  they are
 * created and registered by routines in svc_tcp.c and svc_udp.c, respectively;
 * they in turn call xprt_register and xprt_unregister.
 *
 * 2) An arbitrary number of locally registered services.  Services are
 * described by the following four data: program number, version number,
 * "service dispatch" function, a transport handle, and a boolean that
 * indicates whether or not the exported program should be registered with a
 * local binder service;  if true the program's number and version and the
 * port number from the transport handle are registered with the binder.
 * These data are registered with the rpc svc system via svc_register.
 *
 * A service's dispatch function is called whenever an rpc request comes in
 * on a transport.  The request's program and version numbers must match
 * those of the registered service.  The dispatch function is passed two
 * parameters, struct svc_req * and SVCXPRT *, defined below.
 */

enum xprt_stat {
	XPRT_DIED,
	XPRT_MOREREQS,
	XPRT_IDLE
};

/*
 * Server side transport handle
 */
typedef struct SVCXPRT	SVCXPRT;
struct SVCXPRT {
	int		xp_sock;
	unsigned short		xp_port;	 /* associated port number */
	struct xp_ops {
	    /* receive incomming requests */
	    bool_t	(*xp_recv)(SVCXPRT *xprt, struct rpc_msg *msg);

	    /* get transport status */
	    enum xprt_stat (*xp_stat)(SVCXPRT *xprt);

	    /* get arguments */
	    bool_t	(*xp_getargs)(
		SVCXPRT *xprt,
		xdrproc_t xdr_args,
		char* args_ptr);

	    /* send reply */
	    bool_t	(*xp_reply)(SVCXPRT *xprt, struct rpc_msg *msg);

	    /* free mem allocated for args */
	    bool_t	(*xp_freeargs)(
		SVCXPRT *xprt,
		xdrproc_t xdr_args,
		char* args_ptr);

	    /* destroy this struct */
	    void	(*xp_destroy)(SVCXPRT *xprt);
	} *xp_ops;
	int		xp_addrlen;	 /* length of remote address */
	struct sockaddr_in xp_raddr;	 /* remote address */
	struct opaque_auth xp_verf;	 /* raw response verifier */
	char*		xp_p1;		 /* private */
	char*		xp_p2;		 /* private */
};

/*
 *  Approved way of getting address of caller
 */
#define svc_getcaller(x) (&(x)->xp_raddr)

/*
 * Operations defined on an SVCXPRT handle
 *
 * SVCXPRT		*xprt;
 * struct rpc_msg	*msg;
 * xdrproc_t		 xargs;
 * char*		 argsp;
 */
#define SVC_RECV(xprt, msg)				\
	(*(xprt)->xp_ops->xp_recv)((xprt), (msg))
#define svc_recv(xprt, msg)				\
	(*(xprt)->xp_ops->xp_recv)((xprt), (msg))

#define SVC_STAT(xprt)					\
	(*(xprt)->xp_ops->xp_stat)(xprt)
#define svc_stat(xprt)					\
	(*(xprt)->xp_ops->xp_stat)(xprt)

#define SVC_GETARGS(xprt, xargs, argsp)			\
	(*(xprt)->xp_ops->xp_getargs)((xprt), (xdrproc_t)(xargs), (argsp))
#define svc_getargs(xprt, xargs, argsp)			\
	(*(xprt)->xp_ops->xp_getargs)((xprt), (xdrproc_t)(xargs), (argsp))

#define SVC_REPLY(xprt, msg)				\
	(*(xprt)->xp_ops->xp_reply) ((xprt), (msg))
#define svc_reply(xprt, msg)				\
	(*(xprt)->xp_ops->xp_reply) ((xprt), (msg))

#define SVC_FREEARGS(xprt, xargs, argsp)		\
	(*(xprt)->xp_ops->xp_freeargs)((xprt), (xdrproc_t)(xargs), (argsp))
#define svc_freeargs(xprt, xargs, argsp)		\
	(*(xprt)->xp_ops->xp_freeargs)((xprt), (xdrproc_t)(xargs), (argsp))

#define SVC_DESTROY(xprt)				\
	(*(xprt)->xp_ops->xp_destroy)(xprt)
#define svc_destroy(xprt)				\
	(*(xprt)->xp_ops->xp_destroy)(xprt)


/*
 * Service request
 */
struct svc_req {
	unsigned long		rq_prog;	/* service program number */
	unsigned long		rq_vers;	/* service protocol version */
	unsigned long		rq_proc;	/* the desired procedure */
	struct opaque_auth rq_cred;	/* raw creds from the wire */
	char*		rq_clntcred;	/* read only cooked cred */
	SVCXPRT	*rq_xprt;		/* associated transport */
};


/*
 * Service registration
 *
 * svc_register(xprt, prog, vers, dispatch, protocol)
 *	SVCXPRT *xprt;
 *	unsigned long prog;
 *	unsigned long vers;
 *	void (*dispatch)();
 *	int protocol;  like TCP or UDP, zero means do not register 
 */
#define svc_register	my_svc_register
extern bool_t	svc_register(
	SVCXPRT *xprt,
	unsigned long prog,
	unsigned long vers,
	void (*dispatch)(),
	int protocol);

/*
 * Service un-registration
 *
 * svc_unregister(prog, vers)
 *	unsigned long prog;
 *	unsigned long vers;
 */
#define svc_unregister	my_svc_unregister
extern void	svc_unregister(unsigned long prog, unsigned long vers);

/*
 * Transport registration.
 *
 * xprt_register(xprt)
 *	SVCXPRT *xprt;
 */
#define xprt_register	my_xprt_register
extern void	xprt_register(SVCXPRT *xprt);

/*
 * Transport un-register
 *
 * xprt_unregister(xprt)
 *	SVCXPRT *xprt;
 */
#define xprt_unregister	my_xprt_unregister
extern void	xprt_unregister(SVCXPRT *xprt);




/*
 * When the service routine is called, it must first check to see if it
 * knows about the procedure;  if not, it should call svcerr_noproc
 * and return.  If so, it should deserialize its arguments via 
 * SVC_GETARGS (defined above).  If the deserialization does not work,
 * svcerr_decode should be called followed by a return.  Successful
 * decoding of the arguments should be followed the execution of the
 * procedure's code and a call to svc_sendreply.
 *
 * Also, if the service refuses to execute the procedure due to too-
 * weak authentication parameters, svcerr_weakauth should be called.
 * Note: do not confuse access-control failure with weak authentication!
 *
 * NB: In pure implementations of rpc, the caller always waits for a reply
 * msg.  This message is sent when svc_sendreply is called.  
 * Therefore pure service implementations should always call
 * svc_sendreply even if the function logically returns void;  use
 * xdr.h - xdr_void for the xdr routine.  HOWEVER, tcp based rpc allows
 * for the abuse of pure rpc via batched calling or pipelining.  In the
 * case of a batched call, svc_sendreply should NOT be called since
 * this would send a return message, which is what batching tries to avoid.
 * It is the service/protocol writer's responsibility to know which calls are
 * batched and which are not.  Warning: responding to batch calls may
 * deadlock the caller and server processes!
 */

#define svc_sendreply	my_svc_sendreply
extern bool_t	svc_sendreply(
	register SVCXPRT *xprt,
	xdrproc_t xdr_results,
	char* xdr_location);
#define svcerr_decode	my_svcerr_decode
extern void	svcerr_decode(SVCXPRT *xprt);
#define svcerr_weakauth	my_svcerr_weakauth
extern void	svcerr_weakauth(SVCXPRT *xprt);
#define svcerr_noproc	my_svcerr_noproc
extern void	svcerr_noproc(SVCXPRT *xprt);
#define svcerr_progvers	my_svcerr_progvers
extern void	svcerr_progvers(
	SVCXPRT *xprt, 
	unsigned long low_vers,
	unsigned long high_vers);
#define svcerr_auth	my_svcerr_auth
extern void	svcerr_auth(SVCXPRT *xprt, enum auth_stat why);
#define svcerr_noprog	my_svcerr_noprog
extern void	svcerr_noprog(SVCXPRT *xprt);
#define svcerr_systemerr	my_svcerr_systemerr
extern void	svcerr_systemerr(SVCXPRT *xprt);
    
/*
 * Lowest level dispatching -OR- who owns this process anyway.
 * Somebody has to wait for incoming requests and then call the correct
 * service routine.  The routine svc_run does infinite waiting; i.e.,
 * svc_run never returns.
 * Since another (co-existant) package may wish to selectively wait for
 * incoming calls or other events outside of the rpc architecture, the
 * routine svc_getreq is provided.  It must be passed readfds, the
 * "in-place" results of a select system call (see select, section 2).
 */

/*
 * Global keeper of rpc service descriptors in use
 * dynamic; must be inspected before each call to select 
 */
#ifdef FD_SETSIZE
/*
 * NOTE: The symbol is redefined so that it is not overridden
 * by a symbol with the same name in a sharable-library of the
 * operating-system.
 */
#define svc_fdset	my_svc_fdset
extern fd_set svc_fdset;
#define svc_fds svc_fdset.fds_bits[0]	/* compatibility */
#else
#define svc_fds		my_svc_fds
extern int svc_fds;
#endif /* def FD_SETSIZE */

/*
 * a small program implemented by the svc_rpc implementation itself;
 * also see clnt.h for protocol numbers.
 */
#define rpctest_service	my_rpctest_service
extern void rpctest_service();

#define svc_getreq	my_svc_getreq
extern void	svc_getreq(int rdfds);
#define svc_getreqset	my_svc_getreqset
extern void	svc_getreqset(
#ifdef FD_SETSIZE
	fd_set *readfds);
#else
	int *readfds);
#endif
/*
 * The following routine was added by me and is not in the RPC specification
 * -- SRE 2005-04-01
 */
extern void	svc_getreqsock(int sock);
#define svc_run	my_svc_run
extern void	svc_run(void); 	 /* never returns */

/*
 * Socket to use on svcxxx_create call to get default socket
 */
#define	RPC_ANYSOCK	-1

/*
 * These are the existing service side transport implementations
 */

/*
 * Memory based rpc for testing and timing.
 */
#define svcraw_create	my_svcraw_create
extern SVCXPRT *svcraw_create(void);

/*
 * Udp based rpc.
 */
#define svcudp_create	my_svcudp_create
extern SVCXPRT *svcudp_create(int sock);
#define svcudp_bufcreate	my_svcudp_bufcreate
extern SVCXPRT *svcudp_bufcreate(
	int sock,
	unsigned sendsz,
	unsigned recvsz);

/*
 * Tcp based rpc.
 */
#define svctcp_create	my_svctcp_create
extern SVCXPRT *svctcp_create(
	int sock,
	unsigned sendsize,
	unsigned recvsize);
#define svcfd_create	my_svcfd_create
extern SVCXPRT *svcfd_create(
	int fd,
	unsigned sendsize,
	unsigned recvsize);

#define registerrpc	my_registerrpc
extern int	registerrpc(
	int prognum,
	int versnum,
	int procnum,
	char *(*progname)(),
	xdrproc_t inproc,
	xdrproc_t outproc);

#define svcudp_enablecache	my_svcudp_enablecache
extern int svcudp_enablecache(
	SVCXPRT *transp,
	unsigned long size);

#define _svcauth_short	my__svcauth_short
extern enum auth_stat _svcauth_short(
    struct svc_req*	rqst,
    struct rpc_msg*	msg);

#define _svcauth_unix	my__svcauth_unix
extern enum auth_stat _svcauth_unix(
    struct svc_req* rqst,
    struct rpc_msg* msg);

#define _rpc_dtablesize	my__rpc_dtablesize
extern int _rpc_dtablesize(void);


#endif /* !__SVC_HEADER__ */
