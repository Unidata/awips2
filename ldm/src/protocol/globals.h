#ifndef _LDM_SERVER_GLOBAL_H
#define _LDM_SERVER_GLOBAL_H
/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: globals.h,v 1.1.2.8 2009/07/24 16:04:26 steve Exp $ */

/*
 * Unless otherwise noted, globals are
 * declared (and initialized) in ldmd.c
 */

#include <rpc/rpc.h>  /* svc_req */
#include <signal.h>   /* sig_atomic_t */

extern const char *conf_path;
extern volatile sig_atomic_t done;
extern const char *logfname;
extern const char *pqfname;
extern struct pqueue *pq;

/* timeout for rpc calls */
#ifndef DEFAULT_RPCTIMEO
#  define DEFAULT_RPCTIMEO  60
#endif
extern unsigned int rpctimeo;

/* time we sleep in pq_suspend() and before retrying connects */
extern unsigned int interval;

/*
 * Shut down a service connection that has been idle this long.
 * The keepalive timeout (for the other end) is
 * inactive_timeo/2 - 2 * interval;
 */
extern const int inactive_timeo;

/*
 * In requests,
 * we set 'from' to 'toffset' ago, and it may get
 * trimmed by  pq_clss_setfrom();
 */
#ifndef DEFAULT_OLDEST
#  define DEFAULT_OLDEST  3600
#endif
extern int max_latency;
extern int toffset;

extern void clr_pip_5(void);	        /* defined in svc5.c */
extern int  read_conf(
    const char*	conf_path,
    int		doSomething,		/* defined in conf.y */
    unsigned	port);

/*
 * The following are defined in remote.c:
 */
extern void        free_remote_clss(void);
extern void
    setremote(const struct sockaddr_in* const paddr, const int sock);
extern void        svc_setremote(struct svc_req *rqstp);
extern void        str_setremote(const char *name);
extern const char *remote_name(void);
extern int         update_remote_clss(prod_class_t *want);

/*
 * Calls exit() if the "done" global variable is set; othewise, returns 1 so
 * that it can be easily used in programming loops.
 *
 * Arguments:
 *	status	Exit status for the call to exit().
 * Returns:
 *	1
 */
int
exitIfDone(
    const int	status);

#endif /*!_LDM_SERVER_GLOBAL_H*/
