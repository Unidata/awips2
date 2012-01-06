/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: peer_info.h,v 1.4.22.1 2008/04/15 16:34:13 steve Exp $ */

#ifndef _PEER_INFO_H
#define _PEER_INFO_H

#include <netinet/in.h>
#include <rpc/rpc.h>
#include "prod_class.h"


#define DOTTEDQUADLEN 16
#define HOSTNAMELEN 256

struct peer_info {
	struct in_addr	addr;		/* actual address, addr.s_addr */
	char astr[DOTTEDQUADLEN];	/* dotted quad string */
	char name[HOSTNAMELEN];		/* lookup */
	char *printname;		/* points to addr or name */
	prod_class_t *clssp;		/* cache from acl computation */
	u_int sendsz;			/* handed to rpc handle creation */
	u_int recvsz;			/* handed to rpc handle creation */
};
typedef struct peer_info peer_info;

extern peer_info remote;	/* declared in remote.c */

#endif /* !_PEER_INFO_H */
