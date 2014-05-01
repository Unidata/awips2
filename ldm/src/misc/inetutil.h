/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: inetutil.h,v 1.12.18.3 2006/10/19 19:28:08 steve Exp $ */

/* 
 * Miscellaneous functions to make dealing with internet addresses easier.
 */

#ifndef _INETUTIL_H_
#define _INETUTIL_H_

#include <netinet/in.h>
#include "error.h"

#ifdef IPPROTO_IP /* we included netinet/in.h, so struct sockaddr_in is */
extern const char* hostbyaddr(
    const struct sockaddr_in* const	paddr);
extern int addrbyhost(const char *hostname, struct sockaddr_in *paddr);
extern ErrorObj* hostHasIpAddress(
    const char* const	hostname,
    const in_addr_t	targetAddr,
    int* const		hasAddress);
extern char *s_sockaddr_in(struct sockaddr_in *paddr);
extern int gethostaddr_in(struct sockaddr_in *paddr);
#endif
extern int getservport(const char *servicename, const char *proto);
extern char *ghostname(void);
extern int usopen(const char *name);
extern int udpopen(const char *hostname, const char *servicename);
extern int isMe(const char *remote);
extern int local_sockaddr_in(struct sockaddr_in* addr);
extern int sockbind(const char *type, unsigned short port);

#endif /* !_INETUTIL_H_ */
