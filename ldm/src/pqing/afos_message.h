/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: afos_message.h,v 1.28 1996/09/13 16:48:07 davis Exp $ */
#ifndef _AFOS_MESSAGE_H_
#define _AFOS_MESSAGE_H_

#include "timestamp.h"
#include "xbuf.h"
#include "wmo_header.h"

/* size of the buffer for 9 character AFOS header */
#define MAX_AFOS_HLEN 16 /* actually, its 9 + 1 */
#define MAX_WMO_HLEN 24 /* actually, its 22 +1 */

/*
 * AFOS Message
 */
typedef struct {
	timestampt arrival; /* arrival time */
	char ident[MAX_AFOS_HLEN +MAX_WMO_HLEN + 4]; /* AFOS header and more */
	unsigned char *msg; /* the whole message */
	size_t len;
} afos_message;

extern void afos_stats(void);
extern int scan_afos(xbuf *buf);

#endif /* _AFOS_MESSAGE_H_ */
