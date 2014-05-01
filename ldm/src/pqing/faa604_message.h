/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: faa604_message.h,v 1.1 1996/09/13 18:37:19 davis Exp $ */
#ifndef _FAA604_MESSAGE_H_
#define _FAA604_MESSAGE_H_

#include "timestamp.h"
#include "xbuf.h"
#include "wmo_header.h"

/* size of the buffer for FAA604 identifer */
#define MAX_FAA604_HLEN 16 /* 5 + 1 + 6 + 1 */
#define MAX_WMO_HLEN 24 /* actually, its 22 +1 */

/*
 * FAA604 Message
 */
typedef struct {
	size_t len;
	timestampt arrival; /* arrival time */
	int seqno;
	char ident[MAX_FAA604_HLEN + MAX_WMO_HLEN + 4]; /* 604 ident and more */
	unsigned char *msg; /* the whole message */
} faa604_message;

extern void faa604_stats(void);
extern int scan_faa604_parity(xbuf *buf);
extern int scan_faa604(xbuf *buf);

#endif /* _FAA604_MESSAGE_H_ */
