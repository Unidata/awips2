/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: wmo_message.h,v 1.43 1996/09/11 23:49:20 davis Exp $ */
#ifndef _WMO_MESSAGE_H_
#define _WMO_MESSAGE_H_

#include <stddef.h>
#include "timestamp.h"
#include "wmo_start.h"
#include "wmo_header.h"
#include "xbuf.h"

/*
 * WMO Message
 */
typedef struct {
	timestampt arrival; /* arrival time */
	wmo_start_t *start;
	wmo_header_t *hdr;
	unsigned char *msg; /* the whole message */
	size_t len;
} wmo_message;


#define MIN_WMO_MSG_LEN 34 /* if it has checksums, this size + 2 */

#if 0 /* not exported */
extern void free_wmo_message(wmo_message *msg);
extern wmo_message *new_wmo_message(unsigned char *msg, int len);
#endif

extern unsigned long sohetx_missed;
extern unsigned long bad_cksum;
extern unsigned long not_wmo;

extern void wmo_stats(void);
extern int scan_wmo(xbuf *buf);
extern int scan_wmo_parity(xbuf *buf);
extern int scan_wmo_binary(xbuf *buf);
extern int scan_wmo_binary_crc(xbuf *buf);
const char * wmo_err_ident(xbuf *buf);
extern void wmo_send_buf(xbuf *buf, int backoff);
extern wmo_message *get_wmo_message(xbuf *buf, wmo_message *mess);

#endif /* _WMO_MESSAGE_H_ */
