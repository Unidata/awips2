/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: tokens.h,v 1.13.22.1 2004/12/09 19:36:25 steve Exp $ */
#ifndef _TOKENS_H_
#define _TOKENS_H_
#include "xbuf.h"

/* characters which may trigger change of major state */
#define SOH		0x01	/* ^A */
#define ETX		0x03	/* ^C */

/* characters which may trigger change of 'line' state */
#define NL	0x0a
#define CR	0x0d
#define ETB	0x17
#define ESC	0x1b

#define SP	(' ')
/*
 * <stdlib.h> of HP-UX B.11.00 defines RS.
 */
#define RECORD_SEP	0x1e	/* ^^ */
#define VT	0x0b	/* ^K */
#define EQ	('=')

#define	STX	0x02	/* ^B */


#if 0
#define TOK_SOH (-(SOH + 0xff))
#define TOK_ETX (-(ETX + 0xff))

#define CR_NL ( CR + NL + 0xff ) /* 278 */
#define CR_CR_NL ( CR + CR_NL ) /* 291 */
#define NL_ETX ( NL + ETX + 0xff ) /* 268 */
#define CR_NL_ETX ( CR + NL_ETX ) /* 281 */
#endif

#define Wisspace(ch) ((isascii(ch) && isspace(ch)) \
	|| ((ch) == RECORD_SEP) || ((ch) == SOH) || ((ch) == ETX))

extern int skipline(xbuf *buf, int maxlen);
extern int get_line(xbuf *buf, char *str, int maxlen);
extern int get_wline(xbuf *buf, char *str, int maxlen);
extern int get_str(xbuf *buf, char *str, int maxlen);
extern int get_wstr(xbuf *buf, char *str, int maxlen);
extern int get_num(xbuf *buf, int *num, int maxlen);
extern int get_wnum(xbuf *buf, int *num, int maxlen);
extern int dget_num(xbuf *buf, int *num, int maxlen);
extern int dget_wnum(xbuf *buf, int *num, int maxlen);
extern int get_weqxbuf(xbuf *buf, xbuf *clone);
extern int hasSTR(xbuf *buf, char *str);
extern int whasSTR(xbuf *buf, char *str);

#endif /* !_TOKENS_H_ */
