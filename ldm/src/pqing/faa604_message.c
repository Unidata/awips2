/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: faa604_message.c,v 1.3 1998/10/16 19:28:39 steve Exp $ */

/* 
 * Routines to handle FAA604 messages.
 * Thanks to NWS personnel, who shared their ldm3 mods and
 * other info.
 * N.B. This module prepared without any supporting definitions
 * or FAA604 documentation.
 */

#include <ldmconfig.h>
#include <string.h>
#include <ctype.h>
#include "xbuf.h"
#include "tokens.h"
#include "faa604_message.h"
#include "ulog.h"


static unsigned long completed = 0;
static unsigned long sohetx_missed = 0;
static unsigned long parity_errs;
static unsigned long not_faa604 = 0;

void
faa604_stats(void)
{
	unotice("  FAA Messages seen:  %8lu", completed);
	unotice("  SOH/ETX missing  :  %8lu", sohetx_missed);
	unotice("  parity errors    :  %8lu", parity_errs);
	unotice("  format errors    :  %8lu", not_faa604);
}

static const char *
faa604_err_ident(xbuf *buf)
{
	static char identbuf[128];
	char *cp = identbuf;
	xbuf clone[1];
	int conv;
	int len;
	int ch;

	(void) memset(identbuf, 0, sizeof(identbuf));

	clone_xbuf(buf, clone, 0);
	
	conv = sprintf(cp, "%8u",
			 (unsigned) clone->cnt);
	cp += conv;

	
	/* seqno */
	do{
		ch = nextc(clone);
	}while(Wisspace(ch));
	if(ch == EOB)
		goto done;
	conv = sprintf(cp, " ");
	cp += conv;
	if(!isgraph(ch))
		*cp++ = ' ';
	else
		*cp++ = ch;
	for(len = 0; len < 2 ; len++ )
	{
		ch = nextc(clone);
		if(ch == EOB)
			goto done;
		if(!isgraph(ch))
			*cp++ = ' ';
		else
			*cp++ = ch;
	}
	conv = sprintf(cp, "  ");
	cp += conv;

	
	/* identifier */
	for(len = 0; len < 5 ; len++ )
	{
		ch = nextc(clone);
		if(ch == EOB)
			goto done;
		if(!isgraph(ch))
			*cp++ = ' ';
		else
			*cp++ = ch;
	}
	
	ch = nextc(clone); /* STX */
	if(ch == EOB)
		goto done;
	
	do{
		ch = nextc(clone);
	}while(Wisspace(ch));
	if(ch == EOB)
		goto done;

	*cp++ = ' ';
	if(!isgraph(ch))
		*cp++ = ' ';
	else
		*cp++ = ch;
	for(len = 0; len < 5 ; len++ )
	{
		ch = nextc(clone);
		if(ch == EOB)
			goto done;
		if(!isgraph(ch))
			*cp++ = ' ';
		else
			*cp++ = ch;
	}
done:
	*cp = 0;

	return identbuf;
}


/*
 * Set time stamp and hook up body of message
 */
static faa604_message *
init_faa604_message(
	xbuf *buf, /* should be a clone ! */
	faa604_message *mess
)
{
	mess->len = buf->cnt;
	if( set_timestamp(&mess->arrival) != 0)
		return NULL;
	mess->seqno = -1;
	memset(mess->ident, 0, sizeof(mess->ident));

	mess->msg = buf->get;

	return mess;
}


#define MIN_FAA604_MSG_LEN 28

static int
get_faa604_message(xbuf *buf, faa604_message *mess)
{
	xbuf clone[1];
	int ch;
	char *cp;
	int len;

	clone_xbuf(buf, clone, 0);
	init_faa604_message(clone, mess);

	/* DEBUG */
	if(mess->msg == NULL || mess->msg[0] != SOH)
	{
		udebug("new_faa604_message: Missing SOH");
	}
	if(mess->len < MIN_FAA604_MSG_LEN)
	{
		udebug("new_faa604_message: length %d too short", mess->len);
	}
	if(mess->msg[mess->len-1] != ETX )
	{
		udebug("new_faa604_message: Missing ETX");
	}

	if( get_wnum(clone, &mess->seqno, 3) == EOB || mess->seqno < 1)
	{
		not_faa604++;
		uerror("Invalid sequence number  - %s",
			faa604_err_ident(buf));
		return -1;
	}

	
	/* construct the identifier */
	for(len = 0, cp = mess->ident; len < 5 ; len++ )
	{
		ch = nextc(clone);
	
		if(!(isascii(ch) && isdigit(ch)))
		{
			not_faa604++;
			uerror("Invalid catalogue number - %s",
				faa604_err_ident(buf));
			return -1;
		}
		*cp++ = ch;
	}
	
	ch = nextc(clone);
	if(ch != STX)
	{
		not_faa604++;
		uerror("Missing header STX       - %s",
			faa604_err_ident(buf));
		return -1;
	}
	
	do{
		ch = nextc(clone);
	}while(Wisspace(ch));

	if(!(isascii(ch) && isdigit(ch)))
	{
		not_faa604++;
		uerror("Invalid date             - %s",
			faa604_err_ident(buf));
		return -1;
	}
	*cp++ = ' ';
	*cp++ = ch;
	for(len = 0; len < 5 ; len++ )
	{
		ch = nextc(clone);
	
		if(!(isascii(ch) && isdigit(ch)))
		{
			not_faa604++;
			uerror("Invalid date/time        - %s",
				faa604_err_ident(buf));
			return -1;
		}
		*cp++ = ch;
	}
	*cp = 0;

	{
		/* optional */
		wmo_header_t wmo_hdr;
		dtime time;
		wmo_hdr.time = &time;
		if(get_wmo_header(clone, &wmo_hdr) != NULL
				&& wmo_hdr.ii != 0
				&& isupper(wmo_hdr.TT[0])
				&& isupper(wmo_hdr.TT[1])
				&& isupper(wmo_hdr.AA[0])
				&& isupper(wmo_hdr.AA[1]) /* number okay? */
				&& isupper(wmo_hdr.CCCC[0])
			)
		{
			sprintf(cp,
				 " (%s)", s_wmo_header(&wmo_hdr));
		}
	}

	return 0;
}

extern void toClients(timestampt arrival,
	unsigned seqno,
	const char *ident,
	unsigned len,
	const char *buf);


static void
faa604_send_buf(xbuf *buf)
{
	faa604_message mess[1];

	if(get_faa604_message(buf, mess) != 0)
		return;
	/* else, we got something */

	completed++;
	toClients(mess->arrival,
		mess->seqno,
		mess->ident,
		(unsigned) mess->len, (char *)mess->msg);
}


int
scan_faa604_parity (xbuf * buf)
{
	extern char *parity;
	int sent = 0;
	int ch;

	static enum state
	{
		SKIP,
		SEARCH,
		HEADER_SEEN,
		PARITY_PIP
	}
	state = SEARCH;

	while ((ch = nextc (buf)) != EOB)
	{
		switch(ch) {
		case SOH:	
			switch (state) {
			case SKIP:
				udebug ("Resync skipped %05ld bytes",
					(long) (buf->get - buf->base) - 1);
				break;
			case HEADER_SEEN:
				sohetx_missed++;
				uerror("Lone SOH error           - %s",
					faa604_err_ident(buf));
				break;
			case PARITY_PIP:
				parity_errs++;
				uerror("Parity error             - %s",
					faa604_err_ident(buf));
				break;
			}
			justify_xbuf (buf, 1);
			state = HEADER_SEEN;
			break;
		case ETX:	
			switch (state) {
			case SKIP:
#if 0
			case SEARCH:
#endif
				sohetx_missed++;
				uerror("Lone ETX error           - %s",
					faa604_err_ident(buf));
				break;
			case HEADER_SEEN:
			{
				const long plen = buf->get - buf->base;
				if(plen >= MIN_FAA604_MSG_LEN)
				{
					faa604_send_buf (buf);
					sent++;
				}
				else
				{
					not_faa604++;
					uerror("scan_faa604 - length %ld too short",
						plen);
				}
				justify_xbuf (buf, 0);
				state = SEARCH;
			}
				break;
			case PARITY_PIP:
				parity_errs++;
				uerror("Parity error             - %s",
					faa604_err_ident(buf));
				break;
			}
			justify_xbuf (buf, 0);
			state = SEARCH;
			break;
		case 0:	
		   	if(state == HEADER_SEEN
				/* && (parity != NULL && *parity != 'n') */
				)
		   	{
				state = PARITY_PIP;
		   	}
			break;
		default:
			if(state == SEARCH && isgraph(ch))
				state = SKIP;
			break;
		}
	}			/* input character loop */

	return sent;
}

int
scan_faa604 (xbuf * buf)
{
	extern char *parity;
	int sent = 0;
	int ch;

	static enum state
	{
		SKIP,
		SEARCH,
		HEADER_SEEN
	}
	state = SEARCH;

	while ((ch = nextc (buf)) != EOB)
	{
		switch(ch) {
		case SOH:	
			switch (state) {
			case SKIP:
				udebug ("Resync skipped %05ld bytes",
					(long) (buf->get - buf->base) - 1);
				break;
			case HEADER_SEEN:
				sohetx_missed++;
				uerror("Lone SOH error           - %s",
					faa604_err_ident(buf));
				break;
			}
			justify_xbuf (buf, 1);
			state = HEADER_SEEN;
			break;
		case ETX:	
			switch (state) {
			case SKIP:
#if 0
			case SEARCH:
#endif
				sohetx_missed++;
				uerror("Lone ETX error           - %s",
					faa604_err_ident(buf));
				break;
			case HEADER_SEEN:
			{
				const long plen = buf->get - buf->base;
				if(plen >= MIN_FAA604_MSG_LEN)
				{
					faa604_send_buf (buf);
					sent++;
				}
				else
				{
					not_faa604++;
					uerror("scan_faa604 - length %ld too short",
						plen);
				}
				justify_xbuf (buf, 0);
				state = SEARCH;
			}
				break;
			}
			justify_xbuf (buf, 0);
			state = SEARCH;
			break;
#if defined(PARITY_ERR_CH)
		case 0:	
			/*
			 * Backward compatibility
			 * except it can change file feeds
			 */
		   	if(state == HEADER_SEEN
				 && (parity != NULL && *parity != 'n'))
			{
				*(buf->get -1) = PARITY_ERR_CH;
			}
			break;
#endif
		default:
			if(state == SEARCH && isgraph(ch))
				state = SKIP;
			break;
		}
	}			/* input character loop */

	return sent;
}
