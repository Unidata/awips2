/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: afos_message.c,v 1.39 1999/04/20 22:25:42 davis Exp $ */

/* 
 * Routines to handle AFOS messages.
 * Thanks to NWS personnel, who shared their ldm3 mods and
 * other info.
 * N.B. This module prepared without any supporting definitions
 * or AFOS documentation.
 */

#include <ldmconfig.h>
#include <string.h>
#include <ctype.h>
#include "xbuf.h"
#include "tokens.h"
#include "afos_message.h"
#include "ulog.h"


static int seqno = 0;
static int zczc_missed = 0;
static int nnnn_missed = 0;
static int unrecognizable = 0;

void
afos_stats(void)
{
	unotice("  AFOS Messages seen: %8d", seqno - unrecognizable);
	unotice("  Errors:  lone ZCZC: %8d", nnnn_missed);
	unotice("           lone NNNN: %8d", zczc_missed);
	unotice("      Unrecognizable: %8d", unrecognizable);
}


/*
 * Set time stamp and hook up body of message
 */
static afos_message *
init_afos_message(
	xbuf *buf, /* should be a clone ! */
	afos_message *mess
)
{
	mess->len = buf->cnt;
	mess->msg = buf->get;
	
	if( set_timestamp(&mess->arrival) != 0)
		return NULL;

	return mess;
}


/*
 * skip up to the first Upper case char and get the rest of the line
 */
static int
nextUcaseLine(xbuf *buf, char *str, int maxlen) 
{
	int ch;
	do{
		ch = nextc(buf);
		if(ch == EOB)
			break;
	}while(!isascii(ch) || !isupper(ch));
	unnextc(buf,ch);
	return( get_line(buf, str, maxlen) );
}

#define MIN_AFOS_MSG_LEN 14 /* wild ass guess */

static int
get_afos_message(xbuf *buf, afos_message *mess)
{
	int zczc_missing = 0;

	init_afos_message(buf, mess);

	/* DEBUG */
	if(mess->len < MIN_AFOS_MSG_LEN)
	{
		uerror("new_afos_message: length %d too short", mess->len);
		goto err;
	}

	if(!hasSTR(buf, "ZCZC"))
	{
		/* not fatal, complain later */
		zczc_missing = 1;
	}

	if(nextUcaseLine(buf, mess->ident, 12) == EOB) goto err;

	{
		/*
		 * Skip white space and (maybe) the next two bytes.
		 * What do they mean?
		 */
		int ch;
		do{
			ch = nextc(buf);
		} while((isascii(ch) && !isgraph(ch)));
		if(isascii(ch))
			unnextc(buf, ch);
		else
			ch = nextc(buf);
	}
	
	{
		/* optional */
		wmo_header_t wmo_hdr;
		dtime time;
		wmo_hdr.time = &time;
		if(get_wmo_header(buf, &wmo_hdr) != NULL
				&& isupper(wmo_hdr.TT[0])
				&& isupper(wmo_hdr.TT[1])
				&& isupper(wmo_hdr.AA[0])
				&& isupper(wmo_hdr.CCCC[0])
			)
		{
			size_t len = strlen(mess->ident);
			sprintf(&mess->ident[len],
				 "   (%s)", s_wmo_header(&wmo_hdr));
		}
	}

	if(zczc_missing)
	{
		uerror(" Missing ZCZC: %6s %03d %8d  %s",
				"AFOS",
				seqno,
				mess->len,
				mess->ident);
	}
	if(memcmp(&mess->msg[mess->len -4], "NNNN", 4) != 0)
	{
		uerror(" Missing NNNN: %6s %03d %8d  %s",
				"AFOS",
				seqno,
				mess->len,
				mess->ident);
		/* Fatal, They will send it again */
		return -1;
	}

	return 0;
err:
	unrecognizable++;
	uerror("Unrecognizable: %5s %03d %8d",
			"AFOS",
			seqno,
			buf->cnt);
	return -1;
}

extern void toClients(timestampt arrival,
	unsigned seqno,
	const char *ident,
	unsigned len,
	const char *buf);

static void
afos_send_buf(xbuf *buf, int backoff)
{
	xbuf clone[1];
	afos_message mess[1];

	seqno++;

	clone_xbuf(buf,clone, backoff);

	if(get_afos_message(clone, mess) != 0)
		return;
	/* else, we got something */

	toClients(mess->arrival,
		seqno,
		mess->ident,
		(unsigned) mess->len, (char *)mess->msg);
}


int
scan_afos(xbuf *buf)
{
	int sent = 0;
	int ch;

	static enum afos_l_state {
	    GND,
	    Z_,
	    Z_C_,
	    Z_C_Z_,
	    N_,
	    N_N_,
	    N_N_N_
	} lstate = GND;

	static enum afos_state {
		START,
		HEADER_SEEN,
		TRAILER_SEEN
	} state = START;

	while ((ch = nextc(buf)) != EOB)
	{
		switch (ch) {
		case 'Z':
			if(lstate == Z_C_)
				lstate = Z_C_Z_;
			else
	      			lstate = Z_;
	    		break;

		case 'C':
	    		if(lstate == Z_)
				lstate = Z_C_;
	    		else if(lstate == Z_C_Z_ )
			{
	      			/* Z_C_Z_C */
				if(state == HEADER_SEEN)
				{
	      				/* missing trailer */
					nnnn_missed++;
					udebug("Missing NNNN trailer");
					afos_send_buf(buf, 4);
					sent = 1;
	     			}
				justify_xbuf(buf, 4);
				state = HEADER_SEEN;
	      			lstate = GND;
			}
	    		break;
		case 'N':
	    		switch (lstate) {
			case N_N_N_ :
	      			/* N_N_N_N */
				if(state == TRAILER_SEEN)
				{
					/* missing header */
					zczc_missed++;
					udebug("Missing ZCZC header");
				}
				afos_send_buf(buf, 0);
				sent = 1;
				justify_xbuf(buf, 0);
				state = TRAILER_SEEN;
	      			lstate = GND;
				break;
	      		case N_N_ :
				lstate = N_N_N_;
				break;
	      		case N_ :
				lstate = N_N_;
				break;
	      		default :
				lstate = N_;
				break;
	    		} /* lstate switch */
	   		break;
	   	default :
			lstate = GND;
			break;
		} /* ch switch */
	} /* input character loop */
	return sent;
}
