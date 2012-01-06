/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: wmo_message.c,v 1.70 2002/04/30 18:22:46 chiz Exp $ */

/* 
 * Routines to parse wmo messages.
 * Ref: _Manual on the Global Telecommunications System_, Volume 1,
 *      Global Aspects. WMO-No. 386, 1986
 */
#include <ldmconfig.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include "ulog.h"
#include "ldmalloc.h"
#include "tokens.h"
#include "xbuf.h"
#include "wmo_message.h"

#define GRIB_RE_IDENT 1

extern void toClients(timestampt arrival,
	unsigned seqno,
	const char *ident,
	unsigned len,
	const char *buf);


/* statistics */
static unsigned long completed = 0;
unsigned long sohetx_missed = 0;
unsigned long bad_cksum = 0;
unsigned long not_wmo = 0;


void
wmo_stats(void)
{
	unotice("  WMO Messages seen:  %8lu", completed);
	unotice("  SOH/ETX missing  :  %8lu", sohetx_missed);
	unotice("  parity/chksum err:  %8lu", bad_cksum);
	unotice("  WMO format errors:  %8lu", not_wmo);
}



/*
 * Set time stamp and hook up body of message
 */
static wmo_message *
init_wmo_message(
	xbuf *buf, /* should be a clone ! */
	wmo_message *mess
)
{
	mess->len = buf->cnt;
	mess->msg = buf->get;
	
	if(set_timestamp(&mess->arrival) != 0)
		return NULL;

	return mess;
}


wmo_message *
get_wmo_message(xbuf *buf, wmo_message *mess)
{
	init_wmo_message(buf, mess);

#if 0
	if(mess->msg == NULL || mess->msg[0] != SOH)
		return NULL;

	if(mess->len < MIN_WMO_MSG_LEN)
		return NULL;
#endif

	/* skip SOH CR CR NL */
	if(skipline(buf, 4) < 0) return NULL;

	if( get_wmo_start(buf, mess->start) == NULL )
		return NULL;
	if( get_wmo_header(buf, mess->hdr) == NULL )
		return NULL;

#if 0
	if(mess->msg[mess->len-1] != ETX )
		return NULL;
#endif

#if 0 /* DEBUG */
	fprint_wmo_header(stderr, mess->hdr);
	fputc(' ', stderr);
	fprint_wmo_start(stderr, mess->start);
	fprintf(stderr," \tplen %-7d ok\n", len);
#endif

	return mess;
}


#if GRIB_RE_IDENT

# ifndef KEYSIZE
# define KEYSIZE 255
# endif /*!KEYSIZE */

/* Not very efficient for long anchor strings... */
static const char *
scanFor(const char *anchor, size_t len, const char *const buf)
{
	const char *ap;
	const char *cp;
	const char *start = 0;
	
	ap = anchor;	
	for(cp = buf; len != 0; cp++, len--)
	{
		if(*cp == *ap)
		{
			if(ap == anchor)
				start = cp;
			ap++;
			if(*ap) 
			{
				if(len <= 1)
					return 0;
				if(*ap != *(cp +1))
				{
					/* restart */
					ap = anchor;
					start = 0;
				}
				continue;
			}
			/* else *ap == 0, done */
			return start;
		}
	}
	return 0;
}

static const char *
s_byte(unsigned char bb)
{
	static char buf[4];
	memset(buf, 0, sizeof(buf));
	sprintf(buf, "%d", bb);
	return buf;
}

static const char *
s_model_id(const char *model, unsigned char bb)
{
        static char buf[10];
        memset(buf,0,sizeof(buf));
        sprintf(buf,"%s_%d",model,bb);
        return buf;
}


const char *
s_pds_model(unsigned char center, unsigned char model)
{
        /* TODO: what are the NWS abbrevs of these ?*/
if(center == 7)
   {
        switch (model) {
        case   5: return "SAT"; /* TODO, use pds octet 41 */
        case  10: return "NOW";
        case  19: return "LFM";
        case  25: return "SNO";
        case  39: return "NGM";
        case  42: return "GOI_AVN";
        case  43: return "GOI_FNL";
        case  44: return "SST";
        case  45: return "OCN";
        case  53: return "LFM4";
        case  64: return "ROI";
        case  68: return "SPEC_80_AVN";
        case  69: return "SPEC_80_MRF";
        case  70: return "QLM";
        case  73: return "FOG";
        case  74: return "WWMEX";
        case  75: return "WWAK";
        case  76: return "BMRF";
        case  77: return "AVN";
        case  78: return "MRF";
        case  79: return "BACKUP";
        case  80: return "SPEC62MRF";
        case  81: return "SSIAVN";
        case  82: return "SSIFNL";
        case  83: return s_model_id("ETA",model);
        case  84: return s_model_id("ETA",model);
        case  85: return s_model_id("ETA",model);
        case  86: return "RUC";
        case  87: return "ENSMB";
        case  88: return "PWAV";
        case  89: return s_model_id("ETA",model);
        case  94: return "MRF";
        case  96: return "AVN";
        case 105: return "RUC2";
        case 110: return s_model_id("ETA",model);
        case 121: return s_model_id("NWW",model);
        case 122: return s_model_id("NWW",model);
        case 150: return "RFS";
        case 151: return "FFGS";
        case 152: return "RAD2";
        case 153: return "RAD3";
        default:
                  return s_byte(model);
        }
   }

if((center == 8)||(center == 9))
   return s_model_id("NWS",model);

if(center == 98)
   return s_model_id("ECMWF",model);

if(center == 74)
   return s_model_id("UKM",model);

if(center == 58)
   return s_model_id("FNOC",model);

if(center == 59)
   {
   switch (model) {
        case 105: return "RUC2";
        default:
                  return s_model_id("FSL",model);
        }
   }

if(center == 54)
   {
   switch(model)
      {
      case  36: return "GEM";
      default:
                return s_model_id("CMS",model);
      }
   }


if(center == 60)
   return s_model_id("NCAR",model);

/* default */
return s_byte(model);
}

/*
 * offset
 */
#define IDS_LEN (5)
#define PDS_CENTER (5)
#define PDS_MODEL (6)

static int
ids_len(const char *cp)
{
	const unsigned char *up = (const unsigned char *)cp + IDS_LEN -1 ;
	int len = *up++ * 256 *256;
	len += *up++ * 256;
	len += *up;
	return len;
}

static const char *
pds_ident(const char *cp)
{
	/* assert (pdsp + 28) is valid, accessable memory */
	const unsigned char *pdsp = (const unsigned char *)cp -1;
					/* -1 trick for 1 based indexing */
	static char idb[80];
	memset(idb, 0, sizeof(idb));	

	sprintf(idb, " /m%s",
		s_pds_model(*(pdsp + PDS_CENTER), *(pdsp + PDS_MODEL)));
	return idb;
}

static const char *
grib_ident(size_t remaining, const char *const wmo_msg, const char *const ident)
{
	static const char nada[] = "";
	const char *cp;
	size_t len;

	cp = scanFor("GRIB", remaining, wmo_msg);
	if(!cp)
	{
		unotice("%s: Can't find `GRIB'", ident);
		return nada;
	}
	remaining -= cp - wmo_msg;
	if(remaining < (8 + 28 +4))
	{
		unotice("%s: way too short", ident);
		return NULL;
	}

	len = ids_len(cp);
	if(remaining < len)
	{
		unotice("%s: %d bytes too short", ident, len - remaining);
		return NULL;
	}

         /* this assumes 7777\r\r\n ETX, however, some products
            may have extra \r\n's so this test is too rigid-
            The test for 7777 at expected len should suffice (Chiz)
	if(remaining > len + 8)
	{
		unotice("%s: garbled product", ident);
		unotice("remaining %d, ids_len %d", remaining, len);
		return NULL;
	} */

	if(scanFor("7777", 8, cp + len - 8) == NULL)
	{
		unotice("%s: no end of product", ident);
		return NULL;
	}

	return pds_ident(cp + 8);
}
#endif /* GRIB_RE_IDENT */


const char *
wmo_err_ident(xbuf *buf)
{
	static char identbuf[128];
	char *cp = identbuf;
	xbuf clone[1];
	int conv;

	(void) memset(identbuf, 0, sizeof(identbuf));

	clone_xbuf(buf, clone, 0);
	
#if 0
	conv = sprintf(cp, "%8u ******************",
			 (unsigned) clone->cnt);
#else
	conv = sprintf(cp, "%8u",
			 (unsigned) clone->cnt);
#endif
	cp += conv;

	/* skip SOH CR CR NL */
	if(skipline(clone, 4) < 0)
		return identbuf;

	{
		wmo_start_t start;
		if( get_wmo_start(clone, &start) == NULL )
			return identbuf;
		conv = sprintf(cp, "         %03d", start.seqno);
		cp += conv;
	}

	{
		wmo_header_t hdr;
		dtime time;
		hdr.time = &time;
		if( get_wmo_header(clone, &hdr) == NULL )
			return identbuf;
		conv = sprintf(cp, "  %s", s_wmo_header(&hdr));
		cp += conv;
		assert(cp < &identbuf[sizeof(identbuf)]);
	}
	
	return identbuf;
}

void
wmo_send_buf(
	xbuf *buf,
	int backoff)
{
	xbuf clone[1];
	wmo_message mess[1];
	wmo_start_t start[1];
	wmo_header_t hdr[1];
	dtime time[1];
	char *ident;

	mess->start = start;
	mess->hdr = hdr;
	mess->hdr->time = time;

	/* we have a complete message in the xbuf */
	clone_xbuf(buf,clone, backoff);

	if( get_wmo_message(clone, mess) == NULL
			|| start->seqno < 0
			|| start->seqno > 999
			|| !isalpha(hdr->TT[0])
			|| !isalpha(hdr->TT[1])
			|| !isalpha(hdr->AA[0])
			|| !isalnum(hdr->AA[1])
		)
	{
		not_wmo++;
		uerror("Not a WMO format message.        %s",
			wmo_err_ident(buf));
		return;
	}
	/* else, we got something */

	ident = s_wmo_header(mess->hdr);
#if GRIB_RE_IDENT
	if(*ident == 'H' || *ident == 'Y' || *ident == 'Z')
	{
		const char *ni =
			 grib_ident(mess->len, (char *)mess->msg, ident);
		if(ni == NULL)
		{
			not_wmo++;
			uerror("Not a WMO GRIB format message.   %s",
				wmo_err_ident(buf));
			return;
		}
		/* else */
		if(*ni != 0)
		{
			size_t len = strlen(ident);
			strncat(ident, ni, KEYSIZE - len);
		}
	}
#endif
		
	completed++;
	toClients(mess->arrival,
		mess->start->seqno,
		ident,
		(unsigned) mess->len, (char *)mess->msg);
}
