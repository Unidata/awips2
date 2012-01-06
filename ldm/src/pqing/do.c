/*
 * $Id: do.c,v 1.24.2.3 2003/06/23 18:44:16 steve Exp $
 *
 * Finite state automata for WMO products -- both "old" products and newer 
 * asynchronous binary products (e.g. HDS) with byte stuffing and checksums.
 */

#include <ldmconfig.h>
#include <stdio.h>
#include <ctype.h>

#include "xbuf.h"
#include "tokens.h"
#include "wmo_message.h"
#include "ulog.h"

#if !defined (SCAN_CHECK)

int peek_ahead(xbuf *buf)
{
int i, j, k, ic[4], isstart=1;
static int IC[4]={SOH, CR, CR, NL};
static int TC[4]={CR, CR, NL, ETX};
static int UH[4]={NL, SOH, CR, CR};
/*
 * If there are at least 4 more characters in the buffer, and
 * they are not the start sequence for the next product, then
 * we may still be in the previous product. If there are fewer
 * than 4 characters in the buffer, assume we did find the end.
 */

for(i=0;i<4;i++)
   {
   ic[i] = nextc(buf); j = i;
   if(ic[i] == EOB) 
      {
      isstart = 1;
      break;
      }
   if((ic[i] != TC[i])&&(ic[i] != IC[i])&&(ic[i] != UH[i]))
      isstart = 0;
   }

for(i=j;i>=0;i--)
   {
   if(ic[i] != EOB)
      {
      k = unnextc(buf,ic[i]);
      if(k != ic[i])
         uerror("Error in peek_ahead unnext: %d %d\0",ic[i],k);
      }
   if(isstart == 0)
      udebug("Peek_ahead check %d %d\0",i,ic[i]);
   }

if(isstart)
   return(1);
else
   return(0);
}
#else
int peek_ahead(xbuf *buf);
#endif


#if defined(SCAN_CHECK)

#   include "crc_tab.h"

    /*
     * Macro for accumulating checksum:
     */
#   define ADD_CRC(crc, ch) \
	{ \
	    crc	= crc_table[(crc & 0x00ff) ^ ch] ^ (crc >> 8); \
	} 

    /*
     * Macro for resetting parameters when start-of-frame is seen:
     */
#    define RESET(off, crc, ch) \
	{ (off) = (crc) = 0; ADD_CRC(crc, ch); }

#else	/* old NPS below */

#   define ADD_CRC(crc, ch)	{}
#   define RESET(off, crc, ch)	{}

#endif	/* old NPS above */


/*
 * Scan available input.  Do the right thing at WMO message boundaries.  
 */
    int
#if defined(SCAN_CHECK)
    scan_wmo_binary_crc(xbuf *buf)
#else
    scan_wmo_binary(xbuf *buf)
#endif
{
    int			sent = 0;
    int			ch;

    while ((ch = nextc(buf)) != EOB) {
#	if defined(SCAN_CHECK)
	    unsigned		prev_crc;
	    static unsigned	crc;
	    static unsigned	offset;
#	endif
	static enum state {
	    START,
	    SEARCH,
	    SOH_,
	    SOH_CR_,
	    SOH_CR_CR_,
	    HEADER_SEEN,
	    CR_,
	    CR_CR_,
	    CR_CR_NL_
#	    if defined(SCAN_CHECK)
		,
		TRAILER_SEEN,
		CHECKBYTE_SEEN
#	    endif
	}		state	= START;

	/*
	 * Incrementing the checksum at this point is not always necessary
	 * but is done to reduce the number of ADD_CRC() invocations and,
	 * consequently and hopefully, to reduce problems with maintenance
	 * and understanding.
	 */
#	if defined(SCAN_CHECK)
	    prev_crc	= crc;
	    ADD_CRC(crc, ch);
#	endif

	switch (state) {
	case START:
	    if (SOH == ch) {
#	if defined(SCAN_CHECK)
		RESET(offset, crc, ch);
#	endif
		state	= SOH_;
	    } else {
		if (CR != ch && NL != ch) {
		    udebug("garbage encountered while searching for header");
		}
		state	= SEARCH;
	    }
	    break;
	case SEARCH:
	    if (SOH == ch) {
#	if defined(SCAN_CHECK)
		RESET(offset, crc, ch);
#	endif
		state	= SOH_;
	    }
	    break;
	case SOH_:
#	    if defined(SCAN_CHECK)
		if (SOH == ch) {
		    RESET(offset, crc, ch);
		} else 
#	    endif
	    if (CR == ch) {
		state	= SOH_CR_;
	    } else {
		state	= SEARCH;
	    }
	    break;
	case SOH_CR_:
	    if (CR == ch) {
		state	= SOH_CR_CR_;
	    } else {
		state	= SEARCH;
	    }
	    break;
	case SOH_CR_CR_:
	    if (NL == ch) {
		justify_xbuf(buf, 4);
		state	= HEADER_SEEN;
	    } else if (CR != ch) {
		state	= SEARCH;
	    }
	    break;
	case HEADER_SEEN:
	    if (CR == ch)
		state	= CR_;
	    break;
	case CR_:
	    state	= CR == ch
			    ? CR_CR_
			    : HEADER_SEEN;
	    break;
	case CR_CR_:
	    if (NL == ch) {
		state = CR_CR_NL_;
	    } else if (CR != ch) {
		 state = HEADER_SEEN;
	    }
	    break;
	case CR_CR_NL_:
            if (ETX == ch)
	       { 
	       /* see if we can peek ahead, and if so,  does it look like
		  the start of a new product? */
               if(! peek_ahead(buf))
                  {
		  state = HEADER_SEEN;
		  break;
		  }
               }
	    if (ETX == ch) {
#		if defined(SCAN_CHECK)
		    state	= TRAILER_SEEN;
#		else
		    const long plen = buf->get - buf->base;
		    if(plen >= MIN_WMO_MSG_LEN)
		    {
			wmo_send_buf (buf, 0);
			sent++;
		    }
		    else
		    {
			not_wmo++;
			uerror("scan_wmo_binary: length %ld too short",
				plen);
		    }
		    justify_xbuf(buf, 0);
		    state	= START;
#		endif
	    } else if (CR == ch) {
		state	= CR_;
	    } else {
		    if (ESC == ch) {
			xbuf_rubout(buf);
#		if defined(SCAN_CHECK)
			crc	= prev_crc;
#		endif
		    }
		state	= HEADER_SEEN;
	    }
	    break;
#	if defined(SCAN_CHECK)
	    case TRAILER_SEEN:
		state	= CHECKBYTE_SEEN;
		break;
	    case CHECKBYTE_SEEN:
		if (0 != crc) {
			uerror("Checksum (0x%04x) error          %s",
				crc, wmo_err_ident(buf));
			bad_cksum++;
		}
		else {
		    const long plen = buf->get - buf->base;
		    if(plen >= MIN_WMO_MSG_LEN)
		    {
			wmo_send_buf (buf, 2);
			sent++;
		    }
		    else
		    {
			not_wmo++;
			uerror("scan_wmo_binary_crc: length %ld too short",
				plen);
		    }
		}
		justify_xbuf(buf, 0);
		state	= START;
		break;
#	endif
	}				/* state switch */

#	if defined(SCAN_CHECK)
	    offset++;
#	endif
    }					/* input character loop */
    return sent ;
}


/*
 * Scan input for wmo product boundaries.
 * Non binary (Text) version.
 */
int
#if !defined(SCAN_CHECK)
scan_wmo (xbuf * buf)
#else
scan_wmo_parity (xbuf * buf)
#endif
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
#if 0
			case SEARCH:
				udebug ("Search %05ld bytes",
					(long) (buf->get - buf->base) - 1);
				break;
#endif
			case HEADER_SEEN:
				sohetx_missed++;
				uerror("Lone SOH error                   %s",
					wmo_err_ident(buf));
				break;
			case PARITY_PIP:
				bad_cksum++;
				uerror("Parity error                     %s",
					wmo_err_ident(buf));
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
				uerror("Lone ETX error                   %s",
					wmo_err_ident(buf));
				break;
			case HEADER_SEEN:
			{
				const long plen = buf->get - buf->base;
				if(plen >= MIN_WMO_MSG_LEN)
				{
					wmo_send_buf (buf, 0);
					sent++;
				}
				else
				{
					not_wmo++;
					uerror("scan_wmo: length %ld too short",
						plen);
				}
				justify_xbuf (buf, 0);
				state = SEARCH;
			}
				break;
			case PARITY_PIP:
				bad_cksum++;
				uerror("Parity error                     %s",
					wmo_err_ident(buf));
				break;
			}
			justify_xbuf (buf, 0);
			state = SEARCH;
			break;
#if defined(SCAN_CHECK)
		case 0:	
		   	if(state == HEADER_SEEN
				/* && (parity != NULL && *parity != 'n') */
				)
		   	{
				state = PARITY_PIP;
		   	}
			break;
#elif defined(PARITY_ERR_CH)
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
