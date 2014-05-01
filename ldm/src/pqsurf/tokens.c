/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: tokens.c,v 1.22.22.1 2004/12/09 19:36:24 steve Exp $ */
#include <ldmconfig.h>
#include <stdio.h>
#include <ctype.h>
/* #include <assert.h> */
#include "tokens.h"
#include "xbuf.h"

int
skipline(
        xbuf *buf,
        int maxskip)
{
        int ch = 0;
        do {
                if(maxskip <= 0) break;
                ch = nextc(buf);
                maxskip--;
        } while(ch != NL && ch != EOB);
        return ch;
}


/*
 * read the (rest of) a line, up to maxlen into linebuf.
 */
int
get_line(
        xbuf *buf,
        char *str,
        int maxlen)
{
        int ch;
        int len = 0;

        static enum {
                GND, /* ground state */
                CR_,
                CR_CR_
        /*      CR_CR_NL_, */
        } lstate = GND;

        /* assert(maxlen > 3); */
        for(len = 0; len < maxlen ; len++ )
        {

                ch = nextc(buf);

                switch (ch) {
                case CR :
                        if(lstate == CR_ )
                                lstate = CR_CR_;
                        else 
                                lstate = CR_;
                        break;
                case NL :
                case EOB :
                        goto done;      
                default :
                        if(!isalnum(ch)) goto done;
                        *str++ = (char)ch;
                        break;
                }

        }
done :
        *str = 0;
        return ch;
}


/*
 * skip leading white space and get the rest of the line
 */
int
get_wline(
        xbuf *buf,
        char *str,
        int maxlen)
{
        int ch;
        do{
                ch = nextc(buf);
        }while(ch == SP || ch == '\t'); /* don't soak up CR or NL :-) */
        unnextc(buf,ch);
        return( get_line(buf, str, maxlen) );
}



/*
 * get the next token
 * may NOT have leading white space
 */
int
get_str(
        xbuf *buf,
        char *str,
        int maxlen)
{
        int ch = 0;
        int len = 0;

        for(len = 0; len < maxlen ; len++)
        {
                ch = nextc(buf);
                if(!(isascii(ch) && isgraph(ch)))
                {
                        unnextc(buf, ch);
                        break;
                }
                *str++ = (char)ch;
        }
        *str = 0;
        return ch;
}


/*
 * skip leading white space, get the next alphanumeric token
 */
int
get_wstr(
        xbuf *buf,
        char *str,
        int maxlen)
{
        int ch;
        do{
                ch = nextc(buf);
        }while((isascii(ch) && !isgraph(ch)));
        unnextc(buf,ch);
        return( get_str(buf, str, maxlen) );
}


/*
 * get the next numeric token
 * may NOT have leading white space
 */
int
get_num(
        xbuf *buf,
        int *num, /* returns scanned value or -1 */
        int maxlen)
{
        int ch;
        int len;
        int accum = 0;

        for(len = 0; len < maxlen ; len++ )
        {
                ch = nextc(buf);
                if(!(isascii(ch) && isdigit(ch)))
                {
                        unnextc(buf, ch);
                        break;
                }
                if(accum) accum *= 10;
                accum += (ch - '0'); /* N.B. */
        }
        *num = len ? accum : -1;
        return ch;
}


/*
 * skip leading white space, get the next numeric token
 */
int
get_wnum(
        xbuf *buf,
        int *num,
        int maxlen)
{
        int ch;
        do{
                ch = nextc(buf);
        }while(Wisspace(ch));
        unnextc(buf,ch);
        return( get_num(buf, num, maxlen) );
}


/*
 * get the next numeric token, handle solidus
 * may NOT have leading white space
 */
int
dget_num(
        xbuf *buf,
        int *num,
        int maxlen)
{
        int ch;
        int len;
        int accum = 0;
        int failnum = -1;

        for(len = 0; len < maxlen ; len++ )
        {
                ch = nextc(buf);
                if(!(isascii(ch) && isdigit(ch)))
                {
                        if( ch == '/' ) /* handle the solidus */
                        {
                                accum = failnum;
                                continue;
                        }
                        /* else */
                        unnextc(buf,ch);
                        break;
                }
                if(accum) accum *= 10;
                accum += (ch - '0'); /* N.B. */
        }
        *num = len ? accum : failnum;
        return ch;
}


/*
 * skip leading white space, get the next numeric token, handle solidus
 */
int
dget_wnum(
        xbuf *buf,
        int *num,
        int maxlen)
{
        int ch;
        do{
                ch = nextc(buf);
        }while(Wisspace(ch));
        unnextc(buf,ch);
        return( dget_num(buf, num, maxlen) );
}



/*
 * Skip white space in buf and
 * initialize a clone xbuf that starts at buf's
 * current position and ends just before the next '=' or EOB
 * Used to extract individual obs from reports which contain multiple obs.
 */
int
get_weqxbuf(
        xbuf *buf,
        xbuf *clone)
{
        int ch = 0;

        /* assert(buf != NULL && clone != NULL ); */

        if(buf->cnt == 0 ) return EOB;

        do{
                ch = nextc(buf);
        }while(Wisspace(ch));
        unnextc(buf,ch);

        if(buf->cnt < 5 ) return EOB;

        clone->base = buf->get;
        
        do {
                ch = nextc(buf);
        } while(ch != EQ && ch != RECORD_SEP && ch != EOB);

        clone->bufsiz =  buf->get - clone->base;

        if(clone->bufsiz < 5) return EOB; /* don't bother */

        clone->cnt = (ptrdiff_t) clone->bufsiz;
        clone->get = clone->base;
        clone->put = clone->base + clone->bufsiz - 1;

        /* trim whitespace from end */
        while((clone->put >= clone->base)
                && (Wisspace(*clone->put) || *clone->put == EQ))
        {
                        clone->put--;
                        clone->bufsiz--;
                        clone->cnt--;
        }

        return(ch > 0 ? ch : ETX);
}


/*
 *  scan for literal string. (up to 16 chars)
 * If it is there, swallow it, return !0;
 * else, back up and return 0;
 */
int
hasSTR(
        xbuf *buf,
        char *str)
{
        int ii;
        int cbuf[16];

        for(ii = 0; ii < 16 ; ii++ )
        {
                if(str[ii] == 0) break; /* success */
                cbuf[ii] = nextc(buf);
                if(cbuf[ii] != str[ii])
                {
                        /* unwind */
                        while( ii >= 0 )
                        {
                                if(cbuf[ii] != EOB)
                                        unnextc(buf, cbuf[ii]);
                                ii--;
                        }
                        return 0;
                }
        }
        return !0;
}


int
whasSTR(
        xbuf *buf,
        char *str)
{
        int ch;
        do{
                ch = nextc(buf);
        }while(Wisspace(ch));
        unnextc(buf,ch);
        return( hasSTR(buf,str) );
}
