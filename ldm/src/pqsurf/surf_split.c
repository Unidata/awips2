/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: surf_split.c,v 1.41.18.2 2008/03/13 15:25:08 steve Exp $     */

#include <ldmconfig.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "ldm.h"
#include "ulog.h"
#include "wmo_header.h"
#include "tokens.h"
#include "xbuf.h"
#include "surface.h" /* wind_units_t, CALL_SIGN_LEN */

#include "md5.h"

static double md5ctx[16]; /* 88 would be big enough */
static MD5_CTX *md5ctxp = (MD5_CTX *)md5ctx;

extern int usePil;  /* 1/0 flag to signal use of AFOS like pil identifier */


static int
get_yygg(xbuf *buf, dtime *time)
{
        int status;
        int YY = -1;
        int GG = -1;

        if((status = dget_wnum(buf, &YY, 2)) < 0) return status;
        if((status = dget_num(buf, &GG, 2)) < 0) return status;

        set_dtime(time, YY, GG, 0);
        return status;
}

/* For METAR, check if the HHMMZ time string is present */
static int
whas_yyggZ(xbuf *buf)
{
        int ch;

        /* skip white space */
        do{
                ch = nextc(buf);
        }while((isascii(ch) && !isgraph(ch)));
        unnextc(buf,ch);

        if(buf->cnt < 4) {
                return 0; /* not enough characters */
        } else if(buf->get[4] == 'Z' || buf->get[6] == 'Z' ) {
                return 1;
        } else if( isdigit(buf->get[3])
                && isdigit(buf->get[2])
                && isdigit(buf->get[1])
                && isdigit(buf->get[0])) {
                return 1;
        } else if( isdigit(buf->get[5])
                && isdigit(buf->get[4])
                && isdigit(buf->get[3])
                && isdigit(buf->get[2])
                && isdigit(buf->get[1])
                && isdigit(buf->get[0])) {
                return 1;
        }
        return 0; /* failed */
}

/* For METAR, check if "NIL" */
static int
has_NIL(xbuf *buf)
{
        char nilstr[]  = "NIL";
        char *np = (char *)&buf->base[buf->bufsiz - 1 - (sizeof(nilstr) -1 -1)];

        if(strncmp(np, nilstr, sizeof(nilstr) -1) == 0)
                return 1;
        return 0;
}

/* For METAR, get the bulletin time, if possible */
static void
get_wyyggZ(xbuf *buf, dtime *time)
{
        if(!whas_yyggZ(buf))
                return;
        (void)get_yygg(buf, time);
        (void)nextc(buf); /* eat the 'Z' */
        return;
}


/*
 *      Takes a WMO format product which is a
 *   SAO, SYNOP, SHIP, METAR, or SPECI message, splits it into
 *   individual observations. The observations are each encapsulated in a
 *   new product which inherits most of its description from the
 *   original product.
 *  The new product pkey is derived from the observation type
 *   and has the following form:
 *
 *              SAO -   "sao tt ccc ddhhmm"
 *                      where:
 *                              tt is SA, SP or RS 
 *                              ccc is the station ID like SFO, LXV, etc
 *                              ddhhmm is the time stamp.
 *
 *              SYNOP - "aaxx nnnnn ddhhmm"
 *                      where:
 *                              nnnnn is the WMO station id (5 digit number)
 *
 *              SHIP -  "bbxx c* ddhhmm"
 *                      where:
 *                              c* is the call sign
 *
 *              METAR - "metar cccc ddhhmm"
 *                      where:
 *                              cccc is the call sign
 *
 *              SPECI - "speci cccc ddhhmm" 
 *
 *  The new product sequence number is original sequence number times 1000
 *   plus the sequence of the individual observation within the product.
 *
 *      'doit' is called on each of the new products. It is presumed
 * this function return  zero upon success.
 * 
 *  Returns the number of successful calls to 'doit', eg, the
 *  number of splits. Returns -1 on error.
 */
int
surf_split(const prod_info *infop, const void *datap,
                int (*doit)(const prod_info *, const void *))
{
        wmo_header_t hdr;
        message_type_t mtype;
        dtime dt;
        xbuf buf[1];
        static unsigned char*   dbuf = NULL;
        static size_t           dbufSize = 0;
        char header[50];
        int nsplit = 0;

        enum {
                SURFACE_BOGUS ,
                AAXX,
                US_AAXX,
                BBXX,
                SAO,
                sMETAR,
                sSPECI
        } subtype = SURFACE_BOGUS;

        hdr.time = &dt;

        if(infop->sz > dbufSize || dbuf == NULL) {
                size_t  size = infop->sz != 0 ? infop->sz : 1;

                if (dbuf != NULL)
                    free(dbuf);

                dbuf = malloc(size);

                if (dbuf == NULL) {
                    serror("surf_split(): Couldn't allocate %lu-byte buffer",
                        (unsigned long)size);
                    dbufSize = 0;
                    return -1;
                }

                dbufSize = size;
        }

        memcpy(dbuf, datap, infop->sz);

        if( cbuftoxbuf(buf, dbuf, infop->sz) == NULL)
                return -1;
        
        skipline(buf, 4); /* SOH */
        skipline(buf, 12); /* start */

        /* inspect header as a string to see if to use PIL processing or not */
        if(infop->sz >  48 ) {
                memcpy( header, datap, 48 );
                header[48] = '\0';
        } else {
                memcpy( header, datap, infop->sz );
                header[infop->sz] = '\0';
        }
        if( strstr( header, "METAR\r" ) || strstr( header, "SPECI\r" ) ||
                strstr( header, "MTR" ) ||
                strstr( header, "METAR \r" ) || strstr( header, "SPECI \r" ) ||
                strstr( header, "BOYC" )) { 
                usePil = 1;
        } else {
                usePil = 0;
        }
        if( get_wmo_header(buf, &hdr) == NULL)
        {
                uerror("get_wmo_header: hdr: %s\n", hdr);
                return -1;
        } 
        usePil = 1;
#if DEBUG
        fputs("\t", stderr);
        fprint_wmo_header(stderr, &hdr);
        fputs("\n", stderr);
#endif

        mtype = decode_type(hdr.TT,hdr.AA,hdr.PIL);
        
        /* #### */
        {
        char cbuf[8];
        int digit;
        dtime time;
        wind_units_t wind_units = WIND_UNAVAIL;

        time = *hdr.time; /* default the ob time to the time in the header */

        /* delve into section 0 */

        switch(mtype) {
        case SYNOP :
                if(get_wstr(buf, cbuf, 1) < 0 ) return -1;
                if(cbuf[0] == 'A')
                {
                        subtype = AAXX;
                        if(get_str(buf, &cbuf[1], 3) < 0 ) return -1;
                        if( cbuf[3] != 'X' )
                        {
                                /* punt */
                                uerror("surface_split: Unknown SYNOP type: %s\n",
                                    cbuf);
                                return 0;
                        }
                        if(get_yygg(buf, &time) < 0 ) return -1; /* YYGG */
                        if(dget_num(buf, &digit, 1) < 0 ) return -1; /* isubw */
                        if(digit >= 0 && digit <= 4) wind_units = (wind_units_t)digit;
                }
                else if(isascii(cbuf[0]) && isdigit(cbuf[0])) /* US Stations 7NNNN */
                {
                        unnextc(buf,cbuf[0]);
                        subtype = US_AAXX;
                        /* 
                         * Some US reports leave off AAXX YYGGisubw, so we use the
                         * time from the wmo header. 
                         */
                        wind_units = KNOTS;
                }
                else
                {
                        unnextc(buf,cbuf[0]);
                        return 0; /* ?? */
                }
                break;
        case SHIP :
                if (hdr.PIL[0] != '\0' && whasSTR(buf, "BOYC")) {
                    skipline(buf, 4);
                }
                if(get_wstr(buf, cbuf, 4) < 0 ) return -1;
                if(cbuf[0] == 'B')
                {
                        if( cbuf[3] != 'X' )
                        {
                                /* punt */
                                uerror("surface_split: Unknown SHIP type: %s\n", cbuf);
                                return 0;
                        }
                        subtype = BBXX;
                        /* get time below */
                }
                else
                {
                        unnextc(buf,cbuf[0]);
                        return 0;
                }
                break;
        case METAR :
                if( hdr.PIL[0] != '\0' && 
                        (! strstr(hdr.PIL, "METAR") || 
                        !strstr(hdr.PIL, "MTR"))){
                        subtype = sMETAR;
                } else if(whasSTR(buf, "METAR"))
                {
                        subtype = sMETAR;
                        get_wyyggZ(buf, &time);
                }
                else if(whasSTR(buf, "SPECI"))
                {
                        subtype = sSPECI;
                        get_wyyggZ(buf, &time);
                }
                else {
                        subtype = SAO; /* may actually be a METAR, check below */
                }
                break;  
        case SPECI :
                if(whasSTR(buf, "SPECI"))
                {
                        subtype = sSPECI;
                        get_wyyggZ(buf, &time);
                }
                break;  
        default :
                uerror("surface_split: Can't handle %s", 
                        sMessage_type(mtype) );
                uerror("HDR + PIL: %s%s %s", hdr.TT, hdr.AA, hdr.PIL ) ;
                return -1;
        }

        { /* while block */
        static char newkey[KEYSIZE];
        xbuf subbuf[1];
        prod_info newinfo = *infop;
#define MAX_SURF_LEN 511
#undef MIN
#define MIN(a,b) ((a) <= (b) ? (a) : (b))
        char pbuf[MAX_SURF_LEN + 1];
        int l1, l2;
        static char ident[CALL_SIGN_LEN+1];
        static char type[4];
        char *stype;
        u_int subseq = infop->seqno * 1000; 
        unsigned char *pp;

        while( get_weqxbuf(buf, subbuf) > 0 )
        {
                (void)memset(newkey,0,KEYSIZE);
                (void)memset(pbuf,0,MAX_SURF_LEN + 1);
                (void)memset(ident,0,CALL_SIGN_LEN+1);
                pp = subbuf->base;

                switch(subtype) {
                case AAXX :
                case US_AAXX :
                        strcpy(newkey, "aaxx ");
                        strcpy(pbuf, "AAXX");
                        sprintf(&pbuf[strlen(pbuf)], " %02d%02d%1d\r\r\n",
                                time.mday, time.hour, (int)wind_units);
                                        /* WMO station no. */
                        if(get_wstr(subbuf, ident, 5) < 0)
                                continue;
                        strcat(newkey, ident);
                        break;
                case BBXX :
                        strcpy(newkey, "bbxx ");
                        strcpy(pbuf, "BBXX\r\r\n");
                        /* call sign */
                        if(get_wstr(subbuf, ident, CALL_SIGN_LEN) < 0)
                                continue;
                        strcat(newkey, ident);
                        if(get_yygg(subbuf, &time) < 0) continue; /* YYGG */
                        break;
                case sSPECI :
                        /* call sign */
                        if(get_wstr(subbuf, ident, CALL_SIGN_LEN) < 0)
                                continue;
                        stype = "SPECI";
                        if(strcmp(ident, "METAR") == 0 ||
                                strcmp(ident, "SPECI") == 0)
                        {
                                if( strcmp(ident, "METAR") == 0) {
                                        stype = "METAR";
                                }
                                /* They package each ob with a tag */
                                pp = (subbuf->get +1);
                                if(get_wstr(subbuf, ident, CALL_SIGN_LEN) < 0)
                                continue;
                        }
                        if(!whas_yyggZ(subbuf))
                        {
                                /* Have to insert the date */
                                sprintf(pbuf, "%s\r\r\n%s %02d%02dZ ",
                                        stype, ident, time.hour, time.min);
                                pp = subbuf->get;
                        }
                        else {
                                strcpy(pbuf, stype);
                                strcat(pbuf, "\r\r\n");
                        }
                        if(strcmp(stype, "METAR") == 0 ) {
                                strcpy(newkey, "metar ");
                        } else {
                                strcpy(newkey, "speci ");
                        }
                        strcat(newkey, ident);
                        break;
                case sMETAR :
                        if(has_NIL(subbuf))
                                continue;
                        /* call sign */
                        if(get_wstr(subbuf, ident, CALL_SIGN_LEN) < 0) {
                                continue;
                        }
                        stype = "METAR";
                        if(strcmp(ident, "METAR") == 0 ||
                                strcmp(ident, "SPECI") == 0)
                        {
                                if( strcmp(ident, "SPECI") == 0) {
                                        stype = "SPECI";
                                }
                                /* They package each ob with a tag */
                                pp = (subbuf->get +1);
                                if(get_wstr(subbuf, ident, CALL_SIGN_LEN) < 0)
                                continue;
                        }
                        if(!whas_yyggZ(subbuf))
                        {
                                /* Have to insert the date */
                                sprintf(pbuf, "%s\r\r\n%s %02d%02dZ ",
                                        stype, ident, time.hour, time.min);
                                pp = subbuf->get;
                        }
                        else {
                                strcpy(pbuf, stype);
                                strcat(pbuf, "\r\r\n");
                        }
                        if(strcmp(stype, "METAR") == 0 ) {
                                strcpy(newkey, "metar ");
                        } else {
                                strcpy(newkey, "speci ");
                        }
                        strcat(newkey, ident);
                        break;
                case SAO :
                        /* call sign */
                        if(get_wstr(subbuf, ident, CALL_SIGN_LEN) < 0)
                                continue;
                        if(hdr.AA[0] == 'U' && hdr.AA[1] == 'S'
                                        && strlen(ident) == 6)
                        {
                                /* skip 6 char US "AFOS code" */
                                if(get_wstr(subbuf, ident, CALL_SIGN_LEN) < 0)
                                        continue;
                        }
                                
                        /* SA, SP, RS, USP or XP */
                        if(get_wstr(subbuf, type, 3) < 0)
                                continue;
                        if((type[0] == 'S'
                                         && (type[1] == 'A' || type[1] == 'P'))
                                || (type[0] == 'R' && type[1] == 'S')
                                || (type[0] == 'U' && type[1] == 'S'
                                         && type[2] == 'P')
                                || (type[0] == 'X' && type[1] == 'P')
                                || (type[0] == 'T' &&
                                         (type[1] == 'A' || type[1] == 'S'))
                                )
                        {
                                strcpy(newkey, "sao ");
                                strcat(newkey, type);
                                strcat(newkey, " ");
                                strcat(newkey, ident);
                        } 
                        else if(isdigit(type[0]) && isdigit(type[1]))
                        {
                                /* it is a METAR really */
                                subtype = sMETAR;
                                strcpy(newkey, "metar ");
                                strcat(newkey, ident);
                                strcpy(pbuf, "METAR\r\r\n");
                        }
                        else
                                continue; /* don't know what it is, "NIL=" */
                        break;
                }

                /* safety net */
                if(strlen(ident) == 0)
                {
                        continue;
                }
                /* else */

                sprintf(&newkey[strlen(newkey)], " %02d%02d%02d",
                        time.mday, time.hour, time.min);
                if(hdr.retransmit != ORIGINAL)
                        sprintf(&newkey[strlen(newkey)], " %s",
                                sRetransmit(&hdr));
                newinfo.ident = newkey;
                newinfo.seqno = ++subseq;

                l1 = strlen(pbuf);
                l2 = MIN(MAX_SURF_LEN - l1 - 4, subbuf->bufsiz - (pp - subbuf->base));
                /* N.B.: silent truncation */
                strncat(pbuf, (char *)pp, l2 );
                strcat(pbuf,"=\r\r\n");

                newinfo.sz = l1 + l2 + 4;

#if DEBUG
                fprintf(stderr,"\t\t%s\n", newinfo.ident);
#endif
                
#if PRINT
                {
                        char *cp = pbuf;
                        char *end =  &cp[newinfo.sz];
                        while(cp < end)
                        {
                                putc(*cp, stderr);
                                cp++;
                        }
                }
                putc('\n', stderr);
#endif

                MD5Init(md5ctxp);
                MD5Update(md5ctxp, (const unsigned char *)pbuf, newinfo.sz);
                MD5Final((unsigned char*)newinfo.signature, md5ctxp);
                
                /*
                 * process the single ob in the requested fashion
                 */
                if((*doit)(&newinfo, pbuf) == 0)
                        nsplit++;

        } /* end while */

#if PRINT
                putc('\n', stderr);
#endif
        } /* end while block */
        } /* end #### block */

        return nsplit;
}
