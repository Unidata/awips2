/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ldmprint.c,v 1.49.12.3 2008/04/15 16:34:11 steve Exp $ */

/* 
 * Utility functions for printing contents of some protocol data structures
 */
#include <ldmconfig.h>

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <rpc/rpc.h>

#include "ldm.h"
#include "atofeedt.h"           /* for fassoc[] */
#include "log.h"

static char tprintbuf[1984];
static const char nada[] = "(null)";


/*
 * Fill in buf with a printed version of ts.
 */
int
sprint_time_t(char *buf, size_t bufsize, time_t ts)
{
        struct tm tm_ts;
        size_t len;

#define P_TIMET_LEN 15 /* YYYYMMDDHHMMSS\0 */
        if(!buf || bufsize < P_TIMET_LEN)
                return -1;

        tm_ts = *(gmtime(&ts));
        len = strftime(buf, bufsize,
                        "%Y%m%d%H%M%S", &tm_ts);
        return (int)len;
}


int
sprint_timestampt(char *buf, size_t bufsize, const timestampt *tvp)
{
        int len;

#define P_TIMESTAMP_LEN (P_TIMET_LEN + 4)
        if(!buf || bufsize < P_TIMESTAMP_LEN)
                return -1;

        if(tvp->tv_sec == TS_NONE.tv_sec && tvp->tv_usec == TS_NONE.tv_usec)
        {
                len = sprintf(buf, "TS_NONE");
                return len;
        }
        /* else */
        if(tvp->tv_sec == TS_ZERO.tv_sec && tvp->tv_usec == TS_ZERO.tv_usec)
        {
                len = sprintf(buf, "TS_ZERO");
                return len;
        }
        /* else */
        if(tvp->tv_sec == TS_ENDT.tv_sec && tvp->tv_usec == TS_ENDT.tv_usec)
        {
                len = sprintf(buf, "TS_ENDT");
                return len;
        }
        /* else */
        
        len = sprint_time_t(buf, bufsize, (time_t)tvp->tv_sec);
        /* assert(len == 14) */
        /* we are only printing the microsecs to millisec accuracy */
        (void)sprintf(&buf[len],".%03d", (int)(tvp->tv_usec/1000));
                        
        len += 4;
        return len;
}


/*
 * TODO: needs work
 *
 * @return               The number of characters encoded.
 * @return -1            if the buffer is NULL or the buffer size is less than
 *                       129.
 * @return -1            if feedtype table doesn't contain "any".
 * @return -1            if the given feedtype can't be encoded.
 */
int
sprint_feedtypet(char *buf, size_t bufsize, feedtypet feedtype)
{
        static struct fal *fap = 0;
        struct fal *ap;
        char *sp = buf;
        int len;

/* max len of any feedtype expression we will construct */
#define FDTT_SBUF_SIZE (128)
        if(buf == NULL || bufsize < FDTT_SBUF_SIZE +1)
                return -1;
        (void) memset(buf, 0, bufsize);

        if (feedtype == NONE)
          return sprintf(buf, "%s", "NONE");


        if (fap == 0) {         /* compute and save for subsequent calls */
            /* find the most inclusive feedtype, to work backwards from */
            for(fap = fassoc; fap->type != ANY && fap->name != NULL; )
                fap++;
            if(fap->name == NULL) {
                /* who took ANY out of the feedtype name table? */
                return -1;
            }
        }

        ap = fap;               /* start at ANY and work backwards */
        *sp = '\0';
        while (feedtype && ap->type != NONE) {
            if ((ap->type & feedtype) == ap->type) { /* includes this one */
                (void)strncat(sp, ap->name, bufsize - 1 - strlen(buf) );
                (void)strncat(sp, "|", bufsize - 1 - strlen(buf) );
                sp += strlen(ap->name)+1;
                feedtype &= ~ap->type;
            }
            ap--;
        }

        /* Capitalize it */
        for( sp=buf; *sp && sp < &buf[bufsize - 1]; sp++)
            *sp = (char)(islower(*sp) ? toupper(*sp) : *sp);

        if (feedtype) {
            /* handle error, some unnamed bits in there */
            (void)sprintf(sp, "0x%08x|", (unsigned)feedtype);
        }

        len = (int)strlen(buf)-1;
        buf[len] = '\0';        /* chop trailing "|" */
        return len;
}

char *
s_feedtypet(feedtypet feedtype)
{
        static char buf[FDTT_SBUF_SIZE +1];
        if(sprint_feedtypet(buf, sizeof(buf), feedtype) <= 0)
                return NULL;
        return buf;
}

static int
sprint_ldm_addr_rpc(char *buf, size_t bufsize, const ldm_addr_rpc *rdv) 
{
        if(buf == NULL)
        {
                buf = tprintbuf;
                bufsize = sizeof(tprintbuf);
        }
#define RA_SBUF_SIZE (HOSTNAMESIZE +1 +11+1 +11+1)
        if(bufsize < RA_SBUF_SIZE + 1)
                return -1;
        (void) memset(buf, 0, bufsize);

        if(rdv == NULL)
                return sprintf(buf, "%s", nada);
        
        /* else */
        return sprintf(buf, "%s %11lu %11lu",
                        rdv->hostname, 
                        rdv->prog,
                        rdv->vers
                        );
}

static const char *
s_proto(int protocol)
{
#define PROTO_SBUF_SIZE 3
        switch (protocol) {
        case IPPROTO_TCP:
                return "tcp";
        case IPPROTO_UDP:
                return "udp";
        }
        return "UNK";
}

static int
sprint_ldm_addr_ip(char *buf, size_t bufsize, const ldm_addr_ip *rdv) 
{
        if(buf == NULL)
        {
                buf = tprintbuf;
                bufsize = sizeof(tprintbuf);
        }
#define RI_SBUF_SIZE (PROTO_SBUF_SIZE + 1 + 5 + 1 + 16)
        if(bufsize < RI_SBUF_SIZE + 1)
                return -1;
        (void) memset(buf, 0, bufsize);

        if(rdv == NULL)
                return sprintf(buf, "%s", nada);
        
        /* else */
        /* TODO: dotted quad print of rdv->addr */
        return sprintf(buf, "%s %5hu 0x%08lx",
                        s_proto(rdv->protocol), 
                        rdv->port,
                        rdv->addr);
}


char *
s_rendezvoust(char *buf, size_t bufsize, const rendezvoust *rdv) 
{
        if(rdv != NULL)
        {
                switch (rdv->type) {
                case LDM_ADDR_RPC:
                        if(sprint_ldm_addr_rpc(buf, bufsize,
                                        &rdv->rendezvoust_u.rpc) > 0)
                                return buf;
                        break;
                case LDM_ADDR_IP:
                        if(sprint_ldm_addr_ip(buf, bufsize,
                                        &rdv->rendezvoust_u.ip) > 0)
                                return buf;
                        break;
                case LDM_ADDR_NONE:
                default:
                        break;
                }
        }
        (void) sprintf(buf, "%s", nada);
        return buf;
}


int
sprint_signaturet(char *buf, size_t bufsize, const signaturet signature)
{
    int    len = 0;
    size_t sigLen = sizeof(signaturet);

    if(buf != NULL && bufsize >= 2*sigLen + 1) {
        char                *bp = buf;
        const unsigned char *sp = (unsigned char*)signature;;

        while (sp < (unsigned char*)signature + sigLen)
            bp += sprintf(bp, "%02x", *sp++);

        len = (int)(bp - buf);

        assert(len == 2*sigLen);
    }

    return len;
}


/*
 * Returns a string representation of a product signature.  If the user-
 * supplied buffer is NULL, then a static, internal buffer is used.  This
 * buffer is overwritten by each, successive call.
 *
 * buf                 The buffer into which to put the string representation
 *                     or NULL.
 * bufsize             The size of the buffer in bytes.
 * signaturep          The signature whose string representation is desired.
 *
 * Return
 *   NULL              If the user-supplied buffer is too small.
 *   <else>            A pointer to the buffer containing the string
 *                     representation.
 */
char* s_signaturet(char *buf, size_t bufsize, const signaturet signaturep)
{
        if(buf == NULL)
        {
                buf = tprintbuf;
                bufsize = sizeof(tprintbuf);
        }
        
        {
                const int len = sprint_signaturet(buf, bufsize, signaturep);
                if(len < 2 * sizeof(signaturet))
                        return NULL;
        }
        return buf;
}


/*
 * Parses a formatted signature.
 *
 * Arguments:
 *      string  Pointer to the formatted signature.
 * Returns:
 *      -1      Failure.  log_errno() called.
 *      else    Number of bytes parsed.
 */
int
sigParse(
    const char* const   string,
    signaturet* const   signature)
{
    int                 i;
    signaturet          tmpSig;
    int                 nbytes;

    errno = 0;

    for (i = 0; i < sizeof(signaturet); i++) {
        unsigned        value;

        if (sscanf(string + 2*i, "%2x", &value) != 1)
            break;

        tmpSig[i] = (unsigned char)value;
    }

    if (i != sizeof(signaturet)) {
        log_errno();
        nbytes = -1;
    }
    else {
        (void)memcpy(signature, tmpSig, sizeof(signaturet));
        nbytes = 2*sizeof(signaturet);
    }

    return nbytes;
}


int
sprint_prod_spec(char *buf,
        size_t bufsize, const prod_spec *specp)
{
        size_t len = 0;
        int conv;
#define MIN_PSPECLEN (1 + 7 + 1 + 1 + 1 + 1 +1)
#define MAX_PSPECLEN (MIN_PSPECLEN + MAXPATTERN)
        if(buf == NULL || (bufsize < MAX_PSPECLEN
                        && bufsize < MIN_PSPECLEN
                         + strlen(specp->pattern)))
                return -1;

        if(specp == NULL)
                return sprintf(buf, "%s", nada);

        buf[0] = '{';
        len++;
        bufsize--;

        conv = sprint_feedtypet(&buf[len], bufsize, specp->feedtype);
        if(conv < 1)
                return (int)len;
        len += (size_t) conv;
        bufsize -= len;

        if(specp->pattern == NULL)
                conv = sprintf(&buf[len], ",  \"%s\"}", nada);
        else
                conv = sprintf(&buf[len], ",  \"%s\"}", specp->pattern);
        if(conv < 1)
                return (int)len;
        len += (size_t) conv;
        /* bufsize -= len; */
        return (int)len;
}


char *
s_prod_class(char *buf,
        size_t bufsize, const prod_class_t *clssp)
{
        size_t len = 0;
        int conv;
        int ii;

        if(buf == NULL)
        {
                buf = tprintbuf;
                bufsize = sizeof(tprintbuf);
        }

        if(bufsize < 2 * P_TIMESTAMP_LEN + MAX_PSPECLEN) return NULL;

        if(clssp == NULL)
        {
                (void)sprintf(buf, "%s", nada);
                return buf;
        }
        
        conv = sprint_timestampt(&buf[len], bufsize, &clssp->from);
        if(conv < 1)
                return buf;
        len += (size_t)conv;
        bufsize -= len;

        conv = sprintf(&buf[len]," ");
        len += (size_t)conv;
        bufsize -= (size_t)conv;

        conv = sprint_timestampt(&buf[len], bufsize, &clssp->to);
        if(conv < 1)
                return buf;
        len += (size_t)conv;
        bufsize -= (size_t)conv;
        
        conv = sprintf(&buf[len]," {");
        len += (size_t)conv;
        bufsize -= (size_t)conv;

        {
        const u_int last = clssp->psa.psa_len;
        for(ii = 0; bufsize > 0 && ii < last; /* incr below */)
        {
                conv = sprint_prod_spec(&buf[len], bufsize,
                        &clssp->psa.psa_val[ii]);
                if(conv < 1)
                        return buf;
                len += (size_t)conv;
                bufsize -= (size_t)conv;
                if(++ii == last) /* incr */
                        break;
                /* else */
                if(bufsize > 1)
                {
                        conv = sprintf(&buf[len],",");
                        len += (size_t)conv;
                        bufsize -= (size_t)conv;
                }
                
        }
        }

        if(bufsize > 1)
        {
                conv = sprintf(&buf[len],"}");
#if 0
                len += (size_t)conv;
                bufsize -= (size_t)conv;
#endif
        }

        return buf;
}


char *
s_prod_info(char *buf, size_t bufsize, const prod_info *infop,
        int doSignature)
{
        int conv;
        size_t len = 0;

        if(buf == NULL)
        {
                buf = tprintbuf;
                bufsize = sizeof(tprintbuf);
        }

        if((doSignature && bufsize < 33 + 50 + KEYSIZE)
                         || bufsize < 50 + KEYSIZE)
                 return NULL;

        (void) memset(buf, 0, bufsize);

        if(doSignature)
        {
                conv = sprint_signaturet(&buf[len], bufsize, infop->signature);
                len += (size_t)conv;
                bufsize -= (size_t)conv;
                buf[len++] = ' ';
                buf[len] = 0;
                bufsize--;
        }

        conv = sprintf(&buf[len],"%8u ", infop->sz);
        len += (size_t)conv;
        bufsize -= (size_t)conv;

        conv = sprint_timestampt(&buf[len], bufsize, &infop->arrival);
        len += (size_t)conv;
        bufsize -= (size_t)conv;
                        
        conv = sprintf(&buf[len]," %7s %03u  %s",
                        s_feedtypet(infop->feedtype),
                        infop->seqno,
                        infop->ident);
        return buf;
}


char *
s_ldm_errt(ldm_errt code)
{
        switch (code) {
        case OK : return "OK";
        case SHUTTING_DOWN : return "SHUTTING_DOWN";
        case DONT_SEND : return "DONT_SEND";
        case RESTART : return "RESTART";
        case REDIRECT : return "REDIRECT";
        case RECLASS : return "RECLASS";
        }
        /* default */
        return "";
}


char *
s_ldmproc(unsigned long proc)
{
        switch (proc) {
        case 0  : return "NULLPROC";
        case FEEDME : return "FEEDME";
        case HIYA : return "HIYA";
        case NOTIFICATION : return "NOTIFICATION";
        case NOTIFYME : return "NOTIFYME";
        case COMINGSOON : return "COMINGSOON";
        case BLKDATA : return "BLKDATA";
        }
        /* default */
        {
                static char buf[24];
                (void)sprintf(buf, "%ld", proc);
                return buf;
        }
}

#ifdef TEST_S_FEEDTYPET
#include <stdio.h>

int
main()
{
    feedtypet in;

    while ( scanf("%d", &in) != EOF ) {
        printf("%d\t0x%08x\t\t%s\n", in, in, s_feedtypet(in));
    }
    return 0;
}

#endif
