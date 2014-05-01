/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: wmo_header.c,v 1.35.18.2 2007/06/01 20:42:47 steve Exp $ */

#include "ldmconfig.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "ldmalloc.h"
#include "tokens.h"
#include "ulog.h"
#include "wmo_header.h"
#include "xbuf.h"

int usePil=0;


void
free_wmo_header(wmo_header_t *hdr)
{
        if(hdr == NULL) return;
        free_dtime(hdr->time);
        free(hdr);
}


static void
clear_wmo_header(wmo_header_t *hdr)
{
        hdr->TT[0]  = hdr->AA[0] = hdr->ii = 
                hdr->CCCC[0] = hdr->PIL[0] = 0;
        hdr->retransmit = ORIGINAL;
        hdr->retrans_seq = MESSAGE_TYPE_UNKNOWN;
        clear_dtime(hdr->time);
}


wmo_header_t *
get_wmo_header(xbuf *buf, wmo_header_t *hdr)
{

        clear_wmo_header(hdr);

        if( get_wstr(buf, hdr->TT, 2) == EOB ) return NULL;
        if( get_wstr(buf, hdr->AA, 2) == EOB ) return NULL;
        if( get_wnum(buf, &hdr->ii, 2) == EOB ) return NULL;
        if(hdr->ii == -1) hdr->ii = 0;
        if( get_wstr(buf, hdr->CCCC, 4) == EOB ) return NULL;

        {
        int YY, GG, gg;
        if( get_wnum(buf, &YY, 2) == EOB ) return NULL;
        if( get_wnum(buf, &GG, 2) == EOB ) return NULL;
        if( get_wnum(buf, &gg, 2) == EOB ) return NULL;
        /* uses current time on parse errors */
        hdr->time = set_dtime(hdr->time, YY,GG,gg);
        }


        /* decode BBB feild */
        { /* inline */
        char line[16];
        char pilstr[16];
        int allnum, nonalph, padchar, ich, ch, linelen;
        
        if( get_wline(buf, line, sizeof(line)) == EOB ) return NULL;

        { /* inline inner */
        const char *cp = &line[0];
        const char *const end = &line[strlen(line)];
        

        /* N.B. twisted flow here */
        for(cp = &line[0]; *cp != 0 && *cp != CR && cp + 2 < end; cp++)
        {
                switch( cp[0] ) {
                case 'R' :
                        if(cp[1] == 'R')
                        {
                                hdr->retransmit = RTD;          
                                hdr->retrans_seq = cp[2];               
                        }
                        else if(cp[1] == 'T')
                        {
                                hdr->retransmit = RTD;          
                        }
                        break;
                case 'C' :
                        if(cp[1] == 'C')
                        {
                                hdr->retransmit = COR;          
                                hdr->retrans_seq = cp[2];               
                        }
                        else if(cp[1] == 'O')
                        {
                                hdr->retransmit = COR;          
                        }
                        break;
                case 'A' :
                        if(cp[1] == 'A')
                        {
                                hdr->retransmit = AMD;          
                                hdr->retrans_seq = cp[2];               
                        }
                        else if(cp[1] == 'M')
                        {
                                hdr->retransmit = AMD;          
                        }
                        break;
                case 'P' :
                        hdr->retransmit = PIE;          
                        hdr->retrans_seq = (cp[1] - 'A') * 26
                                 + (cp[2] - 'A');
                        goto done;
                case 0 :
                        break;
                default :
                        continue; /* loop, look some more */
                }
                /* only arrrive here if we got an acceptable BBB string */
                if(hdr->retrans_seq == 0)
                {
                        int tmp;
                        /* see if they used old style, eg RTD01 */
                        cp += 3;
                        tmp = atoi(cp);
                        /* Attachment II-14 */
                        if(tmp >= 0 && tmp < 24 )
                                hdr->retrans_seq = tmp + 'A';
                        else
                                hdr->retrans_seq = 'Y';
                }
done:
                break;
        } /* end for */
        } /* end inline inner */
        if(usePil == 1)
        {
            int icnt = 0;
            /*
            * Accept as a PIL, a line that is exactly 6 or 10 characters
            * (including padded spaces!) if there are no intervening
            * non-alphanumeric characters, the string is not all
            * numeric, and if the string is 10 characters, it must begin
            * with ^NMC.  (Chiz, 12/00)
            */ 
            ich = 0; 
            while((ich < (sizeof(pilstr) - 1)) && ((ch = nextc(buf)) != EOB))
            {
                if((ch == CR) || (ch == NL))
                {
                    unnextc(buf,ch);
                    break;
                }
                pilstr[ich] = ch;
                ich++;
            }

            /* Now put back characters read on buffer (needed for pqsurf) */
            for (icnt = ich-1; icnt >= 0; icnt--)
                unnextc(buf,pilstr[icnt]); 

            pilstr[ich] = '\0';
            if((ich > 3) && (ich < 11))
            {
                padchar = 0; ich--;
                while(( pilstr[ich] == ' ') && (ich >= 0))
                {
                    pilstr[ich] = '\0';
                    padchar++;
                    ich --;
                }

                linelen = padchar + strlen(pilstr);

                if(linelen == 6)
                {
                    nonalph = 0; allnum = 1; 
                    for(ich = 0;ich < strlen(pilstr);ich++)
                    {
                        if(!isalnum(pilstr[ich]))
                            nonalph++;
                        if(!isdigit(pilstr[ich]))
                            allnum = 0;
                    }
                    if((nonalph == 0)&&(allnum == 0))
                        sprintf(hdr->PIL," /p%s\0",pilstr);
                }
                else if((linelen == 10) && (strncmp(pilstr,"^NMC",4) == 0))
                    sprintf(hdr->PIL," /p%s\0",pilstr+4); 
            }
        }
        
        } /* end inline */

        return hdr;
}


char *
sRetransmit(wmo_header_t *hdr)
{
        static char buf[4];
        int seq = hdr->retrans_seq;
        if(hdr->retransmit == PIE)
        {
                if(seq < 0 || seq > 675) /* 26 * 26 -1 */
                        seq = 675;
        }
        else if(seq < 'A' || seq > 'Z')
                seq = 0;
        switch ( hdr->retransmit ) {
        case RTD :
                if(seq)
                {
                        sprintf(buf,"RR%c", seq);
                        return buf;
                }
                /* else */
                return "RTD";
        case COR :
                if(seq)
                {
                        sprintf(buf,"CC%c", seq);
                        return buf;
                }
                /* else */
                return "COR";
        case AMD :
                if(seq)
                {
                        sprintf(buf,"AA%c", seq);
                        return buf;
                }
                /* else */
                return "AMD";
        case PIE :
                {
                        const char c1 = seq/26 + 'A';
                        const char c2 = seq%26 + 'A';
                        sprintf(buf,"P%c%c", c1, c2);
                        return buf;
                }
        }
        /* default */
        return NULL;
}


char *
s_wmo_header(wmo_header_t *hdr)
{
#ifndef KEYSIZE
#define KEYSIZE 255
#endif
        static char sp[KEYSIZE+1];
        char *cp = sp;

        (void) memset(sp,0,sizeof(sp));

        sprintf(cp,
                "%2s%2s%02d %4s",
                hdr->TT, hdr->AA, hdr->ii, hdr->CCCC);
        cp += 11;
        if(hdr->time != NULL)
        {
                sprintf(cp,
                        " %02d%02d%02d",
                        hdr->time->mday, hdr->time->hour,
                        hdr->time->min );
        } 
        else
        {
                sprintf(cp, " DDHHMM");
        }
        cp += 7;

        if(hdr->retransmit != ORIGINAL)
                sprintf(cp, " %s", sRetransmit(hdr));

        if(hdr->PIL[0] != '\0')
           strcat(sp,hdr->PIL);

        return sp;
}


#if 0
int
fprint_wmo_header(FILE *fp, const wmo_header_t *hdr)
{
#if 1
        fprintf(fp,
                "%2s%2s%02d %4s",
                hdr->TT, hdr->AA, hdr->ii, hdr->CCCC);
        if(hdr->time != NULL)
        {
                fprintf(fp,
                        " %02d%02d%02d",
                        hdr->time->mday, hdr->time->hour,
                        hdr->time->min );
        } 
        else
        {
                
                fprintf(fp, " DDHHMM");
        }

        if(hdr->retransmit != ORIGINAL)
                fprintf(fp, " %s", sRetransmit(hdr));
#else
        fputs( s_wmo_header(hdr) , fp );
#endif

        return ferror(fp);
}
#endif


#if USED
static void 
clear_wmo_header(wmo_header_t *hdr)
{
        hdr->TT[0]  = hdr->AA[0] = hdr->ii = 
                hdr->CCCC[0] = hdr->retransmit = hdr->retrans_seq  = 0;
        free_dtime(hdr->time);
        hdr->time = NULL;
}
#endif /* USED */


char *
sMessage_type(message_type_t type)
{
        switch(type) {
        case SYNOP : return "SYNOP";
        case SHIP : return "SHIP";
        case METAR : return "METAR";
        case SPECI : return "SPECI";
        case MESSAGE_TYPE_UNKNOWN : return "MESSAGE_TYPE_UNKNOWN";
        }
        /* default */
        return NULL;
}


message_type_t
decode_type(char *tt, char *aa, char *pil)
{
        if( pil[0] == '/' && pil[1] == 'p' && pil[2] == 'M' && pil[3] == 'T')
        {
                if( pil[4] == 'R' || pil[4] == 'T')
                {
                        uerror("HDR + PIL: %s%s %s", tt, aa, pil ) ;
                        return METAR; 
                }
        }
        if( tt[0] == 'S' || tt[0] == 'F')
        {
                /* Table B1 */
                switch( tt[1] ) {
                case 'I' :
                case 'M' :
                case 'N' :
                        if(aa[0] == 'W' || aa[0] == 'V' )
                        { 
                                /* table C2 */
                                switch( aa[1] ) {
                                case 'A' :
                                case 'B' :
                                case 'C' :
                                case 'D' :
                                case 'E' :
                                case 'F' :
                                case 'J' :
                                case 'X' :
                                        return SHIP;
                                }
                                /* else */
                        }
                        /* else */
                        return SYNOP;
                case 'A' :
                        return METAR; /* might be an 'sao' */
                case 'X' :
                        if(*aa == 'U' && *(aa +1) == 'S')
                                return METAR; /* 'sao' */
                        break;
                case 'P' :
                        return SPECI;
                }
                /* else */
        }
        /* else */
        return MESSAGE_TYPE_UNKNOWN;
        
}
