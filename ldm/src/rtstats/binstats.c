/*
 *   Copyright 1994, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: binstats.c,v 1.12.8.1.2.3.2.3 2005/12/13 00:04:57 steve Exp $ */

/* 
 *
 */

#include <ldmconfig.h>
#include <limits.h> /* PATH_MAX */
#ifndef PATH_MAX
#define PATH_MAX 255
#endif /* !PATH_MAX */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <time.h>

#include "ulog.h"
#include "ldm.h"
#include "ldmalloc.h"
#include "ldmprint.h"
#include "atofeedt.h"
#include "inetutil.h"
#include "timestamp.h"

extern int ldmsend_main(char *statsdata);
extern void ldmsend_clnt_destroy(void);

#ifndef DEFAULT_INTERVAL
#define DEFAULT_INTERVAL        60
#endif

#define DEFAULT_RANDOM          30.0

typedef struct statsbin {
        int needswrite;
        time_t interval;
        timestampt recent;  /* infop->arrival most recent */
        timestampt recent_a; /* reftime (queue time) most recent */
        feedtypet feedtype;
        char origin[HOSTNAMESIZE];
        double nprods;        
        double nbytes;        
        double latency_sum;     
        double max_latency;
        time_t slowest_at;
} statsbin;


/* when you need more, grow by this amount */
#define NGROW   8 
/*
 * Max size of the list.
 * This needs to be big enough that the oldest is beyond any conceivable
 * latency, yet small enough that you can sort it and don't mind
 * the memory usage. It also needs to be a multiple of NGROW.
 */
#define MAXBINS (500*NGROW)
/* number allocated, grows to reach MAXBINS */
static size_t maxbins = 0;
/* number in use, <= maxbins */
static size_t nbins = 0;
static statsbin **binList = NULL;


static char *
s_time(char *buf, size_t bufsize, time_t when)
{
        
        struct tm tm_arriv;

#define P_TIMET_LEN 15 /* YYYYMMDDHHMMSS\0 */
        if(!buf || bufsize < P_TIMET_LEN)
                return buf;

        (void) memset(buf, 0, bufsize);
        tm_arriv = *(gmtime(&when));
        (void)strftime(buf, bufsize,
                        "%Y%m%d%H%M%S", &tm_arriv);
        return buf;
}


static char *
s_time_abrv(time_t when)
{
        
        static char buf[32];
        struct tm tm_arriv;

        (void) memset(buf, 0, sizeof(buf));
        tm_arriv = *(gmtime(&when));
        (void)strftime(buf, sizeof(buf),
                        "%M%S", &tm_arriv);
        return buf;
}


static void
dump_statsbin(statsbin *sb)
{
        char buf[P_TIMET_LEN];
        char buf_a[P_TIMET_LEN];

        unotice("%s %s %s %12.0lf %12.0lf %10.2f %4.0f@%s %s",
                s_time(buf, sizeof(buf), sb->recent.tv_sec),
                s_feedtypet(sb->feedtype),
                sb->origin,
                sb->nprods,
                sb->nbytes,
                sb->latency_sum/(sb->nprods == 0 ? 1 : sb->nprods),
                sb->max_latency,
                s_time_abrv(sb->slowest_at),
                s_time(buf_a, sizeof(buf_a), sb->recent_a.tv_sec)
        );

}


static void
ldmsend_statsbin(statsbin *sb)
{
        char buf[P_TIMET_LEN];
        char buf_a[P_TIMET_LEN];
        char stats_data[4096];
        int status;
        static char myname[HOSTNAMESIZE] = "\0";

        if ( myname[0] == '\0' )
                (void) strcpy(myname, ghostname());

        if(sb->recent_a.tv_sec == -1) return;

        sprintf(stats_data, "%14s %14s %32s %7s %32s %12.0lf %12.0lf %g %10.2f %4.0f@%4s %20s\n\0",
                s_time(buf, sizeof(buf), sb->recent.tv_sec),
                s_time(buf_a, sizeof(buf_a), sb->recent_a.tv_sec),
                myname,
                s_feedtypet(sb->feedtype),
                sb->origin,
                sb->nprods,
                sb->nbytes,
                d_diff_timestamp(&sb->recent_a, &sb->recent),
                sb->latency_sum/(sb->nprods == 0 ? 1: sb->nprods),
                sb->max_latency,
                s_time_abrv(sb->slowest_at),
                ldm_version
        );
        status = ldmsend_main(stats_data);
        if ( status == 0 ) sb->needswrite = 0;
}


static int
fscan_statsbin(FILE *fp, statsbin *sb)
{
        int nf;
        struct tm interval_tm;
        struct tm file_tm;
        char feedtype_str[32];
        double mean;
        int min;
        int sec;

        nf = fscanf(fp, "%4d%2d%2d%2d%2d%d2",
                &file_tm.tm_year,
                &file_tm.tm_mon,
                &file_tm.tm_mday,
                &file_tm.tm_hour,
                &file_tm.tm_min,
                &file_tm.tm_sec
        );
        if(nf != 6) 
                return EOF;
        /* correct to tm representation */
        file_tm.tm_year -= 1900;
        file_tm.tm_mon--;

        sb->recent.tv_sec = mktime(&file_tm); /* N.B. TZ must be UTC */
        sb->recent.tv_usec = 0;

        interval_tm = file_tm;
        interval_tm.tm_sec = 0;
        interval_tm.tm_min = 0;
        sb->interval =  mktime(&interval_tm); /* N.B. TZ must be UTC */

        nf = fscanf(fp, "%s %s %lf %lf %lf %lf@%2d%2d",
                feedtype_str,
                sb->origin,
                &sb->nprods,
                &sb->nbytes,
                &mean,
                &sb->max_latency,
                &min,
                &sec
        );
        if(nf != 8) 
                return EOF;
        sb->feedtype = atofeedtypet(feedtype_str);
        sb->latency_sum = mean * sb->nprods;
        sb->slowest_at = sb->interval + 60*min + sec;

        nf = fscanf(fp, "%4d%2d%2d%2d%2d%d2",
                &file_tm.tm_year,
                &file_tm.tm_mon,
                &file_tm.tm_mday,
                &file_tm.tm_hour,
                &file_tm.tm_min,
                &file_tm.tm_sec
        );
        if(nf == 6)
        {
                /* correct to tm representation */
                file_tm.tm_year -= 1900;
                file_tm.tm_mon--;
                /*
                 * N.B. TZ must be UTC
                 */
                sb->recent_a.tv_sec = mktime(&file_tm);
                sb->recent_a.tv_usec = 0;
        }

        sb->needswrite = 0;

        return nf;
}


static void
free_statsbin(statsbin *sb)
{
        if(sb == NULL)
                return;
        free(sb);
}


static int
init_statsbin(statsbin *sb, time_t interval, feedtypet feedtype, char *origin)
{
        if(sb == NULL)
                return -1;
        (void) memset(sb, 0, sizeof(statsbin));
        sb->interval = interval;
        sb->feedtype = feedtype;
        if(origin && *origin)
                strncpy(sb->origin, origin, HOSTNAMESIZE -1);
        return 0;
}


static statsbin *
new_statsbin(time_t interval, feedtypet feedtype, char *origin)
{
        statsbin *sb;
        if(interval == 0 || feedtype == NONE)
                return NULL;
        sb = Alloc(1, statsbin);                
        if(sb == NULL)
                return NULL;
        init_statsbin(sb, interval, feedtype, origin);

        return sb;
}


static int
node_compare(const void *p1, const void *p2)
{
        statsbin *h1 = *((statsbin **)p1);
        statsbin *h2 = *((statsbin **)p2);

        if(h2 == NULL || h2->interval == 0)
                return 1;
        if(h1 == NULL || h1->interval == 0)
                return -1;

        if(h1->interval > h2->interval)
                return -1;
        if(h1->interval < h2->interval)
                return 1;
        /* else, same interval */

        if(h1->feedtype < h2->feedtype)
                return -1;
        if(h1->feedtype > h2->feedtype)
                return 1;
        /* else, same feedtype */

        if(h2->origin == NULL || *h2->origin == 0)
                return 1;
        if(h1->origin == NULL || *h1->origin == 0)
                return -1;
        return strcasecmp(h1->origin, h2->origin);      
}


static size_t
growlist(void)
{
        if(maxbins == 0)
        {
                /* first time */
                binList = Alloc(NGROW, statsbin *);
                if(binList == NULL)
                        return 0;
                /* else */
                maxbins = NGROW;
                return maxbins;
        } 
        /* else */
        if(nbins <= MAXBINS)
        {
                binList = (statsbin **)realloc(binList,
                                (maxbins + NGROW) * sizeof(statsbin *));
                if(binList == NULL)
                {
                        /* !??? */
                        maxbins = 0;
                        return 0;
                }
                /* else */
                maxbins += NGROW;
                return  maxbins;
        }
        /* else, recycle */
        while(nbins > MAXBINS)
        {
                nbins--;
                free_statsbin(binList[nbins]);
                binList[nbins] = 0;
        }
        return  maxbins;
}


void
fromfile(FILE *fp)
{
        /* attempt to initialize from existing file */
        statsbin fsb;
        statsbin *sb;

        rewind(fp);
        (void) memset(&fsb, 0, sizeof(fsb));
        while(fscan_statsbin(fp, &fsb) != EOF)
        {
                if(nbins > 0)
                {
                        statsbin *keyp, **sbp;
                        keyp = &fsb;
                        sbp = (statsbin **)bsearch(&keyp, binList, nbins,
                                sizeof(keyp), node_compare);
                        if(sbp != NULL && *sbp != NULL)
                                continue; /* found this entry,=> already read */
                }
                if(nbins >= maxbins)
                {
                        if(growlist() < nbins)
                        {
                                break;
                        }
                }
                /* tack it on the end of the list */
                sb = Alloc(1, statsbin);                
                if(sb == NULL)
                        return; /* out of memory */
                *sb = fsb;
                (void) memset(&fsb, 0, sizeof(fsb));
                binList[nbins++] = sb;  
                /* keep the list sorted */
                qsort(binList, nbins,
                        sizeof(statsbin *), node_compare);
        }
}


static statsbin *
get_statsbin(time_t interval, feedtypet feedtype, char *origin)
{
        statsbin *sb;

        if(nbins > 0)
        {
                statsbin key, *keyp, **sbp;
                if(init_statsbin(&key, interval, feedtype, origin) < 0)
                        return NULL;
                keyp = &key;
                sbp = (statsbin **)bsearch(&keyp, binList, nbins,
                        sizeof(keyp), node_compare);
                if(sbp != NULL && *sbp != NULL)
                        return *sbp; /* found it */
        }
        /* else */
        /* create a new entry */
        if(nbins >= maxbins)
        {
                if(growlist() < nbins)
                        return NULL; /* out of space */
        }
        sb = new_statsbin(interval, feedtype, origin);
        if(sb == NULL)
                return NULL;
        /* tack it on the end of the list */
        binList[nbins++] = sb;  
        /* keep the list sorted */
        qsort(binList, nbins,
                sizeof(statsbin *), node_compare);
        return sb;
}


static time_t
arrival2interval(time_t arrival)
{
        return ((arrival/3600)*3600);
}


void
dump_statsbins(void)
{
        size_t ii;
        if(nbins == 0)
                return;
        for(ii = 0; ii < nbins ; ii++)
                dump_statsbin(binList[ii]);
}


int
binstats(const prod_info *infop,
        const struct timeval *reftimep)
{
        statsbin *sb;
        double latency = d_diff_timestamp(reftimep, &infop->arrival);
        time_t interval = arrival2interval(infop->arrival.tv_sec);

        sb = get_statsbin(interval, infop->feedtype, infop->origin);
        if(sb == NULL)
                return -1;

        sb->nprods = sb->nprods + 1.0;
        sb->nbytes = sb->nbytes + (double)infop->sz;
        sb->recent = infop->arrival;
        sb->recent_a = *reftimep;
        sb->latency_sum += latency;

        if(latency > sb->max_latency)
        {
                sb->max_latency = latency;
                sb->slowest_at = infop->arrival.tv_sec;
        }
        
        sb->needswrite = 1;

        return 0;
}


extern const char *remote;

void
syncbinstats(void)
{
        size_t ii;
        time_t tnow;
        static time_t lastsent=0;
        static int REPORT_INTERVAL=DEFAULT_INTERVAL;
        float  rfact;

        tnow = time(NULL);
        if(tnow - lastsent > REPORT_INTERVAL) 
           {
           lastsent = tnow;

           for(ii = 0; ii < nbins; ii++){
               if(binList[ii]->needswrite) ldmsend_statsbin(binList[ii]);
               }

           /* Add a Random time offset from reporting interval so that
              sites contacting stats server don't converge to a single report time */
           rfact = (float)( random() & 0x7f ) / (float)(0x7f);
           REPORT_INTERVAL = DEFAULT_INTERVAL + (int)(DEFAULT_RANDOM * rfact);

           ldmsend_clnt_destroy();
           }

}
