/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: palt.c,v 1.114.10.2.2.21 2009/01/06 20:44:09 steve Exp $ */

/* 
 */

#include <ldmconfig.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include <limits.h>
#include <string.h>
#include <time.h>
#include <sys/timeb.h>
#include <signal.h>
#include "ldm.h"
#include <regex.h>
#include "globals.h"
#include "palt.h"
#include "pq.h"
#include "action.h"
#include "ldmprint.h"
#include "atofeedt.h"
#include "ldmalloc.h"
#include "RegularExpressions.h"
#include "ulog.h"
#include <stdio.h>

#ifndef TEST_DATE_SUB


/* 
 * A pattern/action file "line" gets compiled into one of these.
 */

#define PATSZ (MAXPATTERN+1)

struct palt {    /* "Pattern Action Line" */
        struct palt *next;
        struct palt *prev;
        feedtypet feedtype;     /* mask */
        char pattern[PATSZ];    /* submitted pattern, retain for comparison?? */
        regex_t prog;                   /* compiled reg exp */
        regmatch_t *pmatchp;
        actiont action;         /* action proc to execute */
        char *private;                  /* storage for args */
};
typedef struct palt palt;


static void
free_palt(palt *pal)
{
        if(pal == NULL) return;
        if(pal->pmatchp != NULL)
        {
                regfree(&pal->prog);
                free(pal->pmatchp);
        }
        if(pal->private != NULL)
                free(pal->private);
        free(pal);
}


static palt *
new_palt(void)
{
        palt *pal;
        pal = Alloc(1, palt);
        if(pal == NULL)
                return NULL;
        (void) memset((char *)pal, 0, sizeof(palt));
        return pal;
}


/*
 * This list is the current compiled pattern / action table
 * (Global for this module.)
 */
static palt *paList = 0; /* the only one */


/*
 * remove an entry from the linked list and Free it
 */
static void
remove_palt(palt *pal)
{
        if(pal->prev != NULL)
                pal->prev->next = pal->next;
        if(pal->next != NULL)
                pal->next->prev = pal->prev;
        if(paList == pal)
        {
                if(pal->next != NULL)
                        paList = pal->next;
                else
                        paList = NULL;
        }
        free_palt(pal);
}


/* Begin readPatFile */
/*
 * static global for syntax error reporting
 */
static int linenumber = 1;


/* skip the rest of a line */
static int
skipline(FILE *fp)
{
        int ch;
        while((ch = getc(fp)) != '\n')
                if(ch == EOF) return EOF;
        return ch;
}


/*
 * Gets a complete pattern-action-args entry from the input "fp",
 * of maximum length "bufsize", and puts the result in the array "buf".
 * Skips comments delimited by '#' and newlines.
 * May read several lines, since  any line beginning with a blank or tab
 * is interpreted as a continuation of the previous line,
 * with the previous newline and leading blanks or
 * tabs replaced by a single tab.
 * Returns -1 on EOF, -2 on syntax error, number of characters otherwise.
 */
static int
pal_line(char buf[],
        int  bufsize,
        FILE *fp)
{
        char *cp;
        int len;
        int ch;
        static int incr = 0;

        if(incr != 0)
        {
                linenumber += incr;
                incr = 0;
        }

again1:
        cp = fgets(buf, bufsize, fp);
        if(cp == NULL) return -1;
        if(*cp == '#' || *cp == '\n')
        {
                linenumber++;
                goto again1;    /* skip comments or blank lines */
        }
        len = (int) strlen(buf);
        incr++;

        cp = &buf[len-1];
        if(*cp != '\n')
        {
                uerror("Entry too long at line %d", linenumber);
                return -2;
        }
        /* get rid of trailing white space */
        for(; cp >= buf; cp--, len--)
        {
                if(!isspace(*cp))
                {
                        break;
                }
                /* else */
                *cp = 0;
        }

again2:
        ch = getc(fp);
        if(ch == EOF) return len;
        switch(ch){
                case '#' :
                        /* embedded comment */
                        if(skipline(fp) < 0) return len;
                        incr++;
                        goto again2;
                case ' ' :
                case '\t' :
                        /* continuation line */
                        while((ch = getc(fp)) == ' ' || ch == '\t')
                                ;
                        if(ch == EOF) return len;
                        if(ch == '\n')
                        {
                                /* embedded blank line */
                                incr++;
                                goto again2;
                        }
                        ungetc(ch, fp);
                        if(len >= bufsize)
                        {
                                uerror("Entry too long to continue at line %d", linenumber);
                                return -2;
                        }
                        /* else */
                        *++cp = '\t', len++; /* comma operator */
                        buf = cp + 1;
                        bufsize -= len;
                        goto again1;
        }

        ungetc(ch, fp);
        return len;
}


static int
tokenize(char *buf,
        char **tokensp,
        int maxtokens)
{
        int cnt;
        char *cp;
        static char *scansets[] = {"\t \"", "\""};
        char *sp;

        cnt = 1;
        cp = *tokensp = buf;
        sp = scansets[0];
        while( cnt < maxtokens)
        {
                cp = strpbrk(cp, sp);
                if( cp == NULL )
                {
                        if(**tokensp == 0)
                        {
                                cnt--;
                                *tokensp = NULL;
                        }
                        break; /* normal loop exit */
                }
                if(*cp == '"')
                {
                        /* toggle the scan to exclude or include white space */
                        sp = (sp == scansets[0]) ? scansets[1] : scansets[0];   
                }
                *cp++ = 0;
                /* handle runs */
                if(cp < buf + 2 || *(cp-2) == 0)
                {
                        *tokensp = cp;
                }
                else
                {
                        *++tokensp = cp;
                        cnt++;
                }
        }
        return cnt;     
}


static int
tabTokenize(char *buf,
        char **tokensp,
        int maxtokens)
{
        int cnt;
        char *cp;

        cnt = 1;
        *tokensp = buf;
        while( cnt < maxtokens)
        {
                cp = strchr(*tokensp, '\t');
                if( cp == NULL )
                        break; /* normal loop exit */
                if(cp == *tokensp) /* handle multple tabs */
                {
                        (*tokensp)++;
                        continue;
                }
                *cp++ = 0;
                *++tokensp = cp;
                cnt++;
        }
        return cnt;     
}


static palt *
new_palt_fromStr(char *buf)
{
        palt *pal;

        char *tabtoks[16];
        int ntabtoks;
        int status; 
        
        pal = new_palt();
        if(pal == NULL)
                return NULL;

        ntabtoks = tabTokenize(buf, tabtoks, 4);

        if(ntabtoks < 3 )
        {
                uerror("Syntax error at line %d, not enough fields", linenumber);
                goto err;
        }

        status = strfeedtypet(tabtoks[0], &pal->feedtype);
        if(status != FEEDTYPE_OK)
        {
                uerror("feedtype error at line %d: %s: \"%s\"",
                        linenumber, strfeederr(status), tabtoks[0]);
                goto err;
        }

        assert(tabtoks[1] != NULL && *tabtoks[1] != 0);
        if(strlen(tabtoks[1]) >= (size_t)PATSZ)
        {
                uerror("Pattern string too long at line %d: \"%s\"",
                        linenumber, tabtoks[1]);
                goto err;
        }
        if (re_isPathological(tabtoks[1]))
        {
                uwarn("Adjusting pathological regular-expression at line "
                    "%d: \"%s\"", linenumber, tabtoks[1]);
                re_vetSpec(tabtoks[1]);
        }
        strncpy(pal->pattern, tabtoks[1], PATSZ - 1);
        pal->pattern[PATSZ-1] = 0;


        if( regcomp(&pal->prog, pal->pattern, REG_EXTENDED) != 0)
        {
                uerror("regcomp error at line %d: \"%s\"", linenumber, tabtoks[1]);
                goto err;
        }
        pal->pmatchp = Alloc(pal->prog.re_nsub + 1, regmatch_t);
        if(pal->pmatchp == NULL)
        {
                regfree(&pal->prog);
                serror("new_palt: malloc failed");
                goto err;
        }

        if(atoaction(tabtoks[2], &pal->action) < 0)
        {
                /* duplicate reporting */
                uerror("Unknown action \"%s\" at line %d", tabtoks[2], linenumber);
                goto err;
        }

        if(ntabtoks >= 4)
        {
                size_t len = strlen(tabtoks[3]);
                assert(len != 0);

                pal->private = malloc(len+1);
                if(pal->private == NULL)
                {
                        serror("new_palt: malloc failed");
                        goto err;
                }
                (void) strcpy(pal->private, tabtoks[3]);
        }

        return pal;
err:
        free_palt(pal);
        return NULL;
}


/*
 * Read & parse pattern / action file into a newly allocated palt*. 
 * If all goes well, free the old global palt *paList and set it to the
 * new list.
 *
 * Arguments:
 *      path    The pathname of the configuration-file
 *
 * Returns:
 *      >=0     Number of pattern/action entries (might be zero).
 *      -1      Unable to open configuration-file.
 *      -2      Error in configuration-file.
 */
/* init_pattern_file */
int
readPatFile(char *path)
{
    int         status;
    FILE        *fp = fopen(path, "r");

    if (fp == NULL) {
        serror("Couldn't open configuration-file \"%s\"", path);
        status = -1;
    }
    else {
        palt*   pal = NULL;
        palt*   begin = NULL;
        palt*   othr = NULL;

        linenumber = 1;
        status = 0;

        for (;;) {
            char        buf[512];
            int         len = pal_line(buf, sizeof(buf), fp);

            if (len <= -2) {
                status = -2;            /* error */
                break;
            }

            if (len <= 0)
                break;                  /* EOF */

            if ((pal = new_palt_fromStr(buf)) == NULL) {
                status = -2;
                break;
            }

            if (begin == NULL) {
                begin = pal;
            }
            else {
                othr->next = pal;
                pal->prev = othr;
            }

            othr = pal;
            status++;
        }

        if (status < 0) {
            uerror("Error in configuration-file \"%s\"", path);

            /*
             * Free new list.
             */
            for (pal = begin; pal != NULL; ) {
                othr = pal->next;
                free_palt(pal);
                pal = othr;
            }
        }
        else {
            /*
             * Free old list and replace with new list.
             */
            for (pal = paList; pal != NULL; ) {
                othr = pal->next;
                free_palt(pal);
                pal = othr;
            }

            pal = paList = begin;

            uinfo("Successfully read configuration-file \"%s\"", path);
        }

        (void)fclose(fp);
    }                                   /* configuration-file opened */

    return status;
}

/* End readPatFile */


/*
 * wrapper around c library strftime() call
 * that takes a time_t rather than a struct tm,
 * uses gmtime().
 */
static size_t
gm_strftime(char *str, size_t maxsize, char *format, time_t arrival)
{
        struct tm *atmp, atm;
        atmp = gmtime(&arrival);
        if(atmp == NULL)
        {
                udebug("gmtime returns NULL");
                /* you should never really execute this */
                strncpy(str, format, maxsize-1);
                return strlen(str);
        }
        /* else */
        atm = *atmp;
        return strftime(str, maxsize, format, &atm);
}

#endif


/*
 * Converts a broken-down time, expressed in UTC, into a time since the Epoch
 * value.  The field values in the broken-down time may lie outside their
 * nominal ranges.
 *
 * Arguments:
 *      tm      Pointer to broken-down time structure expressed in UTC.
 * Returns:
 *      -1      Failure.  An error-message is logged.
 *      else    The corresponding time in seconds since the Epoch.
 */
static time_t
utcToEpochTime(
    const struct tm* const      tm)
{
    time_t                      epochTime = -1; /* error */
    struct tm                   localTime = *tm;

#ifdef HAVE_TIMEGM

    epochTime = timegm(&localTime);

    if (epochTime == (time_t)-1)
        serror("utcToEpochTime(): timegm() failure");

#else

    /*
     * Get timezone information.
     */
    tzset();

    /*
     * Convert the broken-down UTC time into local time.
     */
    localTime.tm_min -= (int)timezone/60;
    localTime.tm_isdst = 0;

    /*
     * Obtain the Epoch time corresponding to the broken-down local time.
     */
    epochTime = mktime(&localTime);

    if (epochTime == (time_t)-1)
        serror("utcToEpochTime(): mktime() failure");

#endif

    return epochTime;
}

static void
seq_sub(
    const char* istring,        /* input string, possibly including date
                                   indicators to be expanded */
    char*       ostring,        /* output string, with date indicators
                                   expanded */
    u_int      seqnum)      /* UTC-based product-time (might be "now") */
{
    static int          seqfirst = 1;      /* true only first time called */
    static regex_t      seqprog;           /* compiled regexp for date indicator */
    static regmatch_t   seqpmatch[1];      /* substring matching information */
    const char*         e2;             /* pointer to last character of seq
                                         * indicator substring */
    const char*         is;             /* pointer to next input character */
    /*
     * Compile regular-expression on first call.
     */
    if (seqfirst) {
        static char     seq_exp[] = "\\(seq\\)";

        if (regcomp(&seqprog, seq_exp, REG_EXTENDED) != 0)
            serror("Bad regular expression or out of memory: %s", seq_exp);
        seqfirst = 0;
    }

    for (is = istring; regexec(&seqprog, is, 1, seqpmatch, 0) == 0; is = e2) {
        /*
         * Process the next date indicator in "istring".
         */
        printf("%d, %d\n", seqpmatch[0].rm_so, seqpmatch[0].rm_eo);
        const char *const       s0 = &is[seqpmatch[0].rm_so];
                                        /* start of entire substring match */

        e2 = &is[seqpmatch[0].rm_eo];      /* points to last char of substring */

        /*
         * Copy stuff before match.
         */
        {
            while (is < s0)
              *ostring++ = *is++;
        }

        int printed =  sprintf(ostring, "%d", seqnum);
        ostring+=printed;
    }
    (void)strcpy(ostring, is);          /* copy rest of input to output */
}

/*
        from  ldm3/dd_regexp.c,v 1.24 1991/03/02 17:32:08
  Substitutes date components in a string containing date indicators.
  This is useful for deriving complete date information from WMO
  headers on real-time data, since the WMO headers only contain a day
  of the month.  If any of the following sequences occur in the
  filename field, they are replaced by the date components they
  represent, where `DD' is a recent day of the month for which other
  date components are desired.

  (DD:yyyy) four digit year, e.g. `1988'
  (DD:yy)   last two digits of the year, e.g. `88'
  (DD:mm)   numeric month, from 01 to 12, e.g. `06'
  (DD:mmm)  three-character month abbreviation, e.g. `jun'
  (DD:dd)   day of month, e.g. `30' (same as DD)
  (DD:ddd)  Julian day, e.g. `182'
  (DD:hh)   Current hour (local time)

  For example, if istring is "sa_us.(01:yy)(01:mmm)(01:dd)" on 3 June
  1988, ostring will be "sa_us.88jun01" on return.  If the same string
  were input on 30 June 1988, the returned ostring would be
  "sa_us.88jul01", since this is nearer the current date.

  */

static void
date_sub(
    const char* istring,        /* input string, possibly including date
                                   indicators to be expanded */
    char*       ostring,        /* output string, with date indicators
                                   expanded */
    time_t      prodClock)      /* UTC-based product-time (might be "now") */
{
    static int          first = 1;      /* true only first time called */
    static regex_t      prog;           /* compiled regexp for date indicator */
    static regmatch_t   pmatch[3];      /* substring matching information */
    const char*         e2;             /* pointer to last character of time
                                         * indicator substring */
    struct tm           utcProdTime;    /* initial product-time structure */
    int                 tdom;           /* day-of-month from product-time */
    const char*         is;             /* pointer to next input character */

    /*
     * Compile regular-expression on first call.
     */
    if (first) {
        static char     date_exp[] = "\\(([0-9]{2}):([^)]*)\\)";

        if (regcomp(&prog, date_exp, REG_EXTENDED) != 0)
            serror("Bad regular expression or out of memory: %s", date_exp);

        first = 0;
    }

    /*
     * Convert time argument to broken-down times.
     */
    {
        struct tm *tmp = gmtime(&prodClock);

        if(tmp == NULL)
            uerror("date_sub(): gmtime() returns NULL");

        utcProdTime = *tmp;
    }
    tdom = utcProdTime.tm_mday;         /* UTC-based day-of-month */

    for (is = istring; regexec(&prog, is, 3, pmatch, 0) == 0; is = e2 + 1) {
        /*
         * Process the next date indicator in "istring".
         */
        int                     dom;    /* day-of-month in date indicator */
        char                    select[6];
                                        /* time component code: yyyy, mmm,... */
        int                     year;
        int                     month;
        const char *const       s0 = &is[pmatch[0].rm_so];
                                        /* start of entire substring match */

        const char*             sp = &is[pmatch[2].rm_so];
                                        /* start of time component code */

        e2 = &is[pmatch[2].rm_eo];      /* points to last char of substring */

        /*
         * Copy stuff before match.
         */
        {
            while (is < s0)
              *ostring++ = *is++;

            is++;                       /* skip over `(' */
        }

        /*
         * Get day-of-month from substring.
         */
        {
            char        d1 = *is++;
            char        d2 = *is++;

            dom = (d1 - '0') * 10 + d2 - '0';
        }

        /*
         * Validate day-of-month from substring.
         */
        if (dom < 0 || dom > 31) {
            uerror("bad day of month in ident: %s",istring);
            dom = -1;
        }

        /*
         * Copy time component code to "select" using lower-case letters.
         */
        {
            char*       s = &select[0];

            while (sp < e2)
              *s++ = (char)tolower(*sp++);

            *s = '\0';
        }

        if (dom < 0) {                  /* bad date indicator */
            (void) sprintf(ostring,"%s",select);
            ostring += strlen(select);
        }
        else {
            /* Adjusted, UTC-based, product-time structure: */
            struct tm           adjProdTime = utcProdTime;
            static char*        months[] = {
                "jan","feb","mar","apr","may","jun",
                "jul","aug","sep","oct","nov","dec"};

            if (dom == 0) {
                /*
                 * Special date indicator.  Use day-of-month from product-time.
                 */
                dom = tdom;
            }
            else {
                /*
                 * The matched substring in the product-identifier is a valid
                 * day-of-month.  Adjust the product-time so that it falls on
                 * the specified day.
                 */
                struct tm       tmTime = utcProdTime;
                time_t          prodMonthClock;

                tmTime.tm_mday = dom;           /* set day to specified */
                prodMonthClock = utcToEpochTime(&tmTime);

                if (prodMonthClock != -1) {
                    time_t      prevMonthClock;

                    tmTime.tm_mon--;            /* set month to previous */
                    prevMonthClock = utcToEpochTime(&tmTime);

                    if (prevMonthClock != -1) {
                        time_t      nextMonthClock;

                        tmTime.tm_mon += 2;     /* set month to next */
                        nextMonthClock = utcToEpochTime(&tmTime);

                        if (nextMonthClock != -1) {
                            /*
                             * Of the three time candidates, use the one
                             * closest to the product-time that's not too far
                             * in the future.
                             */
#                           define SECONDS_PER_DAY (60*60*24)
                            time_t              maxTime =
                                prodClock + (3*SECONDS_PER_DAY)/2;
                            time_t              adjClock = 
                                nextMonthClock < maxTime
                                    ? nextMonthClock
                                    : prodMonthClock < maxTime
                                        ? prodMonthClock
                                        : prevMonthClock;
                            struct tm*  adjTmTime = gmtime(&adjClock);

                            if (adjTmTime == NULL) {
                                uerror("date_sub(): gmtime() failure");
                            }
                            else {
                                adjProdTime = *adjTmTime;
                            }
                        }               /* valid "nextMonthClock" */
                    }                   /* valid "prevMonthClock" */
                }                       /* valid "prodMonthClock" */
            }                           /* "adjProdTime" needs adjusting */

            if (strcmp(select,"yyyy") == 0) {
                year = adjProdTime.tm_year + 1900;
                (void) sprintf(ostring,"%d",year);
                ostring += 4;
            }
            else if (strcmp(select,"yy") == 0) {
                year = adjProdTime.tm_year;
                (void) sprintf(ostring,"%02d", year % 100);
                ostring += 2;
            }
            else if (strcmp(select,"mm") == 0) {
                month = adjProdTime.tm_mon + 1;
                (void) sprintf(ostring,"%02d",month);
                ostring += 2;
            }
            else if (strcmp(select,"mmm") == 0) {
                month = adjProdTime.tm_mon;
                (void) sprintf(ostring,"%s",months[month]);
                ostring += 3;
            }
            else if (strcmp(select,"dd") == 0) {
                (void) sprintf(ostring,"%02d",(int)adjProdTime.tm_mday);
                ostring += 2;
            }
            else if (strcmp(select,"ddd") == 0) {
                int     doy = adjProdTime.tm_yday + 1;
                (void) sprintf(ostring,"%03d",doy);
                ostring += 3;
            }
            else if (strcmp(select,"hh") == 0) {
                (void) sprintf(ostring,"%02d",adjProdTime.tm_hour);
                ostring += 2;
            }
            else {
                uerror("unknown date indicator: %s",select);
            }
        }                               /* good date indicator */
    }                                   /* date substitution loop */

    (void)strcpy(ostring, is);          /* copy rest of input to output */
}


#ifdef TEST_DATE_SUB
int
main()
{
    char        buf[128];
    struct tm   tm;
    time_t      feb28;
    time_t      feb28leap;
    time_t      feb29leap;
    time_t      mar01;
    time_t      mar01leap;
    time_t      dec31;
    time_t      jan01;
    time_t      may31;

    (void)openulog("date_sub", (LOG_CONS|LOG_PID), LOG_LDM, "-");

    /*
     * The start of the epoch is not tested because it can cause
     * utcToEpochTime() to fail.
     */

    {
        time_t          unixTime = time(NULL);
        struct tm       utc = *gmtime(&unixTime);

        assert(utcToEpochTime(&utc) == unixTime);

        unixTime = 86400;
        utc = *gmtime(&unixTime);

        assert(utcToEpochTime(&utc) == unixTime);
    }

    tm.tm_year = 71;
    tm.tm_mon = 1;
    tm.tm_mday = 28;
    tm.tm_hour = 12;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    tm.tm_isdst = 0;
    feb28 = utcToEpochTime(&tm);

    date_sub("(27:yyyy)-(27:mm)-(27:dd)", buf, feb28);
    assert(strcmp(buf, "1971-02-27") == 0);

    date_sub("(28:yyyy)-(28:mm)-(28:dd)", buf, feb28);
    assert(strcmp(buf, "1971-02-28") == 0);

    date_sub("(29:yyyy)-(29:mm)-(29:dd)", buf, feb28);
    assert(strcmp(buf, "1971-03-01") == 0);

    date_sub("(01:yyyy)-(01:mm)-(01:dd)", buf, feb28);
    assert(strcmp(buf, "1971-03-01") == 0);

    date_sub("(02:yyyy)-(02:mm)-(02:dd)", buf, feb28);
    assert(strcmp(buf, "1971-02-02") == 0);

    tm.tm_year = 80;
    tm.tm_mon = 1;
    tm.tm_mday = 29;
    tm.tm_hour = 12;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    tm.tm_isdst = 0;
    feb29leap = utcToEpochTime(&tm);

    date_sub("(28:yyyy)-(28:mm)-(28:dd)", buf, feb29leap);
    assert(strcmp(buf, "1980-02-28") == 0);

    date_sub("(29:yyyy)-(29:mm)-(29:dd)", buf, feb29leap);
    assert(strcmp(buf, "1980-02-29") == 0);

    date_sub("(30:yyyy)-(30:mm)-(30:dd)", buf, feb29leap);
    assert(strcmp(buf, "1980-03-01") == 0);

    date_sub("(01:yyyy)-(01:mm)-(01:dd)", buf, feb29leap);
    assert(strcmp(buf, "1980-03-01") == 0);

    date_sub("(02:yyyy)-(02:mm)-(02:dd)", buf, feb29leap);
    assert(strcmp(buf, "1980-02-02") == 0);

    tm.tm_year = 80;
    tm.tm_mon = 1;
    tm.tm_mday = 28;
    tm.tm_hour = 12;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    tm.tm_isdst = 0;
    feb28leap = utcToEpochTime(&tm);

    date_sub("(27:yyyy)-(27:mm)-(27:dd)", buf, feb28leap);
    assert(strcmp(buf, "1980-02-27") == 0);

    date_sub("(28:yyyy)-(28:mm)-(28:dd)", buf, feb28leap);
    assert(strcmp(buf, "1980-02-28") == 0);

    date_sub("(29:yyyy)-(29:mm)-(29:dd)", buf, feb28leap);
    assert(strcmp(buf, "1980-02-29") == 0);

    date_sub("(30:yyyy)-(30:mm)-(30:dd)", buf, feb28leap);
    assert(strcmp(buf, "1980-01-30") == 0);

    date_sub("(01:yyyy)-(01:mm)-(01:dd)", buf, feb28leap);
    assert(strcmp(buf, "1980-02-01") == 0);

    tm.tm_year = 70;
    tm.tm_mon = 2;
    tm.tm_mday = 1;
    tm.tm_hour = 12;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    tm.tm_isdst = 0;
    mar01 = utcToEpochTime(&tm);

    date_sub("(28:yyyy)-(28:mm)-(28:dd)", buf, mar01);
    assert(strcmp(buf, "1970-02-28") == 0);

    date_sub("(29:yyyy)-(29:mm)-(29:dd)", buf, mar01);
    assert(strcmp(buf, "1970-03-01") == 0);

    date_sub("(01:yyyy)-(01:mm)-(01:dd)", buf, mar01);
    assert(strcmp(buf, "1970-03-01") == 0);

    date_sub("(02:yyyy)-(02:mm)-(02:dd)", buf, mar01);
    assert(strcmp(buf, "1970-03-02") == 0);

    date_sub("(03:yyyy)-(03:mm)-(03:dd)", buf, mar01);
    assert(strcmp(buf, "1970-02-03") == 0);

    tm.tm_year = 80;
    tm.tm_mon = 2;
    tm.tm_mday = 1;
    tm.tm_hour = 12;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    tm.tm_isdst = 0;
    mar01leap = utcToEpochTime(&tm);

    date_sub("(28:yyyy)-(28:mm)-(28:dd)", buf, mar01leap);
    assert(strcmp(buf, "1980-02-28") == 0);

    date_sub("(29:yyyy)-(29:mm)-(29:dd)", buf, mar01leap);
    assert(strcmp(buf, "1980-02-29") == 0);

    date_sub("(30:yyyy)-(30:mm)-(30:dd)", buf, mar01leap);
    assert(strcmp(buf, "1980-03-01") == 0);

    date_sub("(01:yyyy)-(01:mm)-(01:dd)", buf, mar01leap);
    assert(strcmp(buf, "1980-03-01") == 0);

    date_sub("(02:yyyy)-(02:mm)-(02:dd)", buf, mar01leap);
    assert(strcmp(buf, "1980-03-02") == 0);

    date_sub("(03:yyyy)-(03:mm)-(03:dd)", buf, mar01leap);
    assert(strcmp(buf, "1980-02-03") == 0);

    tm.tm_year = 70;
    tm.tm_mon = 11;
    tm.tm_mday = 31;
    tm.tm_hour = 12;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    tm.tm_isdst = 0;
    dec31 = utcToEpochTime(&tm);

    date_sub("(30:yyyy)-(30:mm)-(30:dd)", buf, dec31);
    assert(strcmp(buf, "1970-12-30") == 0);

    date_sub("(31:yyyy)-(31:mm)-(31:dd)", buf, dec31);
    assert(strcmp(buf, "1970-12-31") == 0);

    date_sub("(01:yyyy)-(01:mm)-(01:dd)", buf, dec31);
    assert(strcmp(buf, "1971-01-01") == 0);

    date_sub("(02:yyyy)-(02:mm)-(02:dd)", buf, dec31);
    assert(strcmp(buf, "1970-12-02") == 0);

    tm.tm_year = 71;
    tm.tm_mon = 0;
    tm.tm_mday = 1;
    tm.tm_hour = 12;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    tm.tm_isdst = 0;
    jan01 = utcToEpochTime(&tm);

    date_sub("(31:yyyy)-(31:mm)-(31:dd)", buf, jan01);
    assert(strcmp(buf, "1970-12-31") == 0);

    date_sub("(01:yyyy)-(01:mm)-(01:dd)", buf, jan01);
    assert(strcmp(buf, "1971-01-01") == 0);

    date_sub("(02:yyyy)-(02:mm)-(02:dd)", buf, jan01);
    assert(strcmp(buf, "1971-01-02") == 0);

    date_sub("(03:yyyy)-(03:mm)-(03:dd)", buf, jan01);
    assert(strcmp(buf, "1970-12-03") == 0);

    tm.tm_year = 107;
    tm.tm_mon = 4;
    tm.tm_mday = 31;
    tm.tm_hour = 14;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    tm.tm_isdst = 1;
    may31 = mktime(&tm);

    date_sub("(31:yyyy)-(31:mm)-(31:dd)", buf, may31);
    assert(strcmp(buf, "2007-05-31") == 0);

    date_sub("(01:yyyy)-(01:mm)-(01:dd)", buf, may31);
    assert(strcmp(buf, "2007-06-01") == 0);

    date_sub("(02:yyyy)-(02:mm)-(02:dd)", buf, may31);
    assert(strcmp(buf, "2007-05-02") == 0);
    seq_sub("/tmp/(seq).txt", buf, 1234);

    exit(0);
}
#else


/*
 - regsub - perform substitutions after a regcomp match
 */
static void
regsub(palt *pal, const char *ident, char *dest)
{
        register const char *src;
        register char *dst;
        register char c;
        register int no;
        register int len;

        src = pal->private;
        dst = dest;
        while ((c = *src++) != '\0') {
                if (c == '&')
                        no = 0;
                else if (c == '\\') {
                        if ('0' <= *src && *src <= '9') {
                                no = *src++ - '0';
                        }
                        else if ('(' == *src &&
                                '0' <= src[1] && '9' >= src[1]) {

                                int     i;

                                if (sscanf(src+1, "%d)", &i) == EOF || i < 0) {
                                    uerror("damaged match string: %s",
                                            strerror(errno));
                                    break;
                                }
                                no = i;
                                src = strchr(src, ')') + 1;
                        }
                        else {
                            no = -1;
                        }
                }
                else
                        no = -1;

                if (no < 0) {   /* Ordinary character. */
                        if (c == '\\' && (*src == '\\' || *src == '&'))
                                c = *src++;
                        *dst++ = c;
                } else if (no <= pal->prog.re_nsub &&
                        pal->pmatchp[no].rm_so >= 0 &&
                        pal->pmatchp[no].rm_eo > pal->pmatchp[no].rm_so) {

                        len = pal->pmatchp[no].rm_eo - pal->pmatchp[no].rm_so;
                        (void) strncpy(dst, &ident[pal->pmatchp[no].rm_so],
                                len);
                        dst += len;
                        if (len != 0 && *(dst-1) == '\0') {
                                /* strncpy hit NUL. */
                                uerror("damaged match string");
                                return;
                        }
                }
        }
        *dst++ = '\0';
}


/*
 * Apply the action in pal to prod
 */
static int
prodAction(product *prod, palt *pal, const void *xprod, size_t xlen)
{
    int         argc;
    int         status;

    if (pal->private == NULL || *pal->private == 0)
    {
        char*   argv[1] = {NULL};

        argc = 0;
        status = (*pal->action.prod_action)(prod, argc, argv, xprod, xlen);
    }
    else
    {
        /*
         * The following are static in case _POSIX_ARG_MAX is large enough
         * to blow the stack.
         */
        static char    buf1[_POSIX_ARG_MAX];
        static char    buf2[_POSIX_ARG_MAX];
        static char*   argv[1 + _POSIX_ARG_MAX/2];
        char *result = 0;

        regsub(pal, prod->info.ident, buf1);
        buf1[sizeof(buf1)-1] = 0;

        gm_strftime(buf2, sizeof(buf2), buf1, prod->info.arrival.tv_sec);
        buf2[sizeof(buf2)-1] = 0;

        /*
         * Alternate between buf1 and buf2 as input/output. If more functions
         * added here make sure result is assigned the proper buffer.
         */
        date_sub(buf2, buf1, prod->info.arrival.tv_sec);
        buf1[sizeof(buf1)-1] = 0;

        seq_sub(buf1, buf2, prod->info.seqno);
        buf2[sizeof(buf2)-1] = 0;

        result = &buf2[0];

        if (ulogIsVerbose())
            uinfo("               %s: %s and the ident is %s", s_actiont(&pal->action), result, prod->info.ident);

        argc = tokenize(result, argv, ARRAYLEN(argv));

        if (argc < ARRAYLEN(argv))
        {
            argv[argc] = NULL;
            status = (*pal->action.prod_action)(prod, argc, argv, xprod, xlen);
        }
        else
        {
            uerror("Too many PIPE arguments: \"%s\"", result);
            status = -1;
        }
    }

    return status;
}


/*
 * Loop thru the pattern / action table, applying actions
 */
/*ARGSUSED*/
int
processProduct(const prod_info *infop, const void *datap,
        void *xprod, size_t xlen,
        void *otherargs)
{
        palt*           pal;
        palt*           next;
        int             status = 0;
        int             did_something = 0;
        product         prod;

        if(ulogIsVerbose())
                uinfo("%s", s_prod_info(NULL, 0, infop, ulogIsDebug()));

        for(pal = paList; pal != NULL; pal = next)
        {
                next = pal->next;
                /*
                 * If the feedtype matches AND ((the product ID matches the
                 * regular expression) OR (the pattern is "_ELSE_" AND nothing
                 * has been done to this product yet AND the first char of
                 * the ident isn't '_'))
                 */
                if((infop->feedtype & pal->feedtype)
                   && ((regexec(&pal->prog, infop->ident,
                                pal->prog.re_nsub +1,  pal->pmatchp,
                                        0) == 0)
                       || (strcmp(pal->pattern, "^_ELSE_$") == 0
                           && did_something == 0
                           && infop->ident[0] != '_')))
                {
                        /* A hit, do something */
                        prod.info = *infop;
                        prod.data = (void *)datap; /* cast away const */
                        status = prodAction(&prod, pal, xprod, xlen);
                        did_something++;
                        if(status < 0
                                && (pal->action.flags & LDM_ACT_TRANSIENT))
                        {
                                /* connection closed, don't try again */
                                remove_palt(pal);
                        }
                }
        }
        
        return 0; /* always succeeds */
}


/*
 * Create and process an (auto) empty product
 * whose ident is ident.
 * Used for _BEGIN_ and _END_ processing
 */
void
dummyprod(char *ident)
{
        product prod;

        (void) memset((char *)&prod, 0 , sizeof(prod));
        prod.data = NULL;
        (void) set_timestamp(&prod.info.arrival);
        prod.info.feedtype = ANY;
        prod.info.ident = ident;
        prod.info.origin = "localhost";

        processProduct(&prod.info, &prod.data, 0, 0, 0);
}
#endif
