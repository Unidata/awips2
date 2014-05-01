/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See top level COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pqutil.c,v 1.18.10.2.2.6 2008/04/15 16:34:10 steve Exp $ */

#include <ldmconfig.h>

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netdb.h>             /* won't need this when inetutil.h gets fixed */
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#include "ldm.h"        /* needed by following */
#include "atofeedt.h"
#include "inetutil.h"
#include "ldmprint.h"
#include "md5.h"
#include "pq.h"
#include "ulog.h"
#include "RegularExpressions.h"

#define MAX_COMMAND     256

/* command set */

typedef enum {
    CMD_SET,
    CMD_SHOW,
    CMD_READ,
    CMD_NEW,
    CMD_PUT,
    CMD_WRITE,
    CMD_DISCARD,
    CMD_DISPLAY,
    CMD_WATCH,
    CMD_DELETE,
    CMD_STATS,
    CMD_HELP,
    CMD_QUIT,
    CMD_COMMENT,
    CMD_BAD
} cmd_set;

/* set command options */

typedef enum {
    OPT_FEEDTYPE,
    OPT_PATTERN,
    OPT_CURSOR,
    OPT_CURSDIR,
    OPT_TOTIME,
    OPT_FROMTIME,
    OPT_ARRIVAL,
    OPT_PRODFTYPE,
    OPT_SEQNO,
    OPT_ORIGIN,
    OPT_IDENT,
    OPT_NONE,
    OPT_BAD
} opt_set;

/* command record */

typedef struct {
    cmd_set     cmd;
    opt_set     cmd_opt;
    char        cmd_val[MAX_COMMAND];
} cmd_rec;

/******************************************************************************
 Global declarations
******************************************************************************/

int     seqnum = 0;                               /* product sequence number */
int     tty_flag = 0;                                 /* true if interactive */
char    orighost[HOSTNAMESIZE + 1];                        /* host of origin */
char    prod_ident[KEYSIZE+1];                                 /* product ID */
cmd_rec crec;                                           /* command structure */
static struct termios   save_termios; /* holder for default stdin parameters */
static  MD5_CTX *md5ctxp = NULL;                     /* md5 checksum context */
feedtypet       fdtype = ANY;                       /* command line feedtype */

/******************************************************************************
  print program usage message and exit
******************************************************************************/

static void
usage(const char *av0) {

    fprintf(stderr,
            "Usage: %s [options] pqueuefname\t\nOptions:\n", av0);
    fprintf(stderr,
            "\t-l logfile     Send log info to file (default STDERR)\n");
    fprintf(stderr,
            "\t-v             Verbose output\n");
    fprintf(stderr,
            "\t-x             Debug output\n");
    fprintf(stderr,
            "\t-c             Create the pqueue, clobber existing\n");
    fprintf(stderr,
            "\t-n             Create the pqueue, error if it exists\n");
    fprintf(stderr,
            "\t-a align       Align (round up) allocations to \"align\" boundaries\n");
    fprintf(stderr,
            "\t-p             Align (round up) allocations to the pagesize\n");
    fprintf(stderr,
            "\t-s initialsz   Initial data section size\n");
    fprintf(stderr,
            "\t-S nproducts   Initially allocate index space for \"nproducts\" products\n");
    fprintf(stderr,
            "\t-r             Open read only\n");
    fprintf(stderr,
            "\t-P             Open private (PQ_PRIVATE)\n");
    fprintf(stderr,
            "\t-L             Open no locking (PQ_NOLOCK)\n");
    fprintf(stderr,
            "\t-F             Open \"fixed size\" (PQ_NOGROW)\n");
    fprintf(stderr,
            "\t-M             Open PQ_NOMAP\n");
    fprintf(stderr,
            "\t-w             Run the watch command and exit when through\n");
    fprintf(stderr,
            "\t-f feedtype    Product feedtype (default ANY)\n");

    exit(EXIT_FAILURE);

} /*usage*/

/******************************************************************************
  close the queue and exit
******************************************************************************/

void
exit_prog(pqueue *pq) {

    pq_close(pq);
    exit(EXIT_SUCCESS);

}

/******************************************************************************
 parse time value from "set cursor" command. Returns 0 on success, non-0
 otherwise.
******************************************************************************/

int
parse_time(char *in_string, timestampt *result) {

    if (!strcmp(in_string,"0")) {           /* set to beginning of the epoch */
        *result = TS_ZERO;
        return(0);
    }

    else if (!strcmp(in_string, "EOT")) {         /* set to end of the epoch */
        *result = TS_ENDT;
        return(0);
    }

    else if (!strcmp(in_string,"NOW")) {              /* set to current time */
        gettimeofday(result,NULL);
        return(0);
    }

    else {                                     /* parse the date-time string */
        int     ret_val;
        struct tm       time_conv;

        ret_val = sscanf(in_string,"%4d%2d%2d%2d%2d", &time_conv.tm_year,
                         &time_conv.tm_mon, &time_conv.tm_mday,
                         &time_conv.tm_hour, &time_conv.tm_min);
        if (ret_val != 5) {                                     /* bad input */
            uerror("Bad time string: %s", in_string);
            return(1);
        }

        time_conv.tm_sec = 0;
        time_conv.tm_year -= 1900;
        time_conv.tm_mon--;
        time_conv.tm_isdst = -1;

        result->tv_sec = mktime(&time_conv);

        return(0);
    }
} /*parse_time*/

/******************************************************************************
 set stdin to be unbuffered and non-blocking
******************************************************************************/

int
set_stdin(int vmin) {
    struct termios      buf;

    if (tcgetattr(STDIN_FILENO, &save_termios) < 0)
        return(-1);

    buf = save_termios;

/* echo off, canonical mode off, extended input processing off, signal chars
   off */

    buf.c_lflag &= ~(ICANON | IEXTEN | ECHONL | ISIG);

/* no SIGINT on BREAK, input parity check off, don't strip 8th bit on input,
   output flow control off */

    buf.c_iflag &= ~(BRKINT | INPCK | ISTRIP | IXON);

/* clear size bits, parity checking off */

    buf.c_cflag &= ~(CSIZE | PARENB);

/* set 8 bits/char */

    buf.c_cflag |= CS8;

/* 1 byte at a time and no timer */

    buf.c_cc[VMIN] = vmin;
    buf.c_cc[VTIME] = 0;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &buf) < 0)
        return(-1);

    return(0);

} /*set_stdin*/

/******************************************************************************
 reset stdin to default behavior
******************************************************************************/

int
reset_stdin(void) {

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &save_termios) < 0)
        return(-1);

    return(0);

} /*reset_stdin*/

/******************************************************************************
 initialize settable options
******************************************************************************/

void
init_options(pqueue     *pq,
             prod_class *clssp,
             product    *prod,
             pq_match   *dir,
             char       *pattern) {

    timestampt          tv;

/* set the product class structure */

    clssp->psa.psa_val->feedtype = fdtype;
    uinfo("Feedtype set to %s", s_feedtypet(clssp->psa.psa_val->feedtype));

    strcpy(pattern,".*");
    clssp->psa.psa_val->pattern = pattern;
    regcomp(&clssp->psa.psa_val->rgx, clssp->psa.psa_val->pattern,
                REG_EXTENDED|REG_NOSUB);
    uinfo("Pattern set to \".*\"");

    clssp->from = TS_ZERO;
    uinfo("From time set to %s", ctime(&clssp->from.tv_sec));

    clssp->to = TS_ENDT;
    uinfo("To time set to %s", ctime(&clssp->to.tv_sec));

/* take care of the queue cursor */

    parse_time("NOW", &tv);
    pq_cset(pq, &tv);
    uinfo("Cursor set to %s", ctime(&tv.tv_sec));

    *dir = TV_GT;
    uinfo("Cursor direction set to TV_GT");

/* set up the product information record */
    
    strcpy(orighost,ghostname());
    prod->info.origin = orighost;
    parse_time("NOW", &tv);
    prod->info.arrival = tv;
    prod->info.feedtype = EXP;
    prod->info.seqno = seqnum++;
    strcpy(prod_ident, "TEST PRODUCT");
    prod->info.ident = prod_ident;
    uinfo("info.origin: %s\n", prod->info.origin);
    uinfo("info.arrival: %s\n", ctime(&prod->info.arrival.tv_sec));
    uinfo("info.feedtype: EXP\n");
    uinfo("info.seqno: %d\n", prod->info.seqno);
    uinfo("info.ident: %s\n", prod->info.ident);

/* allocate an MD% context */

    md5ctxp = new_MD5_CTX();
    if (md5ctxp == NULL) {
        serror("init_options: new_MD5_CTX failed");
        exit(6);
    }

} /*init_options*/

/******************************************************************************
  parse the interactive command line
******************************************************************************/

void
parse_command(char *cmd) {

    char *result;

    result = strtok(cmd," \n");

    if (!strcmp(result,"set")) {
        crec.cmd = CMD_SET;

        result = strtok(NULL," ");
        if (result == NULL) {
            crec.cmd_opt = OPT_BAD;
            crec.cmd = CMD_BAD;
            uerror("Set command must have option specified");
        }
        else if (!strcmp(result,"feedtype"))
            crec.cmd_opt = OPT_FEEDTYPE;
        else if (!strcmp(result,"pattern"))
            crec.cmd_opt = OPT_PATTERN;
        else if (!strcmp(result,"cursor"))
            crec.cmd_opt = OPT_CURSOR;
        else if (!strcmp(result,"direction"))
            crec.cmd_opt = OPT_CURSDIR;
        else if (!strcmp(result,"totime"))
            crec.cmd_opt = OPT_TOTIME;
        else if (!strcmp(result,"fromtime"))
            crec.cmd_opt = OPT_FROMTIME;
        else if (!strcmp(result,"arrival"))
            crec.cmd_opt = OPT_ARRIVAL;
        else if (!strcmp(result,"prodftype"))
            crec.cmd_opt = OPT_PRODFTYPE;
        else if (!strcmp(result,"seqno"))
            crec.cmd_opt = OPT_SEQNO;
        else if (!strcmp(result,"origin"))
            crec.cmd_opt = OPT_ORIGIN;
        else if (!strcmp(result,"ident"))
            crec.cmd_opt = OPT_IDENT;
        else {
            crec.cmd_opt = OPT_BAD;
            crec.cmd = CMD_BAD;
            uerror("Invalid option: %s", result);
        }

        if ((result = strtok(NULL,"\n")) == NULL) {
            uerror("Set command must have a value specified");
            crec.cmd = CMD_BAD;
            crec.cmd_opt = OPT_BAD;
        }
        else
            strcpy(crec.cmd_val,result);
    }

    else if (!strcmp(result,"show")) {
        crec.cmd = CMD_SHOW;

        result = strtok(NULL,"\n");
        if (result == NULL) {
            crec.cmd_opt = OPT_BAD;
            crec.cmd = CMD_BAD;
            uerror("Show command must have an option specified");
        }
        else if (!strcmp(result,"feedtype"))
            crec.cmd_opt = OPT_FEEDTYPE;
        else if (!strcmp(result,"pattern"))
            crec.cmd_opt = OPT_PATTERN;
        else if (!strcmp(result,"cursor"))
            crec.cmd_opt = OPT_CURSOR;
        else if (!strcmp(result,"direction"))
            crec.cmd_opt = OPT_CURSDIR;
        else if (!strcmp(result,"totime"))
            crec.cmd_opt = OPT_TOTIME;
        else if (!strcmp(result,"fromtime"))
            crec.cmd_opt = OPT_FROMTIME;
        else if (!strcmp(result, "arrival"))
            crec.cmd_opt = OPT_ARRIVAL;
        else if (!strcmp(result,"prodftype"))
            crec.cmd_opt = OPT_PRODFTYPE;
        else if (!strcmp(result,"seqno"))
            crec.cmd_opt = OPT_SEQNO;
        else if (!strcmp(result,"origin"))
            crec.cmd_opt = OPT_ORIGIN;
        else if (!strcmp(result,"ident"))
            crec.cmd_opt = OPT_IDENT;
        else {
            crec.cmd_opt = OPT_BAD;
            crec.cmd = CMD_BAD;
            uerror("Invalid option: %s", result);
        }
    }

    else {
        if (!strcmp(result,"read")) {
            crec.cmd = CMD_READ;
            if ((result = strtok(NULL,"\n")) == NULL) {
                uerror("read command must have a filename");
                crec.cmd = CMD_BAD;
            }
            else
                strcpy(crec.cmd_val, result);
        }

        else if (!strcmp(result,"new")) {
            crec.cmd = CMD_NEW;
            if ((result = strtok(NULL, "\n")) == NULL) {
                uerror("new command must have a product size");
                crec.cmd = CMD_BAD;
            }
            else
                strcpy(crec.cmd_val, result);
        }

        else if (!strcmp(result,"put")) {
            crec.cmd = CMD_PUT;
            if ((result = strtok(NULL, "\n")) == NULL) {
                uerror("put command must have a filename");
                crec.cmd = CMD_BAD;
            }
            else
                strcpy(crec.cmd_val, result);
        }

        else if (!strcmp(result,"write"))
            crec.cmd = CMD_WRITE;
        else if (!strcmp(result,"discard"))
            crec.cmd = CMD_DISCARD;

        else if (!strcmp(result, "display")) {
            crec.cmd = CMD_DISPLAY;
            if ((result = strtok(NULL, "\n")) == NULL) {
                uerror("display command must have a filename");
                crec.cmd = CMD_BAD;
            }
            else
                strcpy(crec.cmd_val, result);
        }

        else if (!strcmp(result, "watch"))
            crec.cmd = CMD_WATCH;

        else if (!strcmp(result,"delete")) {
            crec.cmd = CMD_DELETE;
            if ((result = strtok(NULL, "\n")) == NULL) {
                uerror("delete command must have an ending time");
                crec.cmd = CMD_BAD;
            }
            else
                strcpy(crec.cmd_val, result);
        }

        else if (!strcmp(result,"stats"))
            crec.cmd = CMD_STATS;
        else if (!strcmp(result,"help"))
            crec.cmd = CMD_HELP;
        else if (!strcmp(result,"quit"))
            crec.cmd = CMD_QUIT;
        else if (result[0] == '#')
            crec.cmd = CMD_COMMENT;
        else
            crec.cmd = CMD_BAD;

        crec.cmd_opt = OPT_NONE;
    }

} /*parse_command*/

/******************************************************************************
 "help" command - print out help screen
******************************************************************************/

void
print_help(void) {

    printf("\nset <option> <value>\n");
    printf("  options:  feedtype   [any valid LDM feedtype]\n");
    printf("            pattern   [any valid regular expression]\n");
    printf("            cursor    [0|NOW|EOT|yyyymmddhhmm]\n");
    printf("            direction [LT|EQ|GT]\n");
    printf("            totime    [0|NOW|EOT|yyyymmddhhmm]\n");
    printf("            fromtime  [0|NOW|EOT|yyyymmddhhmm]\n");
    printf("            arrival   [0|NOW|EOT|yyyymmddhhmm]\n");
    printf("            prodftype [any valid LDM feedtype]\n");
    printf("            seqno     [any positive integer]\n");
    printf("            origin    [hostname string]\n");
    printf("            ident     [product id]\n");
    printf("show <option>\n");
    printf("  options: same as for set command\n");
    printf("read <filename> (use - for stdin)\n");
    printf("new <product size>\n");
    printf("put <filename> (use - for stdin)\n");
    printf("write\n");
    printf("discard\n");
    printf("display <filename> (use - for stdout)\n");
    printf("watch\n");
    printf("delete totime [0|NOW|EOT|yyyymmddhhmm]\n");
    printf("stats\n");
    printf("quit\n\n");

} /*print_help*/

/******************************************************************************
 "stat" command - print out queue stats:
      system page size
      queue page size
      maximum bytes used
      maximum number of products
******************************************************************************/

void
print_stats(pqueue *pq) {

    off_t       highwater;                                 /* max bytes used */
    size_t      maxprods;                               /* max products held */

    printf("\nSystem Page Size:\t%20d\n",pq_pagesize(NULL));
    printf("Queue Page Size:\t%20d\n",pq_pagesize(pq));

    pq_highwater(pq, &highwater, &maxprods);
#ifdef LONGLONG_MAX
    if (sizeof(off_t) > sizeof(long))
        printf("Maximum Bytes Used:\t%20lld\n", (long long)highwater);
    else
#else
        printf("Maximum Bytes Used:\t%20ld\n", (long)highwater);
#endif
    printf("Maximum Products Held:\t%20lu\n\n", (unsigned long)maxprods);

} /*print_stats*/

/******************************************************************************
 "set" command - set an option value
******************************************************************************/

void
set_option(prod_class *clssp,
           pqueue *pq,
           product *prod_rec,
           pq_match *dir,
           char *pattern) {

    feedtypet   ftype;
    int         result;
    timestampt  curstime;                           /* cursor time structure */

    switch (crec.cmd_opt) {
    case OPT_FEEDTYPE:                             /* set the class feedtype */
        if ((result = strfeedtypet(crec.cmd_val,&ftype)) != FEEDTYPE_OK) {
            uerror("set_option: %s: %s\n", crec.cmd_val, strfeederr(result));
            break;
        }
        clssp->psa.psa_val->feedtype = ftype;
        uinfo("Feedtype set to %s", crec.cmd_val);
        break;

    case OPT_PATTERN:                      /* set the product header pattern */
        if (re_isPathological(crec.cmd_val)) {
            uwarn("Adjusting pathological regular-expression: \"%s\"",
                crec.cmd_val);
            re_vetSpec(crec.cmd_val);
        }
        if ( regcomp(&clssp->psa.psa_val->rgx, crec.cmd_val,
                REG_EXTENDED|REG_NOSUB) != 0) {
            uerror("Invalid regular expresssion: %s", crec.cmd_val);
            break;
            /* TODO: inconsistant state on error */
        }
        strcpy(pattern, crec.cmd_val);
        clssp->psa.psa_val->pattern = pattern;
        uinfo("Pattern set to \"%s\"", crec.cmd_val);
        break;

    case OPT_CURSOR:                                       /* set the cursor */
        if (parse_time(crec.cmd_val,&curstime)) {          /* bad time value */
            uerror("Invalid cursor time specification: %s", crec.cmd_val);
            break;
        }
        pq_cset(pq, &curstime);
        uinfo("Cursor time set to %s", ctime(&curstime.tv_sec));
        break;

    case OPT_CURSDIR:                            /* set the cursor direction */
        if (!strcmp(crec.cmd_val,"LT")) {
            *dir = TV_LT;
            uinfo("Cursor direction set to TV_LT");
        }

        else if (!strcmp(crec.cmd_val, "EQ")) {
            *dir = TV_EQ;
            uinfo("Cursor direction set to TV_EQ");
        }

        else if (!strcmp(crec.cmd_val, "GT")) {
            *dir = TV_GT;
            uinfo("Cursor direction set to TV_GT");
        }

        else
            uerror("Invalid direction specified: %s", crec.cmd_val);
        break;

    case OPT_TOTIME:                               /* set prod class to time */
        if (parse_time(crec.cmd_val,&curstime)) {          /* bad time value */
            uerror("Invalid \"to\" time specification: %s", crec.cmd_val);
            break;
        }
        clssp->to = curstime;
        uinfo("To time set to %s", ctime(&clssp->to.tv_sec));
        break;

    case OPT_FROMTIME:                           /* set prod class from time */
        if (parse_time(crec.cmd_val,&curstime)) {          /* bad time value */
            uerror("Invalid \"from\" time specification: %s", crec.cmd_val);
            break;
        }
        clssp->from = curstime;
        uinfo("From time set to %s", ctime(&clssp->from.tv_sec));
        break;

    case OPT_ARRIVAL:                                /* set the arrival time */
        if (parse_time(crec.cmd_val,&curstime)) {          /* bad time value */
            uerror("Invalid \"arrival\" time specification: %s", crec.cmd_val);
            break;
        }
        prod_rec->info.arrival = curstime;
        uinfo("Arrival time set to %s", ctime(&prod_rec->info.arrival.tv_sec));
        break;

    case OPT_PRODFTYPE:
        if ((result = strfeedtypet(crec.cmd_val,&ftype)) != FEEDTYPE_OK) {
            uerror("set_option: %s: %s\n", crec.cmd_val, strfeederr(result));
            break;
        }
        prod_rec->info.feedtype = ftype;
        uinfo("Feedtype set to %s", crec.cmd_val);
        break;

    case OPT_SEQNO:
        seqnum = atoi(crec.cmd_val);
        prod_rec->info.seqno = seqnum++;
        uinfo("Sequence Number set to %d", prod_rec->info.seqno);
        break;

    case OPT_ORIGIN:
        strcpy(prod_rec->info.origin, crec.cmd_val);
        uinfo("Origin set to %s", prod_rec->info.origin);
        break;

    case OPT_IDENT:
        strcpy(prod_rec->info.ident,crec.cmd_val);
        uinfo("Product ID set to %s", prod_rec->info.ident);
        break;
    }

} /*set_option*/

/******************************************************************************
 "show" command - show an option value
******************************************************************************/

void
show_option(const prod_class *clssp,
            pqueue *pq,
            product *prodp,
            pq_match dir,
            const char *pattern) {

    timestampt  tv;

    switch (crec.cmd_opt) {
    case OPT_FEEDTYPE:                            /* show the class feedtype */
        printf("Feedtype is %s\n",s_feedtypet(clssp->psa.psa_val->feedtype));
        break;

    case OPT_PATTERN:                     /* show the product header pattern */
        printf("Pattern is \"%s\"\n", clssp->psa.psa_val->pattern);
        break;

    case OPT_CURSOR:                                      /* show the cursor */
        pq_ctimestamp(pq, &tv);
        printf("Cursor time is %s", ctime(&tv.tv_sec));
        break;

    case OPT_CURSDIR:                           /* show the cursor direction */
        if (dir == TV_LT)
            printf("Cursor direction is TV_LT\n");
        else if (dir == TV_EQ)
            printf("Cursor direction is TV_EQ\n");
        else
            printf("Cursor direction is TV_GT\n");
        break;

    case OPT_TOTIME:                              /* show prod class to time */
        printf("To time set to %s", ctime(&clssp->to.tv_sec));
        break;

    case OPT_FROMTIME:                          /* show prod class from time */
        printf("From time set to %s", ctime(&clssp->from.tv_sec));
        break;

    case OPT_ARRIVAL:                           /* show product arrival time */
        printf("Arrival time set to %s", ctime(&prodp->info.arrival.tv_sec));
        break;

    case OPT_PRODFTYPE:
        printf("Product feedtype is %s\n", s_feedtypet(prodp->info.feedtype));
        break;

    case OPT_SEQNO:
        printf("Seqence number is %d\n", seqnum - 1);
        break;

    case OPT_ORIGIN:
        printf("Product origin is %s\n", prodp->info.origin);
        break;

    case OPT_IDENT:
        printf("Product ID is \"%s\"\n", prodp->info.ident);
        break;
    }

} /*show_option*/

/******************************************************************************
 read a file containing a single raw product.  product can be of any size.
 if larger than DBUFMAX, then product is treated as COMINGSOON,BLKDATA.  If
 filename is a "-", then stdin is read.
******************************************************************************/

void
read_file(pqueue *pq,
          product *prodrec) {

    int fd;                                         /* input file descriptor */
    int status;                                     /* function return value */
    struct stat f_stat;                                 /* file stats struct */

/* open the file if not stdin */

    if (!strcmp(crec.cmd_val,"-"))                         /* input is stdin */
        fd = STDIN_FILENO;

    else {

        if((fd = open(crec.cmd_val, O_RDONLY)) < 0) {            /* bad open */
            serror("Error opening %s", crec.cmd_val);
            return;
        }
    }

/* if input is a file do a read() to get the data */

    if (fd != STDIN_FILENO) {

/* how we handle this depends on the size */

        fstat(fd, &f_stat);
        udebug("read_file: File size is %d", f_stat.st_size);

        if (f_stat.st_size <= DBUFMAX) {                    /* small product */
            prodrec->info.sz = f_stat.st_size;
            prodrec->data = malloc(prodrec->info.sz);

            status = read(fd, prodrec->data, prodrec->info.sz);
            if (status < 0) {
                serror("Bad file read: %s", crec.cmd_val);
                free(prodrec->data);
                return;
            }

/* calculate the MD5 checksum */

            MD5Init(md5ctxp);
            MD5Update(md5ctxp, (const unsigned char *)&(prodrec->data),
                      prodrec->info.sz);
            MD5Final((unsigned char*)prodrec->info.signature, md5ctxp);

            status = pq_insert(pq,prodrec);
            if (status) {
                uerror("pq_insert: %s", strerror(status));
                free(prodrec->data);
                return;
            }

/* clean up and set up for the next product */

            prodrec->info.seqno = seqnum++;
            free(prodrec->data);
        }

        else {                                                /* big product */
            char                *dbuf;
            pqe_index   idx;

            prodrec->info.sz = f_stat.st_size;
            status = pqe_new(pq, &(prodrec->info),
                             (void **)&dbuf, &idx);
            if (status) {
                uerror("pqe_new: %s", strerror(status));
                return;
            }

            status = read(fd, dbuf, prodrec->info.sz);
            if (status < 0) {
                serror("Bad file read: %s", crec.cmd_val);
                return;
            }

/* calculate the MD5 checksum */

            MD5Init(md5ctxp);
            MD5Update(md5ctxp, (const unsigned char *)dbuf, prodrec->info.sz);
            MD5Final((unsigned char*)prodrec->info.signature, md5ctxp);

            status = pqe_insert(pq, idx);
            if (status) {
                uerror("pqe_insert: %s", strerror(status));
                return;
            }

/* get ready for next product */

            prodrec->info.seqno = seqnum++;
        }
    }

/* read from stdin - terminate with ^D */

    else {
        char    ch;                                       /* input character */
        int     bufcnt = 0;                     /* number of bytes in buffer */

        if((prodrec->data = malloc(DBUFMAX)) == NULL) {
            serror("read_file: malloc failed");
            return;
        }

        if (tty_flag) {
            printf("Enter product (^D when finished)\n");
            if (set_stdin(1)) {
                uerror("set_stdin: noncanonical mode set failed");
                return;
            }
        }


        while ((ch = getc(stdin)) != '\004') {
            *((char *)prodrec->data + bufcnt) = ch;
            bufcnt++;

            if (bufcnt > DBUFMAX) {
                uerror("\nProduct must be smaller than %d bytes", DBUFMAX);
                uerror("Read operation aborted");
                free(prodrec->data);
                return;
            }
        }

        prodrec->info.sz = bufcnt;

/* calculate the MD5 checksum */

        MD5Init(md5ctxp);
        MD5Update(md5ctxp, (const unsigned char *)prodrec->data, bufcnt);
        MD5Final((unsigned char*)prodrec->info.signature, md5ctxp);

        status = pq_insert(pq,prodrec);
        if (status != 0 && status != PQUEUE_DUP) {
            uerror("pq_insert: %s", strerror(status));
            free(prodrec->data);
            return;
        }

/* clean up and set up for the next product */

        prodrec->info.seqno = seqnum++;
        free(prodrec->data);

        if (tty_flag) {
            if (reset_stdin())
                uerror("reset_stdin: stdin reset failed");
            printf("\n");
        }
    }

/* clean up the mess */

    if (fd > 0)                                                 /* not stdin */
        close(fd);

} /*read_file*/

/******************************************************************************
 Handle a multi-part product.  This function is called by the new, put, write
 and discard commands.
******************************************************************************/

void
big_product(pqueue      *pq,
            product     *prec) {

    static int  in_progress = 0;                 /* product in progress flag */
    static pqe_index    idx;                    /* product in progress index */
    static char *dbuf = 0;                     /* product in progress buffer */
    static char *prod_ptr = 0;                      /* buffer insert pointer */
    static int  prod_sz = 0;                     /* product in progress size */
    static int  prod_cnt = 0;            /* product in progress size to date */

    int         status;                            /* function return status */

    switch (crec.cmd) {
    case CMD_NEW:                                     /* start a new product */
        if (in_progress) {               /* a product is already in progress */
            uerror("Must complete old product first");
            return;
        }

        prod_sz = atoi(crec.cmd_val);

        prec->info.sz = prod_sz;
        status = pqe_new(pq, &(prec->info), (void **)&dbuf, &idx);
        if (status) {
            uerror("pq_new: %s", strerror(status));
            return;
        }

        prod_ptr = dbuf;
        in_progress = 1;

        break;

    case CMD_PUT:                             /* read a piece of the product */
        if (!in_progress) {              /* must initialize with new command */
            uerror("Must start product with \"new\" command");
            return;
        }

        if (!strcmp(crec.cmd_val, "-")) {    /* get product piece from stdin */
            int         ch;                               /* input character */

            if (tty_flag)
                printf("Enter product piece (^D when finished): ");

            while ((ch = fgetc(stdin)) != EOF) {
                if (++prod_cnt > prod_sz) {
                    uerror("\nproduct exceeds buffer size: aborting input");
                    fflush(stdin);
                    prod_cnt--;
                    return;
                }
                *prod_ptr++ = ch;
            }
        }

        else {                                /* get product piece from file */
            struct stat f_stat;                         /* file stats struct */
            int fd;                                 /* input file descriptor */

            if ((fd = open(crec.cmd_val, O_RDONLY)) < 0) {       /* bad open */
                serror("Error opening %s", crec.cmd_val);
                return;
            }

            fstat(fd, &f_stat);                     /* we need the file size */
            udebug("big_product: File size is %d", f_stat.st_size);

            if ((prod_cnt + f_stat.st_size) > prod_sz) {  /* file is too big */
                uerror("Product exceeds allocated size: input aborted");
                close(fd);
                return;
            }

            status = read(fd, prod_ptr, f_stat.st_size);
            if (status < 0) {
                serror("Bad file read: %s", strerror(status));
                return;
            }

            prod_ptr += f_stat.st_size;
            prod_cnt += f_stat.st_size;

            close(fd);
        }

        break;

    case CMD_WRITE:                     /* insert the product into the queue */
/* calculate the MD5 checksum */

        MD5Init(md5ctxp);
        MD5Update(md5ctxp, (const unsigned char *)dbuf, prod_sz);
        MD5Final((unsigned char*)prec->info.signature, md5ctxp);

        status = pqe_insert(pq, idx);
        if (status) {
            uerror("pqe_insert: %s", strerror(status));
            return;
        }

        in_progress = 0;
        prod_cnt = 0;

        break;

    case CMD_DISCARD:                       /* discard a product in progress */
        if (!in_progress) {
            uerror("No product in progress: nothing to discard");
            return;
        }

        status = pqe_discard(pq, idx);
        if (status) {
            uerror("pqe_discard: %s", strerror(status));
            return;
        }

        in_progress = 0;
        prod_cnt = 0;
    }

} /*big_product*/

/******************************************************************************
 display prodcut to stdout.  Called from pq_sequence.
******************************************************************************/

static int
disp_stdout(const prod_info     *infop,
            const void          *datap,
            void                *xprod,
            size_t              size_notused,
            void                *notused) {

    if (write(STDOUT_FILENO, datap, infop->sz) !=
        infop->sz) {
        int errnum = errno;

        serror("disp_stdout: data write failed");
        return(errnum);
    }

    printf("\n");

    return(0);

} /*disp_stdout*/

/******************************************************************************
 display product to a file. Called from pq_sequence.
******************************************************************************/

static int
disp_file(const prod_info       *infop,
          const void            *datap,
          void                  *xprod,
          size_t                size_notused,
          void                  *notused) {

    int fd;                                        /* output file discriptor */
    int errnum;                                        /* system error value */
    
    if ((fd = open(crec.cmd_val, O_CREAT|O_WRONLY|O_APPEND, 0664)) < 0) {
                                                                 /* bad open */
        errnum = errno;
        serror("Error opening %s", crec.cmd_val);
        return(errnum);
    }

    if (write(fd, datap, infop->sz) != infop->sz) {
        errnum = errno;
        serror("disp_file: data write failed");
        close(fd);
        return(errnum);
    }

    close(fd);
    return(0);

} /*disp_file*/

/******************************************************************************
 display all product matching the product class in the direction specified.
 Products are either written to stdout or to a file.
******************************************************************************/

void
display_product(pqueue          *pq,
                prod_class      *clssp,
                pq_match        direction) {

    int status = 0;                                /* function return status */

    while (1) {                   /* loop exit is controlled inside the loop */
        if (!strcmp(crec.cmd_val, "-"))                   /* write to stdout */
            status = pq_sequence(pq, direction, clssp, disp_stdout, 0);
        else                                                /* write to file */
            status = pq_sequence(pq, direction, clssp, disp_file, 0);

        switch(status) {
        case 0:                                                  /* no error */
            break;

        case PQUEUE_END:
            udebug("End of queue");
            return;
            break;

        default:
            uerror("pq_sequence: %s", strerror(status));
            return;
            break;
        }
    }
} /*display_product*/

/******************************************************************************
 display product information from the watch command.  Called from pq_sequence
******************************************************************************/

static int
display_watch(const prod_info   *infop,
              const void        *datap,
              void              *xprod,
              size_t            size_notused,
              void              *notused) {

    unsigned int        oldMask = setulogmask(0u);

    (void)setulogmask(oldMask | LOG_MASK(LOG_INFO));
    uinfo("%s", s_prod_info(NULL, 0, infop, ulogIsDebug()));
    (void)setulogmask(oldMask);

    return ENOERR;
} /*display_watch*/

/******************************************************************************
 display products as the arrive in the queue
******************************************************************************/

void
watch_queue(pqueue      *pq,
            prod_class  *clssp) {
    char        ch;
    int         status;
    int         keep_at_it = 1;
    timestampt  tvout;

/* stuff for the select call */

    int         ifd = fileno(stdin);
    int         ready;
    int         width = (1 << ifd);
    fd_set      readfds;
    struct timeval      timeo;

/* we need the cursor time of the end of the queue */

    clssp->from = TS_ZERO;
    clssp->to = TS_ENDT;
    uinfo("From time set to %s", ctime(&clssp->from.tv_sec));
    uinfo("To time set to %s", ctime(&clssp->to.tv_sec));
    pq_last(pq, clssp, &tvout);

    if (tty_flag) {
        printf("(Type ^D when finished)\n");
        if (set_stdin(0)) {
            uerror("set_stdin: noncanonical mode set failed");
            return;
        }
    }

    while(keep_at_it) {
        FD_ZERO(&readfds);
        FD_SET(ifd, &readfds);
        timeo.tv_sec = 1;
        timeo.tv_usec = 0;
        ready = select(width, &readfds, 0, 0, &timeo);

        if (ready < 0) {
            if (errno == EINTR) {
                errno = 0;
                continue;
            }
            serror("select");
            exit(1);
        }

        if (ready > 0) {
            if  (FD_ISSET(ifd, &readfds)) {
                ch = getc(stdin);
                if (ch != '' && ch != '') {
                    while(1) {
                        status = pq_sequence(pq, TV_GT, clssp, display_watch,
                                             0);

                        if (status == 0)
                            continue;
                        else if (status == PQUEUE_END){
                            udebug("End of queue");
                            break;
                        }
                        else {
                            printf("status: %d\n",status);
                            uerror("pq_sequence: %s", strerror(status));
                            return;
                        }
                    }
                }
                else {
                    keep_at_it = 0;
                }
            }
        }
        else {
            while(1) {
                status = pq_sequence(pq, TV_GT, clssp, display_watch, 0);

                if (status == 0)
                    continue;
                else if (status == PQUEUE_END){
                    udebug("End of queue");
                    break;
                }
                else {
                    printf("status: %d\n",status);
                    uerror("pq_sequence: %s", strerror(status));
                    return;
                }
            }
        }
    }

    if (tty_flag) {
        if (reset_stdin())
            uerror("reset_stdin: failed");
        printf("\n");
    }

    return;
            
} /*watch_queue*/

/******************************************************************************
 delete product from the queue.
******************************************************************************/

void
rm_prod(pqueue          *pq,
        prod_class      *clssp,
        pq_match        direction) {

    int status;                                     /* function return value */
    size_t      savail;                                /* queue spaced freed */
    timestampt  ins_time;               /* insertion time of deleted product */

    while (1) {                   /* loop exit is controlled inside the loop */
        status = pq_seqdel(pq, direction, clssp, 0, &savail, &ins_time);

        switch(status) {
        case 0:                                                  /* no error */
            uinfo("%d bytes freed: %s", savail, ctime(&ins_time.tv_sec));
            break;

        case PQUEUE_END:
            udebug("End of queue");
            return;
            break;

        default:
            uerror("pq_seqdel: %s", strerror(status));
            return;
            break;
        }
    }
} /*rm_prod*/

/******************************************************************************
  program main
******************************************************************************/

int
main(int argc, char *argv[]) {

    char        *path;                                 /* product queue path */
    char        *logname = 0;            /* logfile name - STDERR by default */
    const char  *logident = ubasename(argv[0]);     /* log file program name */
    char        command[MAX_COMMAND];                /* input command buffer */
    char        pattern[128];                /* product class pattern string */
    pqueue      *pq;                             /* pointer to product queue */
    int         aflags = PQ_DEFAULT;             /* product queue open flags */
    int         status = ENOERR;                   /* function return status */
    int         create = 0;                             /* queue create flag */
    int         logopts = (LOG_CONS|LOG_PID);             /* logging options */
    int         watch_flag = 0;                        /* watch command flag */
    off_t       initialsz = 0;    /* initial product queue data section size */
    size_t      align = 0;                               /* alignment factor */
    size_t      nproducts = 0;         /* number of products for index space */
    pq_match    cdir = (pq_match)0;             /* cursor direction */
    prod_class_t  clss;                        /* product class record */
    prod_spec   spec;
    product     prodrec;                                   /* product struct */

/* set the timezone to be GMT */

    putenv("TZ=GMT");

/* if interactive set the logging for STDERR */

    if (isatty(fileno(stderr))) {
        logname = "-";
        logopts = 0;
    }

    if (isatty(fileno(stdin)))
        tty_flag = 1;

    clss.psa.psa_val = &spec;
    clss.psa.psa_len = 1;

/* process the command line */

    {
        extern int optind;
        extern int opterr;
        extern char *optarg;
        int ch;
        int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) |
            LOG_MASK(LOG_NOTICE));

        opterr = 1;

        while ((ch=getopt(argc, argv, "vxl:pa:cs:nrPLFMS:wf:")) != EOF)
            switch (ch) {
            case 'v':
                logmask |= LOG_MASK(LOG_INFO);
                break;

            case 'x':
                logmask |= LOG_MASK(LOG_DEBUG);
                break;

            case 'l':
                logname = optarg;
                break;

            case 'a':                                               /* align */
                align = atol(optarg);
                if (align == 0)
                    fprintf(stderr, "Illegal align \"%s\", ignored\n", optarg);
                break;

            case 'S':                                  /* number of products */
                nproducts = atol(optarg);
                if(nproducts == 0)
                    fprintf(stderr, "Illegal nproducts \"%s\", ignored\n",
                            optarg);
                break;

            case 'c':                           /* create queue with clobber */
                create = 1;
                break;

            case 'n':                         /* create queue with noclobber */
                create = 1;
                aflags |= PQ_NOCLOBBER;
                break;

            case 's': {                                /* initial queue size */
                char *cp = optarg + strlen(optarg) - 1;    /* last character */
                initialsz = atol(optarg);
                if (isalpha(*cp))
                    switch (*cp) {
                    case 'k':
                    case 'K':
                        initialsz *= 1024;
                        break;

                    case 'm':
                    case 'M':
                        initialsz += (1024 * 1024);
                        break;

                    default:
                        initialsz = 0;                /* trigger error below */
                        break;
                    }

                if (initialsz == 0)
                    fprintf(stderr, "Illegal intialsz \"%s\", ignored\n",
                            optarg);

                break;

            }

            case 'p':                                   /* align on pagesize */
                align = pq_pagesize(NULL);
                break;

            case 'r':                                /* open queue read only */
                aflags |= PQ_READONLY;
                break;

            case 'P':                                 /* open queue no share */
                aflags &= PQ_PRIVATE;
                break;

            case 'L':                          /* open queue with no locking */
                aflags &= PQ_NOLOCK;
                break;

            case 'F':                             /* open queue with no grow */
                aflags |= PQ_NOGROW;
                break;

            case 'M':                   /* open queue with no mmap interface */
                aflags |= PQ_NOMAP;
                break;

            case 'w':                                  /* set the watch flag */
                logname = "-";
                logopts = 0;
                watch_flag = 1;
                break;

            case 'f':                                    /* set the feedtype */
                if (strfeedtypet(optarg,&fdtype) != FEEDTYPE_OK) {
                    fprintf(stderr, "Invalid feedtype: %s\n",optarg);
                    usage(argv[0]);
                }
                break;

            case '?':                                        /* bad argument */
                usage(argv[0]);
                break;
            }

/* product queue path is required */

        if (argc - optind <= 0)
            usage(argv[0]);
        path = argv[optind];

/* set the logging mask */

        setulogmask(logmask);
    }

/* initialize logger */

    (void)openulog(logident, logopts, LOG_LDM, logname);

/* open the queue */

    if (!create) {                                    /* open existing queue */
        status = pq_open(path, aflags, &pq);
        if (status != ENOERR) {                                    /* whoops */
            if (PQ_CORRUPT == status) {
                uerror("The product-queue \"%s\" is inconsistent\n",
                        path);
            }
            else {
                uerror("pq_open: %s: %s\n", path, strerror(status));
            }
            exit(EXIT_FAILURE);
        }
        udebug("pq_open: %s: Success\n", path);
    }

    else {                                               /* create new queue */
        status = pq_create(path, 0666, aflags, align, initialsz, nproducts,
                           &pq);
        if (status != ENOERR) {                                    /* whoops */
            uerror("pq_create: %s: %s\n", path, strerror(status));
            exit(EXIT_FAILURE);
        }
        udebug("pq_create: %s: Success\n", path);
    }

/* initialize options */

    init_options(pq, &clss, &prodrec, &cdir, pattern);

/* if the watch flag is set then we just want to do the watch command and
   then exit */

    if (watch_flag) {
        watch_queue(pq, &clss);
        exit(0);
    }

/* main process loop */

    if (tty_flag)
        printf("Enter Command --> ");

    while(fgets(command, sizeof(command), stdin) != NULL) {
        char            *work_str;      /* copy of command line for strtok() */


        command[strlen(command) - 1] = '\0';  /* get rid of trailing newline */
        work_str = strdup(command);

        if (strlen(command) > (size_t)0) {
            parse_command(work_str);                 /* break out the pieces */

            switch (crec.cmd) {                      /* do something with it */
            case CMD_SET:                                     /* set a value */
                set_option(&clss, pq, &prodrec, &cdir, pattern);
                break;

            case CMD_SHOW:                                    /* show values */
                show_option(&clss, pq, &prodrec, cdir, pattern);
                break;

            case CMD_READ:                               /* read a data file */
                read_file(pq, &prodrec);
                break;

            case CMD_NEW:                      /* set up for a large product */
                big_product(pq, &prodrec);
                break;

            case CMD_PUT:                        /* put a peice of a product */
                big_product(pq, &prodrec);
                break;

            case CMD_WRITE:            /* write completed multi-part product */
                big_product(pq, &prodrec);
                break;
               
            case CMD_DISCARD:                     /* discard partial product */
                big_product(pq, &prodrec);
                break;

            case CMD_DISPLAY:                    /* display product in queue */
                display_product(pq, &clss, cdir);
                break;

            case CMD_WATCH:        /* monitor products coming into the queue */
                watch_queue(pq, &clss);
                break;

            case CMD_DELETE:                             /* delete a product */
                rm_prod(pq, &clss, cdir);
                break;

            case CMD_STATS:                          /* print out statistics */
                print_stats(pq);
                break;

            case CMD_HELP:                              /* print help screen */
                print_help();
                break;

            case CMD_QUIT:                          /* beat feet and go home */
                exit_prog(pq);
                break;

            case CMD_COMMENT:                        /* comment line, ignore */
                break;

            case CMD_BAD:                         /* invalid command entered */
                uerror("Bad command(type help for command list): %s", command);
                break;
            }
        }

        free(work_str);

        if (tty_flag)
            printf("Enter Command --> ");
    }

/* close the queue and beat feet */

    pq_close(pq);
    exit(EXIT_SUCCESS);
    
} /*main*/
