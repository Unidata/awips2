/*
 *   Copyright 2003, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: regex.c,v 1.2.18.1 2005/01/21 21:35:15 steve Exp $ */


#include "ldmconfig.h"

#include <sys/types.h>  /* necessary for "off_t" under FreeBSD 4.7-STABLE */
#include <regex.h>
#include <stdio.h>
#include <unistd.h>


/*
 * This utility is useful for validating extended regular expressions used by
 * the LDM package.  In particular, a looping facility permits the determination
 * of the relative efficiency of, otherwise, equivalent regular-expressions.
 * For example, "[09]$" is MUCH faster than ".*[09]$" under Solaris:
 *
 * $ time ./regex -n 10000 -s 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' '.*[09]$'
 * no match
 * 
 * real    0m7.186s
 * user    0m7.160s    <-- NOTE
 * sys     0m0.010s
 * $ time ./regex -n 10000 -s 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' '[09]$'
 * no match
 * 
 * real    0m0.016s
 * user    0m0.000s    <-- NOTE
 * sys     0m0.010s
 *
 * Usage:
 *   regex [-n count] [-s string] pattern
 *
 *   where:
 *     count   - Number of times to match the regular-expression against the 
 *               string.  Default is 1.
 *     string  - The string to be matched against the regular-expression.  
 *     pattern - The regular-expression to be matched against the string.
 *
 * If no string is specified, then the regular-expression is simply compiled and
 * not matched against anything.
 *
 * If a string is specified and the regular expression is successfully compiled,
 * then the utility prints "match" or "no match" as appropriate to standard
 * output.
 *
 * Exit codes:
 *   0 - Success
 *   1 - Invalid usage.
 *   2 - Regular-expression failure.
 */
int main(const int argc, char* const argv[])
{
    int          retCode = 0;                   /* return code */
    int          ch;                            /* option character */
    int          count = 1;                     /* loop count */
    const char*  string = NULL;                 /* string to be matched */
    const char*  pattern;                       /* regular-expression */

    /*
     * Decode options.
     */
    while ((ch = getopt(argc, argv, "n:s:")) != -1) {
        switch (ch) {
            case 'n': {
                int i = sscanf(optarg, "%d", &count);

                if ((1 != i) || (1 > count)) {
                    fprintf(stderr, "Invalid \"-n\" argument: \"%s\"\n",
                        optarg);

                    retCode = 1;                /* usage error */
                }
                break;
            }

            case 's':
                string = optarg;
                break;
        }
    }

    /*
     * Obtain regular-expression string.
     */
    if (argc - 1 == optind) {
        pattern = argv[optind];
    }
    else {
        retCode = 1;                            /* usage error */
    }

    if (retCode) {
        fprintf(stderr, "Usage: %s [-n count] [-s string] pattern\n", argv[0]);
    }
    else {
        char    regMsg[512];                    /* RE error-message buffer */
        regex_t regExp;                         /* compiled RE */
        int     regCode;                        /* RE status */

        /*
         * Compile regular-expression string.
         */
        regCode = regcomp(&regExp, pattern, REG_EXTENDED);

        if (regCode != 0) {
            (void)regerror(regCode, &regExp, regMsg, sizeof(regMsg));
            (void)fprintf(stderr, "regcomp() failure: %s\n", regMsg);

            retCode = 2;                        /* recomp() failure */
        }
        else if (NULL != string) {
            int i;
            int matches;

            /*
             * Match compiled regular-expression against string for specified
             * number of times.
             */
            for (i = 0; i < count; i++) {       /* count > 0 */
                regCode = regexec(&regExp, string, 0, NULL, 0);

                if (0 == regCode) {
                    matches = 1;
                }
                else if (REG_NOMATCH == regCode) {
                    matches = 0;
                }
                else {
                    retCode = 2;                /* regexec() failure */
                    break;
                }
            }

            if (!retCode) {
                (void)printf("%s\n", matches ? "match" : "no match");
            }
            else {
                (void)regerror(regCode, &regExp, regMsg, sizeof(regMsg));
                (void)fprintf(stderr, "regexec() failure: %s\n", regMsg);
            }

            regfree(&regExp);
        }
    }

    return retCode;
}
