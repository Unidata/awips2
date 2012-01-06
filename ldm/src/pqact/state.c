/*
 *   Copyright 2007, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: state.c,v 1.1.2.4 2008/09/04 20:12:13 steve Exp $ */

/*
 * Persists state (e.g., time of last-processed data-product) of pqact(1)
 * processes between invocations.
 */

#include <ldmconfig.h>
#include <assert.h>
#include <libgen.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "timestamp.h"
#include "log.h"

#include "state.h"

static char*    statePathname = NULL;
static char*    tmpStatePathname = NULL;


int
stateInit(
    const char* const   configPathname)
{
    int         status;

    if (configPathname == NULL) {
        log_start("stateInit(): Pathname is NULL");
        status = -1;
    }
    else {
        static const char* const        extension = ".state";
        size_t                          pathlen = 
            strlen(configPathname) + strlen(extension) + 1;
        char*                           newPath = malloc(pathlen);

        if (newPath == NULL) {
            log_errno();
            log_add("stateInit(): Couldn't allocate %lu-byte pathname",
                (unsigned long)pathlen);
            status = -2;
        }
        else {
            static const char* const    tmpExt = ".tmp";
            char*                       newTmpPath;

            pathlen += strlen(tmpExt);
            newTmpPath = malloc(pathlen);

            if (newTmpPath == NULL) {
                log_errno();
                log_add("stateInit(): Couldn't allocate %lu-byte pathname",
                    (unsigned long)pathlen);
                status = -2;
            }
            else {
                (void)strcpy(newPath, configPathname);
                (void)strcat(newPath, extension);
                free(statePathname);
                statePathname = newPath;

                (void)strcpy(newTmpPath, newPath);
                (void)strcat(newTmpPath, tmpExt);
                free(tmpStatePathname);
                tmpStatePathname = newTmpPath;

                status = 0;
            }
        }
    }                                   /* configPathname != NULL */

    return status;
}


int
stateRead(
    timestampt* const   pqCursor)
{
    int         status;

    if (statePathname == NULL) {
        log_start("stateRead(): stateInit() not successfully called");
        status = -1;                    /* module not initialized */
    }
    else {
        FILE*   file = fopen(statePathname, "r");

        if (file == NULL) {
            log_errno();
            log_add("stateRead(): Couldn't open \"%s\"", statePathname);
            status = -2;                /* couldn't open state-file */
        }
        else {
            int         c;
            char        comment[1];

            status = -3;                /* state-file is corrupt */

            while ((c = fgetc(file)) == '#')
                (void)fscanf(file, "%*[^\n]\n", comment);

            if (ferror(file)) {
                log_errno();
                log_add("stateRead(): Couldn't read comments from \"%s\"",
                    statePathname);
            }
            else {
                unsigned long   seconds;
                long            microseconds;

                ungetc(c, file);

                if (fscanf(file, "%lu.%ld", &seconds, &microseconds) != 2) {
                    log_errno();
                    log_add("stateRead(): Couldn't read time from \"%s\"",
                        statePathname);
                }
                else {
                    pqCursor->tv_sec = seconds;
                    pqCursor->tv_usec = microseconds;
                    status = 0;         /* success */
                }                       /* read time */
            }                           /* read comments */

            (void)fclose(file);
        }                               /* file open */
    }                                   /* module initialized */

    return status;
}


int
stateWrite(
    const timestampt* const     pqCursor)
{
    int         status;

    if (statePathname == NULL) {
        log_start("stateWrite(): stateInit() not successfully called");
        status = -1;
    }
    else {
        FILE*   file = fopen(tmpStatePathname, "w");

        if (file == NULL) {
            log_errno();
            log_add("stateWrite(): Couldn't open \"%s\"", tmpStatePathname);
            status = -2;
        }
        else {
            status = -3;

            if (fputs(
"# The following line contains the insertion-time of the last, successfully-\n"
"# processed data-product.  Do not modify it unless you know exactly what\n"
"# you're doing!\n", file) < 0) {
                log_errno();
                log_add("stateWrite(): Couldn't write comment to \"%s\"",
                    tmpStatePathname);
            }
            else {
                if (fprintf(file, "%lu.%06lu\n",
                        (unsigned long)pqCursor->tv_sec, 
                        (unsigned long)pqCursor->tv_usec) < 0) {
                    log_errno();
                    log_add("stateWrite(): Couldn't write time to \"%s\"",
                        tmpStatePathname);
                }
                else {
                    if (rename(tmpStatePathname, statePathname) == -1) {
                        log_errno();
                        log_add("stateWrite(): "
                            "Couldn't rename \"%s\" to \"%s\"",
                            tmpStatePathname, statePathname);
                    }
                    else {
                        status = 0;
                    }
                }
            }

            (void)fclose(file);
        }                               /* output file open */
    }                                   /* module initialized */

    return status;
}
