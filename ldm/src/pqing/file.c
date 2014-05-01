/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: file.c,v 1.19.22.1 2004/12/09 19:36:24 steve Exp $ */

#include <ldmconfig.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include "rawfile.h"
#include "ulog.h"
#include "feed.h"

static unsigned long file_bytes = 0; /* total bytes read */


void
file_stats(void)
{
        unotice("  FILE Bytes read:    %8lu",
                file_bytes);
}


int
read_file(int ifd, char *buf, size_t nbytes, size_t *ngotp)
{
        const ssize_t nread = read(ifd, buf, nbytes);
        if(nread < 0)
                return errno;
        *ngotp = (size_t)nread;

        if (nread > 0)
        {
                file_bytes += (unsigned long) nread;
                write_rawfile(nread, buf);
        }

        return ENOERR;
}


int
file_open(const char *feedfname, int *const fdp)
{
        int fd = open(feedfname, O_RDONLY, 0);
        if(fd < 0)
        {
                const int status = errno;
                serror("Couldn't open file \"%s\"", feedfname);
                return status;
        }

        *fdp = fd;
        unotice("FILE \"%s\"", feedfname);
        return ENOERR;
}


int
file_close(int fd)
{
        if(close(fd) < 0)
                return errno;
        return ENOERR;
}
