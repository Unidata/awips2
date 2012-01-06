/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: fsStats.c,v 1.11.18.3 2005/06/24 21:05:18 steve Exp $ */

#include "ldmconfig.h"

#include "fsStats.h"

#if __ultrix
        /* ULTRIX 4.4 (MIPS), 4.3 (VAX), blatant kludge */
#include <sys/param.h>
#include <sys/mount.h>
typedef struct fs_data STRUCT_STATFS;
#if 1
#define FSTATFS(fd, sbp) (errno = ENOSYS, -1)
#else
#define FSTATFS(fd, sbp) (statfs(".", (sbp)))
#endif
#define f_blocks fd_btot
#define f_bavail fd_bfreen
#endif

#if __bsdi__ || __bsdi || __FreeBSD__ || (__APPLE__ && __MACH__)
        /* BSDI, ... */
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/mount.h>
typedef struct statfs STRUCT_STATFS;
#define FSTATFS(fd, sbp) (fstatfs((fd), (sbp)))
#define f_frsize f_bsize
#endif

#if defined(CRAY)
        /* UNICOS */
#include <sys/statfs.h>
typedef struct statfs STRUCT_STATFS;
#define FSTATFS(fd, sbp) (fstatfs((fd), (sbp), (sizeof(struct statfs)), (0)))
#define f_frsize f_bsize
#define f_bavail f_bfree
#endif

#if (__sun && !__SVR4)  || __hpux || __linux
        /* SunOS 4, HPUX, Linux */
#include <sys/vfs.h>
typedef struct statfs STRUCT_STATFS;
#define FSTATFS(fd, sbp) (fstatfs((fd), (sbp)))
#define f_frsize f_bsize
        /* On HP the f_bavail member seems to be wrong, and f_bfree
         * agrees with others f_bavail... */
#endif

#if _SYSTYPE_SVR4 || __SVR4 || _AIX || __osf__ || __sgi
        /* irix 5.3, SunOS 5, AIX, OSF1 */
#include <sys/statvfs.h>
typedef struct statvfs STRUCT_STATFS;
#define FSTATFS(fd, sbp) (fstatvfs((fd), (sbp)))
#endif


int
fsStats(int fd, off_t *fs_szp, off_t *remp)
{
        STRUCT_STATFS sbuf;
        off_t frsize; /* multiplier */

        if(FSTATFS(fd, &sbuf) == -1)
        {
                return errno;
        }
        /* else */
#if __ultrix
        frsize = 1024;
#else
        frsize = sbuf.f_frsize;
        if(frsize <= 0)
        {
                /* f_frsize not supported ? */
                frsize = sbuf.f_bsize;
        }
#endif

        if(fs_szp != 0)
                *fs_szp = frsize * sbuf.f_blocks;
        if(remp != 0)
                *remp = frsize * sbuf.f_bavail;

        return ENOERR;
}


#ifdef TEST_FSSTATS /* test driver */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>


static void
usage(const char *const av0)
{
        (void)fprintf(stderr, "Usage: %s filename\n",
                av0);
        exit(EXIT_FAILURE);
}


main(int ac, char *av[])
{
        int status = ENOERR;
        int fd;
        const char *const path = av[ac-1];
        off_t total;
        off_t avail;

        if(ac != 2)
                usage(av[0]);

        fd = open(path, O_RDONLY, 0);
        if(fd == -1)
        {
                status = errno;
                (void)fprintf(stderr, "open %s failed: %s\n",
                        path, strerror(status));
                exit(EXIT_FAILURE);
        }

        status = fsStats(fd, &total, &avail);
        if(status != ENOERR)
        {
                (void)fprintf(stderr, "fsStats %s failed: %s\n",
                        path, strerror(status));
                exit(EXIT_FAILURE);
        }

        printf("File system size: %10ld (%7ld k)\n",
                (long)total, (long)total/1024);
        printf(" space available: %10ld (%7ld k)\n",
                (long)avail, (long)avail/1024);

        exit(EXIT_SUCCESS);
}
#endif /* TEST_FSSTATS */
