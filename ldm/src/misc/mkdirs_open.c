/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: mkdirs_open.c,v 1.14.18.4 2006/01/23 22:12:26 steve Exp $ */

#include "ldmconfig.h"

#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h> /* O_RDONLY et al */
#include "mkdirs_open.h"

#include <limits.h> /* PATH_MAX */
#ifndef PATH_MAX
#define PATH_MAX 255
#endif /* !PATH_MAX */

#include <errno.h>
#ifndef ENAMETOOLONG
#define ENAMETOOLONG E2BIG /* punt */
#endif /* !ENAMETOOLONG */

#include <string.h>
#ifndef NULL
#define NULL 0
#endif
#include <unistd.h> /* access */

/*
 * Ensures that a directory exists.  Will create all necessary parent 
 * directories.
 *
 * Arguments:
 *      path    Pointer to directory pathname.
 *      mode    File creation mode for directory and any parent directories.
 * Returns:
 *      -1      Failure.  errno is set.
 *      0       Success.
 */
int
mkdirs(
    const char* const   path,
    const mode_t        mode)
{
    int         retCode = mkdir(path, mode);

    if (retCode != 0) {
        if (errno == EEXIST) {
            /*
             * The directory already exists (another process might just have
             * created it).  This is still a "success".
             */
            retCode = 0;
        }
        else if (errno == ENOENT) {
            /*
             * A necessary parent directory doesn't exist.
             *
             * Strip off the last parent directory in the path.
             */
            char*       ep = strrchr(path, '/');
            size_t      len = (size_t)(ep - path);
            char        stripped[PATH_MAX+1];

            assert(ep != NULL && ep != path);
            assert(len < PATH_MAX);

            /*
             * Copy up-to the last component in the path.
             */
            (void)memcpy(stripped, path, len);
            stripped[len] = 0;

            /*
             * Ensure that the parent directory exists.
             */
            retCode = mkdirs(stripped, mode);

            if (retCode == 0)
                retCode = mkdir(path, mode);
        }                               /* a parent directory doesn't exist */
    }                                   /* couldn't create directory */

    return retCode;
}


/*
 * Like open(2), but will create components as necessary.
 * Returns valid file descriptor if successful, -1 on failure.
 */
int
mkdirs_open(
        const char *path,
        int flags,
        mode_t mode)
{
        int fd;
        fd = open(path, flags, mode);
        if(fd != -1)            /* open suceeded */
                return fd;
        /* else */
        if(errno != ENOENT)     /* not a problem we will fix here */
                return -1;
        /* else */
        if(!((unsigned int)flags & O_CREAT))
                return -1;

        /* else, a component of the path prefix does not exist */
        {
        char *ep;
        size_t len;
        char stripped[PATH_MAX+1];

        ep = strrchr(path, '/');
        if(ep == NULL || ep == path)
                return -1; /* can't happen ? */
        /* else */

        /* strip off last component in path */
        len = (size_t)(ep - path);
        if(len >= PATH_MAX)
        {
                errno = ENAMETOOLONG;
                return -1;
        }
        (void)memcpy(stripped, path, len);
        stripped[len] = 0;

        if(mkdirs(stripped, (mode | 0111)) == -1)
                return -1;
        /* else */
        }
        return open(path, flags, mode);
}


#if TEST_FIXTURE
main(ac,av)
int ac;
char *av[];
{
        int fd;

        if(ac < 2) exit(1);

        fd = mkdirs_open(av[1] , O_WRONLY | O_CREAT | O_TRUNC, 0664);
        if(fd == -1)
        {
                perror(av[1]);
                exit(2);
        }

        (void) close(fd);
        exit(0);
}
#endif


/*
 * Check to see if we have access to all components of 'path'
 * up to the last component. (Doesn't check the access of the full path)
 * If 'create' is no zero, attempt to create path components (directories)
 * as necessary.
 * Returns 0 if access is ok, -1 on error.
 */
int
diraccess(
        const char *path,
        int access_m,
        int create)
{
        char *ep;
        size_t len;
        char stripped[PATH_MAX+1];

        ep = strrchr(path, '/');
        if(ep == NULL || ep == path)
                return (0);

        /* else */
        /* strip off last component in path */
        len = (size_t)(ep - path);
        if(len >= PATH_MAX)
        {
                errno = ENAMETOOLONG;
                return -1;
        }
        (void)memcpy(stripped, path, len);
        stripped[len] = 0;

        /* Now we have something that looks like a directory */
        if(access(stripped, (unsigned int)access_m | X_OK) == 0)
                return(0);
        /* else */

        if(create && errno == ENOENT)
        {
                if(mkdirs(stripped, 0777) == 0) /* let the umask fix it */
                        return 0;
        }
        /* else */

        return -1;
}
