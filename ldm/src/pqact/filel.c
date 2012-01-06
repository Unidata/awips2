/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: filel.c,v 1.177.10.6.2.18 2008/09/17 16:35:18 steve Exp $ */

#include <ldmconfig.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <limits.h> /* PATH_MAX */
#include <inttypes.h>
#ifndef PATH_MAX
#define PATH_MAX 255                    /* _POSIX_PATH_MAX */
#endif /* !PATH_MAX */
#include <sys/types.h>
#include <sys/sem.h>
#include <sys/stat.h>
#include <fcntl.h> /* O_RDONLY et al */
#include <unistd.h> /* access, lseek */
#include <signal.h>
#include <errno.h>

#if defined(_AIX) && !defined(NO_WAITPID)
/*
 * Use POSIX wait macros, not _BSD
 */
#define _H_M_WAIT
#endif
#include <sys/wait.h>


#include "error.h"
#include "filel.h"
#include "action.h"
#include "ldm.h"
#include "ldmalloc.h"
#include "ldmprint.h"
#include "mkdirs_open.h"
#include "ulog.h"
#include "pbuf.h"
#include "pq.h"

extern pqueue*  pq;

static unsigned maxEntries = 0;
static int shared_id = -1;
static int sem_id = -1;
static unsigned shared_size;
static unsigned queue_counter = 0;
static unsigned largest_queue_element = 0;
static union semun semarg;
/*
 * Defined in pqcat.c
 */
extern int pipe_timeo;

#ifndef NO_DB

/*
 * Define DB_XPROD non zero if you want the whole "product" data
 * structure put into the database, otherwise just the data goes in.
 * Be sure this is consistant with ../dbcat and whatever else used
 * to read the files.
 */
# ifndef DB_XPROD
# define DB_XPROD 1
# endif

/*
 * Backward compatibility.
 * If you want to use gdbm interfaces, define USE_GDBM
 */
# ifdef USE_GDBM
# include "gdbm.h"
# else
# include <ndbm.h>
# endif

#endif /* !NO_DB */


/*
 * 
 */
typedef enum {
        FT_NONE = 0,
        UNIXIO,
        STDIO,
        PIPE,
        FT_DB
} ft_t ;


union f_handle {
        int fd;
        FILE *stream;
        pbuf *pbuf;
#ifndef NO_DB
# ifdef USE_GDBM
        GDBM_FILE db;
# else
        DBM *db;
# endif
#endif /*!NO_DB*/
};
typedef union f_handle f_handle;


struct fl_entry {
    struct fl_entry*    next;
    struct fl_entry*    prev;
    struct fl_ops*      ops;
    f_handle            handle;
    unsigned long       private;        /* pid, hstat*, R/W flg */
    int                 flags;
    ft_t                type;
    char                path[PATH_MAX]; /* PATH_MAX includes NUL */
};
typedef struct fl_entry fl_entry;

#if defined(__cplusplus) || defined(__STDC__)
struct fl_ops {
        int (*cmp)(fl_entry *, int, char**);
        int (*open)(fl_entry *, int, char**);
        void (*close)(fl_entry *);
        int (*sync)(fl_entry *, int);
        int (*put)(fl_entry *, const char *,
                const void *, size_t );
};
#else /* Old Style C */
struct fl_ops {
        int (*cmp)();
        int (*open)();
        void (*close)();
        int (*sync)();
        int (*dbufput)();
};
#endif


/*
 * the one global list of of open files
 */
static struct fl {
        int size;
        fl_entry *head;
        fl_entry *tail;
} thefl[] = {
        0 ,
        NULL ,
        NULL
};


#define TO_HEAD(entry) \
        if(thefl->head != entry) to_head(entry)

static void
to_head(fl_entry *entry)
{
        if(thefl->head == entry)
                return;

        if(entry->prev != NULL)
                entry->prev->next = entry->next;
        if(entry->next != NULL)
                entry->next->prev = entry->prev;

        if(thefl->head != NULL)
                thefl->head->prev = entry;
        if(thefl->tail == entry)
                thefl->tail = entry->prev;

        entry->next = thefl->head;
        entry->prev = NULL;
        thefl->head = entry;
        if(thefl->tail == NULL)
                thefl->tail = entry;
}


static void
free_fl_entry(fl_entry *entry)
{
        if(entry == NULL) return;

        if(entry->ops != NULL)
        {
                entry->ops->close(entry);
        }
        free(entry);
}

/* forward reference */
static fl_entry * new_fl_entry(ft_t type, int argc, char **argv);

#ifdef FL_DEBUG
static void
dump_fl(void)
{
        fl_entry *entry;
        int fd;

        udebug("      thefl->size %d", thefl->size);
        for(entry = thefl->head; entry != NULL; 
                        entry = entry->next )
        {
                switch (entry->type) {
                case UNIXIO :
                        fd = entry->handle.fd;
                        break;
                case STDIO :
                        fd = entry->handle.stream == NULL
                                ? -1 : fileno(entry->handle.stream);    
                        break;
                case PIPE :
                        fd = entry->handle.pbuf == NULL
                                ? -1 : entry->handle.pbuf->pfd;
                        break;
                case FT_DB :
#ifndef NO_DB
                        fd = entry->handle.db == NULL
                                ? -1 : -2;
                        break;
#endif /* !NO_DB */
                default :
                        fd = -2;
                }
                udebug("       %d %s", fd, entry->path);
        }
}
#endif

static fl_entry *
lookup_fl_entry(ft_t type, int argc, char **argv)
{
        fl_entry *entry = NULL;

        for(entry = thefl->head; entry != NULL; 
                        entry = entry->next )
        {
                if(entry->type == type &&
                                entry->ops->cmp(entry, argc, argv) == 0)
                        break;
        }
        return entry;
}


static void
delete_entry(fl_entry *entry)
{
        /* assert(thefl->size >= 1); */
        if(entry == NULL) return;

        if(entry->prev != NULL)
                entry->prev->next = entry->next;
        if(entry->next != NULL)
                entry->next->prev = entry->prev;
        if(thefl->head == entry)
                thefl->head = entry->next;
        if(thefl->tail == entry)
                thefl->tail = entry->prev;
        thefl->size--;

        free_fl_entry(entry);
}


/*
 * sync up to nentries entries, tail to head.
 */
void
fl_sync(int nentries,
         int block) /* bool_t, FALSE => nonblocking */
{
        fl_entry *entry, *prev;

/*      udebug("  fl_sync"); */

        if(thefl->size <= 0)
                return;
        if(nentries == -1)      /* sync everyone */
                nentries = thefl->size;
                
        for(entry = thefl->tail;
                entry != NULL && nentries >= 0; entry = prev, nentries--)
        {
                prev = entry->prev;
                if(entry->flags & FL_NEEDS_SYNC)
                {
                        if(entry->ops->sync(entry, block) == -1)
                                delete_entry(entry);
                }
        }
}


/*
 * close the "least recently used" entry
 */
void
close_lru(int skipflags)
{
        fl_entry *entry, *prev;

        if(thefl->size <= 0)
                return;
        entry = thefl->tail;    


        for(entry = thefl->tail;
                entry != NULL; entry = prev)
        {
                prev = entry->prev;
                /* twisted logic */
                if(entry->flags & skipflags)
                        continue;
                /* else */
        /*      udebug("   close_lru: %s", entry->path); */
                delete_entry(entry);
                return;
        }
}


void
fl_close_all(void)
{
        while(thefl->size > 0)
        {
                close_lru(0);
        }
}


/*
 * Look for an fl_entry in the list.
 * If there isn't one there that matches what you need, make a new one.
 */
static fl_entry *
get_fl_entry(ft_t type, int argc, char **argv)
{
        fl_entry *entry;

        entry = lookup_fl_entry(type, argc, argv);
        if( entry != NULL )
        {
                TO_HEAD(entry);
#ifdef FL_DEBUG
                dump_fl();
#endif
                return entry;
        }
        /* else */

        assert(maxEntries > 0);

        if(thefl->size >= maxEntries)
                close_lru(0);

        entry = new_fl_entry(type, argc, argv);
        if( entry == NULL )
        {
                return NULL; /* malloc or open failed */
        }

        /* to front */
        if(thefl->head != NULL)
                thefl->head->prev = entry;
        entry->next = thefl->head;
        entry->prev = NULL;
        thefl->head = entry;
        if(thefl->tail == NULL)
                thefl->tail = entry;
        thefl->size++;

#ifdef FL_DEBUG
        dump_fl();
#endif
        return entry;
}


/*
 * Ensures that a given file descriptor will be closed upon execution of an
 * exec(2) family function.
 *
 * Arguments:
 *      fd      The file descriptor to be set to close-on-exec.
 * Returns:
 *      NULL    Success.
 *      else    Error object.
 */
static ErrorObj*
ensureCloseOnExec(
    const int   fd)
{
    ErrorObj*    errObj = NULL;          /* success */
    int         flags = fcntl(fd, F_GETFD);

    if (-1 == flags) {
        errObj = ERR_NEW2(errno, NULL,
            "Couldn't get flags for file descriptor %d: %s",
            fd, strerror(errno));
    }
    else if (!(flags & FD_CLOEXEC) &&
            (-1 == fcntl(fd, F_SETFD, flags | FD_CLOEXEC)))
    {
        errObj = ERR_NEW2(errno, NULL,
            "Couldn't set file descriptor %d to close-on-exec(): %s",
            fd, strerror(errno));
    }

    return errObj;
}


static int
atFinishedArgs(int ac,
        char *av[],
        fl_entry *entry)
{
        int status = 0;
        int syncflag = 0;
        int closeflag = 0;
        for(; ac > 1 && *av[0] == '-'; ac-- , av++)
        {
                if( strncmp(*av,"-close",3) == 0)
                {
                        closeflag = 1;
                }
                else if( strncmp(*av,"-flush",3) == 0)
                {
                        syncflag = 1;
                }
        }
        if(syncflag)
                status = (*entry->ops->sync)(entry, syncflag);
        if(closeflag)
                delete_entry(entry);
        return status;
}


/*
 * Given a dbuf, return a copy with the non '\n'
 * control characters removed.
 * Remember to free the result.
 */
static void *
dupstrip(const void *in, size_t len, size_t *outlenp)
{
        void *out;
        size_t blen;
        const unsigned char *ip;
        unsigned char *op;

        if(in == NULL || len == 0)
                return NULL;

        out = malloc(len);
        if(out == NULL)
        {
                serror("dupstrip: malloc %ld failed", (long) len);
                return NULL;
        }
        
        for(blen = len, ip = in, op = out, *outlenp = 0;
                blen != 0; blen--, ip++)
        {
                if(((int)*ip) > 127
                                || (iscntrl(*ip) && *ip != '\n'))
                        continue;
                /* else */
                *op++ = *ip;
                (*outlenp)++;
        }

        return out;
}


/* Begin UNIXIO */
static int
str_cmp( fl_entry *entry, int argc, char **argv)
{
        char *path;

        assert(argc > 0);
        assert(argv[argc -1] != NULL);
        assert(*argv[argc -1] != 0);

        path = argv[argc-1];
        return(strcmp(path, entry->path));
}


/*
 * Opens an output-file for the FILE action.
 *
 * Arguments:
 *      entry   Pointer to the relevant entry in the pattern/action list.
 *      ac      The number of argument.
 *      av      Pointer to pointers to arguments.
 * Returns:
 *      -1      Failure.  An error-message is logged.
 *      else    The file descriptor for the output-file.
 */
static int
unio_open(fl_entry *entry, int ac, char **av)
{
    char*       path;
    int         flags = (O_WRONLY|O_CREAT);
    int         writeFd = -1;           /* failure */

    assert(ac > 0);
    assert(av[ac -1] != NULL);
    assert(*av[ac -1] != 0);

    for (; ac > 1 && *av[0] == '-'; ac-- , av++) {
        if (0 == strncmp(*av,"-overwrite",3)) {
            entry->flags |= FL_OVERWRITE;
            flags |= O_TRUNC;
        }
        else if (0 == strncmp(*av,"-strip",3)) {
            entry->flags |= FL_STRIP;
        }
        else if (0 == strncmp(*av,"-log",4)) {
            entry->flags |= FL_LOG;
        }
        else if (0 == strncmp(*av,"-edex",3)) {
            entry->flags |= FL_EDEX;
        }
    }

    path = av[ac-1];
    entry->handle.fd = -1;

    writeFd = mkdirs_open(path, flags, 0666);

    if (-1 == writeFd) {
        if(errno == EMFILE || errno == ENFILE) {
            /*
             * Too many open files.
             */
            close_lru(0);
            close_lru(0);
        }

        serror("unio_open: %s", path);
    }
    else {
        int     error = 0;
        /*
         * Ensure that the file descriptor will close upon execution 
         * of an exec(2) family function because no child processes should
         * inherit it.
         */
        ErrorObj*        errObj = ensureCloseOnExec(writeFd);

        if (errObj) {
            err_log_and_free(
                ERR_NEW(0, errObj, "Couldn't open FILE output-file"),
                ERR_FAILURE);

            error = 1;
        }
        else {
            if (!(flags & O_TRUNC)) {
                if (lseek(writeFd, 0, SEEK_END) < 0) {
                    /*
                     * The "file" must be a pipe or FIFO.
                     */
                    serror("unio_open(): Couldn't seek to EOF: %s", path);
                }
            }

            entry->handle.fd = writeFd;
            strncpy(entry->path, path, PATH_MAX);
            entry->path[PATH_MAX-1] = 0; /* just in case */

            udebug("    unio_open: %d %s", entry->handle.fd, entry->path);
        }                               /* output-file set to close_on_exec */

        if (error) {
            (void)close(writeFd);
            writeFd = -1;
        }
    }                                   /* "writeFd" open */

    return writeFd;
}


static void
unio_close(fl_entry *entry)
{
        udebug("    unio_close: %d", entry->handle.fd); 
        if(entry->handle.fd != -1)
        {
                if(close(entry->handle.fd) == -1) 
                {
                        serror("close: %s", entry->path);
                }
        }
        entry->path[0] = 0;
        entry->handle.fd = -1;
}


static int
unio_sync(fl_entry *entry, int block)
{
        /*
         * Some systems may not have an fsync(2) call.
         * The best you can do then would be to make this
         * routine a noop which returns 0.
         */
        int status  = 0;
        udebug("    unio_sync: %d %s",
                entry->handle.fd, block ? "" : "non-block"); 
        if(block)
        {
#ifndef NO_FSYNC
                if(entry->handle.fd != -1)
                        status = fsync(entry->handle.fd); 
                if(status == -1)
                {
                        serror("fsync: %s", entry->path);
                }
#endif
                entry->flags &= ~FL_NEEDS_SYNC;
        }
        return status;
}


/*ARGSUSED*/
static int
unio_put(fl_entry *entry, const char *ignored, 
                const void *data, size_t sz)
{
    int         errCode = 0;            /* success */

    if (0 < sz) {
        TO_HEAD(entry);
        udebug("    unio_dbufput: %d", entry->handle.fd);

        do {
            ssize_t     nwrote = write(entry->handle.fd, data, sz);

            if (-1 != nwrote) {
                sz -= nwrote;
                data = (char*)data + nwrote;
            }
            else {
                if (EINTR != errno) {
                    /*
                     * According to the UNIX standard, errno should not be set
                     * to EINTR because the SA_RESTART option was specified to
                     * sigaction(3) for most signals that could occur.  The
                     * OSF/1 operating system is non-conforming in this regard,
                     * however.  For a discussion of the SA_RESTART option, see
                     * http://www.opengroup.org/onlinepubs/007908799/xsh/sigaction.html
                     */
                    serror("unio_put(): write() error: %s", entry->path);
                    errCode = -1;
                    break;
                }
            }
        } while (0 < sz);

        if (0 == errCode) {
            entry->flags |= FL_NEEDS_SYNC;
        }
        else {
            /*
             * Don't waste time syncing an errored entry.
             */
            entry->flags &= ~FL_NEEDS_SYNC;
            delete_entry(entry);
        }
    }

    return errCode;
}


static struct fl_ops unio_ops = {
        str_cmp,
        unio_open,
        unio_close,
        unio_sync,
        unio_put,
};


/*ARGSUSED*/
int
unio_prodput(
    const product*      prodp,
    int                 argc,
    char**              argv,
    const void*         ignored,
    size_t              also_ignored)
{
    int         status = -1;            /* failure */
    fl_entry*   entry = get_fl_entry(UNIXIO, argc, argv);

    udebug("    unio_prodput: %d",
            entry == NULL
                ? -1
                : entry->handle.fd);


    if (entry != NULL) {
        if(entry->flags & FL_EDEX) {
            if(shared_id == -1) {
                uerror("Notification specified but shared memory is not available.");
            }
            else {
                edex_message * queue = (edex_message *)shmat(shared_id, (void *)0, 0);
                strncpy(queue[queue_counter].filename, entry->path, 4096);
                strncpy(queue[queue_counter].ident, prodp->info.ident, 256);
                if(shmdt(queue) == -1) {
                    uerror("Detaching shared memory failed.");
                }
            }
        }
        size_t  sz = prodp->info.sz;
        void*   data = 
            (entry->flags & FL_STRIP)
                ? dupstrip(prodp->data, prodp->info.sz, &sz)
                : prodp->data;

        if (data != NULL) {
            status = 0;                 /* success */

            if (entry->flags & FL_OVERWRITE) {
                if (lseek(entry->handle.fd, 0, SEEK_SET) < 0) {
                    /*
                     * The "file" must be a pipe or FIFO.
                     */
                    serror("unio_prodput(): Couldn't seek to BOF: %s",
                        entry->path);
                }
            }

            status = unio_put(entry, prodp->info.ident, data, sz);
            if (status == 0) {
                if (entry->flags & FL_OVERWRITE)
                    (void)ftruncate(entry->handle.fd, sz);

                status = atFinishedArgs(argc, argv, entry);

                if (status == 0) {
                    if(entry->flags & FL_LOG)
                        unotice("Filed in \"%s\": %s",
                            argv[argc-1],
                            s_prod_info(NULL, 0, &prodp->info, ulogIsDebug()));
                    if(entry->flags & FL_EDEX && shared_id != -1) {
                        semarg.val = queue_counter;
                        int semreturn = semctl(sem_id, 1, SETVAL, semarg);
                        queue_counter = (queue_counter == largest_queue_element) ? queue_counter = 0 : queue_counter + 1;
                    }
                }
            }                       /* data written */

            if (data != prodp->data)
                free(data);
        }                               /* data != NULL */
    }                                   /* entry != NULL */
    udebug("    unio_prodput: complete for %s at location %s", prodp->info.ident, entry->path);
    return status;
}


/* End UNIXIO */

/* Begin STDIO */
/*
 * Opens an output-file for the STDIOFILE action.
 *
 * Arguments:
 *      entry   Pointer to the relevant entry in the pattern/action list.
 *      ac      Number of arguments.
 *      av      Pointer to pointers to arguments.
 * Returns:
 *      -1      Failure.  An error-message is logged.
 *      else    File descriptor for the output-file.
 */
static int
stdio_open(fl_entry *entry, int ac, char **av)
{
    char*       path;
    int         flags = (O_WRONLY|O_CREAT);
    int         fd;
    char*       mode = "a";

    assert(ac > 0);
    assert(av[ac -1] != NULL);
    assert(*av[ac -1] != 0);

    entry->handle.stream = NULL;

    for (; ac > 1 && *av[0] == '-'; ac-- , av++) {
        if( strncmp(*av,"-overwrite",3) == 0) {
            entry->flags |= FL_OVERWRITE;
            flags |= O_TRUNC;
            mode = "w";
        }
        else if( strncmp(*av,"-strip",3) == 0) {
            entry->flags |= FL_STRIP;
        }
        else if (0 == strncmp(*av,"-log",4)) {
            entry->flags |= FL_LOG;
        }
    }

    path = av[ac-1];

    fd = mkdirs_open(path, flags, 0666);

    if (-1 == fd) {
        if (errno == EMFILE || errno == ENFILE) {
            /*
             * Too many open files.
             */
            close_lru(0);
            close_lru(0);
        }

        serror("mkdirs_open: %s", path);
    }
    else {
        int     error = 1;
        /*
         * Ensure that the file descriptor will close upon execution of an
         * exec(2) family function because no child processes should inherit it.
         */
        ErrorObj*        errObj = ensureCloseOnExec(fd);

        if (errObj) {
            err_log_and_free(
                ERR_NEW(0, errObj, "Couldn't open STDIOFILE output-file"),
                ERR_FAILURE);
        }
        else {
            entry->handle.stream = fdopen(fd, mode);

            if (NULL == entry->handle.stream) {
                serror("fdopen: %s", path);
            }
            else {
                if (!(flags & O_TRUNC)) {
                    if (fseek(entry->handle.stream, 0, SEEK_END) < 0) {
                        /*
                         * The "file" must be a pipe or FIFO.
                         */
                        serror("stdio_open(): Couldn't seek to EOF: %s",
                            entry->path);
                    }
                }

                strncpy(entry->path, path, PATH_MAX);
                entry->path[PATH_MAX-1] = 0; /* just in case */
                udebug("    stdio_open: %d", fileno(entry->handle.stream));
                error = 0;
            }                           /* entry->handle.stream allocated */
        }                               /* output-file set to close-on-exec */

        if (error) {
            (void)close(fd);
            fd = -1;
        }
    }                                   /* "fd" open */

    return fd;
}


static void
stdio_close(fl_entry *entry)
{
        udebug("    stdio_close: %d",
                entry->handle.stream ? fileno(entry->handle.stream) : -1);
        if(entry->handle.stream != NULL)
        {
                if(fclose(entry->handle.stream) == EOF) 
                {
                        serror("fclose: %s", entry->path);
                }
        }
        entry->path[0] = 0;
        entry->handle.stream = NULL;
}


/*ARGSUSED*/
static int
stdio_sync(fl_entry *entry, int block)
{
        int status  = 0;
        udebug("    stdio_sync: %d",
                entry->handle.stream ? fileno(entry->handle.stream) : -1);
        if(fflush(entry->handle.stream) == EOF) 
        {
                serror("fflush: %s", entry->path);
                status = -1;
        }
        entry->flags &= ~FL_NEEDS_SYNC;
        return status;
}


/*ARGSUSED*/
static int
stdio_put(fl_entry *entry, const char *ignored,
                const void *data, size_t sz)
{
        size_t nwrote;

        TO_HEAD(entry);
        udebug("    stdio_dbufput: %d", fileno(entry->handle.stream));

        /* else */
        nwrote = fwrite(data, 1, sz, entry->handle.stream);
        if(nwrote != sz)
        {
                if (errno != EINTR)
                    serror("stdio_put(): fwrite() error: %s", entry->path);

                /* don't waste time syncing an errored entry */
                entry->flags &= ~FL_NEEDS_SYNC;
                delete_entry(entry);
                return -1;
        }
        /* else */
        entry->flags |= FL_NEEDS_SYNC;
        return 0;
}


static struct fl_ops stdio_ops = {
        str_cmp,
        stdio_open,
        stdio_close,
        stdio_sync,
        stdio_put,
};


/*ARGSUSED*/
int
stdio_prodput(
    const product*      prodp,
    int                 argc,
    char**              argv,
    const void*         ignored,
    size_t              also_ignored)
{
    int         status = -1;            /* failure */
    fl_entry*   entry = get_fl_entry(STDIO, argc, argv);

    udebug("    stdio_prodput: %d %s",
            entry == NULL
                ? -1
                : fileno(entry->handle.stream) , prodp->info.ident);

    if (entry != NULL) {
        size_t  sz = prodp->info.sz;
        void*   data =
            (entry->flags & FL_STRIP)
                ? dupstrip(prodp->data, prodp->info.sz, &sz)
                : prodp->data;

        if (data != NULL) {
            if (entry->flags & FL_OVERWRITE) {
                if (fseek(entry->handle.stream, 0, SEEK_SET) < 0) {
                    /*
                     * The "file" must be a pipe or FIFO.
                     */
                    serror("stdio_prodput(): Couldn't seek to BOF: %s",
                        entry->path);
                }
            }

            status = stdio_put(entry, prodp->info.ident, data, sz);

            if (status == 0) {
                if (entry->flags & FL_OVERWRITE)
                    (void)ftruncate(fileno(entry->handle.stream), sz);

                status = atFinishedArgs(argc, argv, entry);

                if ((status == 0) && (entry->flags & FL_LOG))
                    unotice("StdioFiled in \"%s\": %s",
                        argv[argc-1],
                        s_prod_info(NULL, 0, &prodp->info, ulogIsDebug()));
            }                       /* data written */

            if (data != prodp->data)
                free(data);
        }                               /* data != NULL */
    }                                   /* entry != NULL */

    return status;
}

/* End STDIO */


/* Begin PIPE */

/*
 * Concatenates arguments into one, long, NUL-terminated string.
 *
 * Arguments:
 *      buf     Point to buffer into which to concatenate arguments.
 *      len     Maximum number of characters that can be put into buffer
 *              excluding terminating NUL.
 *      argc    Number of arguments.
 *      argv    Pointer to "argc" pointers to the arguments.
 * Returns:
 *      Number of characters placed in "buf" excluding terminating NUL.
 */
static int
argcat(char *buf, int len, int argc, char **argv)
{
        int cnt = 0;
        char *cp;

        while(argc-- > 0 && (cp = *argv++) != NULL)
        {
                while(*cp != 0)
                {
                        buf[cnt++] = *cp++;
                        if(cnt >= len)
                                break;
                }
        }
        buf[cnt] = 0;
        return cnt;
}


static int
argcat_cmp(fl_entry *entry, int argc, char **argv)
{
        char buf[PATH_MAX];

        assert(argc > 0);
        assert(argv[0] != NULL);
        assert(*argv[0] != 0);

        argcat(buf, sizeof(buf)-1, argc, argv);
        return(strcmp(buf, entry->path));
}


/*
 * Set to non-root privilege if possible.
 * Do it in such a way that it is safe to fork.
 * TODO: this is duplicated from ../server/priv.c
 */
void
endpriv(void)
{
        const uid_t euid = geteuid();
        const uid_t uid = getuid();

        /* if either euid or uid is unprivileged, use it */
        if(euid > 0)
                setuid(euid);
        else if(uid > 0)
                setuid(uid);

        /* else warn??? or set to nobody??? */
}


/* 
 * Open a pipe to a child decoder process.
 *
 * Arguments:
 *      entry   Pointer to the entry in the pattern/action list.
 *      argc    Number of arguments of the decoder invocation command.
 *      argv    Pointer to pointers to arguments of the decoder invocation
 *              command.
 * Returns:
 *      -1      Failure.  An error-message is logged.
 *      else    File descriptor for the write-end of the pipe.
 */
static int
pipe_open(fl_entry *entry, int argc, char **argv)
{
    int         ac = argc;
    char**      av = argv;
    int         pfd[2];
    int         writeFd = -1;           /* failure */

    assert(argc >= 1);
    assert(argv[0] != NULL && *argv[0] != 0);
    assert(argv[argc] == NULL);

    entry->handle.pbuf = NULL;
    entry->flags |= FL_NOTRANSIENT;

    /*
     * Handle command-line options.
     */
    for (; ac > 1 && *av[0] == '-'; ac-- , av++)
    {
        if (strncmp(*av, "-transient", 3) == 0)
        {
            entry->flags &= ~FL_NOTRANSIENT;
        }
        else if (strncmp(*av, "-strip", 3) == 0)
        {
            entry->flags |= FL_STRIP;
        }
        else if (strncmp(*av, "-metadata", 3) == 0)
        {
            entry->flags |= FL_METADATA;
        }
        else if (strncmp(*av, "-nodata", 3) == 0)
        {
            entry->flags |= FL_NODATA;
            entry->flags |= FL_METADATA;
        }
    }

    /*
     * Create a pipe into which the parent pqact(1) process will write one or
     * more data-products and from which the child decoder process will read.
     */
    if (-1 == pipe(pfd))
    {
        if (errno == EMFILE || errno == ENFILE)
        {
            /*
             * Too many open files.
             */
            close_lru(0);
            close_lru(0);
        }

        err_log_and_free(
            ERR_NEW1(0, NULL, "Couldn't create pipe: %s", strerror(errno)),
            ERR_FAILURE);
    }
    else
    {
        ErrorObj*        errObj;

        /*
         * Ensure that the write-end of the pipe will close upon execution 
         * of an exec(2) family function because no child processes should
         * inherit it.
         */
        if (errObj = ensureCloseOnExec(pfd[1]))
        {
            err_log_and_free(
                ERR_NEW(0, errObj,
                    "Couldn't set write-end of pipe to close on exec()"),
                ERR_FAILURE);
        }
        else
        {
            pid_t       pid = fork();

            if (-1 == pid)
            {
                err_log_and_free(
                    ERR_NEW1(0, NULL,
                        "Couldn't fork() child process: %s",
                        strerror(errno)),
                    ERR_FAILURE);
            }
            else
            {
                if (0 == pid)
                {
                    /*
                     * Child process.
                     */

                    (void)signal(SIGTERM, SIG_DFL);
                    (void)pq_close(pq);
                    pq = NULL;

                    /*
                     * This process is made its own process-group leader to
                     * isolate it from signals sent to the LDM process-group
                     * (e.g., SIGCONTs, SIGINTs, and SIGTERMs).
                     */
                    if (setpgid(0, 0) == -1) {
                        log_errno();
                        log_add(
                            "Couldn't make decoder a process-group leader.");
                        log_log(LOG_WARNING);
                    }

                    /*
                     * It is assumed that the standard output and error streams
                     * are correctly established and should not be modified.
                     */

                    /*
                     * Associate the standard input stream with the
                     * read-end of the pipe.
                     */
                    if (STDIN_FILENO != pfd[0])
                    {
                        if (-1 == dup2(pfd[0], STDIN_FILENO))
                        {
                            err_log_and_free(
                                ERR_NEW3(0, NULL,
                                    "Couldn't dup2(%d,%d): %s",
                                    pfd[0], STDIN_FILENO, strerror(errno)),
                                ERR_FAILURE);
                        }
                        else
                        {
                            (void)close(pfd[0]);

                            pfd[0] = STDIN_FILENO;
                        }
                    }

                    if (STDIN_FILENO == pfd[0])
                    {
                        endpriv();
                        (void)execvp(av[0], &av[0]);
                        err_log_and_free(
                            ERR_NEW2(0, NULL, "Couldn't exec(%s): %s",
                                av[0], strerror(errno)),
                            ERR_FAILURE);
                    }

                    exit(EXIT_FAILURE);
                }                       /* child process */
                else
                {
                    /*
                     * Parent process.
                     *
                     * Close the read-end of the pipe because it won't be used.
                     */
                    (void)close(pfd[0]);

                    /*
                     * Create a pipe-buffer with pfd[1] as the output file
                     * descriptor.
                     */
#ifdef PIPE_BUF
                    entry->handle.pbuf = new_pbuf(pfd[1], PIPE_BUF);
#else
                    entry->handle.pbuf = new_pbuf(pfd[1], _POSIX_PIPE_BUF);
#endif
                    if (NULL == entry->handle.pbuf) 
                    {
                        err_log_and_free(
                            ERR_NEW1(0, NULL,
                                "Couldn't create pipe-buffer: %s",
                                strerror(errno)),
                            ERR_FAILURE);
                    }
                    else
                    {
                        entry->private = pid;
                        writeFd = pfd[1];       /* success */

                        argcat(entry->path, PATH_MAX-1, argc, argv);
                        udebug("    pipe_open: %d %d", writeFd, pid);
                    }
                }                       /* parent process */
            }                           /* fork() success */
        }                               /* write-end of pipe is FD_CLOEXEC */

        if (-1 == writeFd)
        {
            (void) close(pfd[1]);
            (void) close(pfd[0]);
        }
    }                                   /* pipe() success */

    return writeFd;
}


/*
 * Returns the file-list PIPE-entry associated with a PID.
 *
 * Arguments:
 *      pid             The PID of the PIPE-entry to return.
 * Returns:
 *      NULL            The file-list doesn't contain a PIPE-entry with the
 *                      given PID.
 *      else            A pointer to the associated file-list PIPE-entry.
 */
static fl_entry*
fl_findByPid(
    pid_t       pid)
{
    fl_entry*   entry = NULL;           /* entry not found */

    for (entry = thefl->tail; entry != NULL; entry = entry->prev)
    {
        if (entry->type == PIPE && pid == entry->private)
            break;
    }

    return entry;
}


/*
 * Waits-upon one or more child processes.
 *
 * Arguments:
 *      pid             The PID of the process upon which to wait.  If 
 *                      (pid_t)-1, then any child process is waited-upon.
 *      options         Bitwise or of WCONTINUED, WNOHANG, or WUNTRACED.
 *                      See waitpid().
 * Returns:
 *      -1              Failure.  "errno" is set.
 *      0               "options" & WNOHANG is true and status isn't available
 *                      for process "pid".
 *      else            PID of the waited-upon process.
 */
pid_t
reap(
    const pid_t pid,
    const int   options)
{
    int         status = 0;
    const pid_t wpid = waitpid(pid, &status, options);

    if (wpid == -1)
    {
        if (!(errno == ECHILD && pid == -1))
        {
            /*
             * Unwaited-for child processes exist.
             */
            serror("waitpid()");
        }
    }
    else if (wpid != 0) 
    {
        fl_entry* const         entry = fl_findByPid(wpid);
        const char* const       cmd = entry ? entry->path : NULL;

        if (WIFSTOPPED(status))
        {
            unotice(
                cmd
                    ? "child %d stopped by signal %d (%s)"
                    : "child %d stopped by signal %d",
                wpid, WSTOPSIG(status), cmd);
        }
        else if (WIFSIGNALED(status))
        {
            unotice(
                cmd
                    ? "child %d terminated by signal %d (%s)"
                    : "child %d terminated by signal %d",
                wpid, WTERMSIG(status), cmd);
            delete_entry(entry);        /* NULL safe */
        }
        else if (WIFEXITED(status))
        {
            if (WEXITSTATUS(status) != 0)
                unotice(
                    cmd
                        ? "child %d exited with status %d (%s)"
                        : "child %d exited with status %d",
                    wpid, WEXITSTATUS(status), cmd);
            delete_entry(entry);        /* NULL safe */
        }
    }                                   /* wpid != -1 && wpid != 0 */

    return wpid;
}


static int
pipe_sync(fl_entry *entry, int block)
{
        int status = ENOERR;
        udebug("    pipe_sync: %d %s",
                entry->handle.pbuf ? entry->handle.pbuf->pfd : -1,
                block ? "" : "non-block"); 
        status = pbuf_flush(entry->handle.pbuf, block, pipe_timeo, entry->path);
        if(status != ENOERR && status != EINTR)
                entry->flags &= ~FL_NEEDS_SYNC;
        return status;
}


static void
pipe_close(fl_entry *entry)
{
        pid_t pid = (pid_t)entry->private;
        int pfd = -1;

        udebug("    pipe_close: %d, %d",
                entry->handle.pbuf ? entry->handle.pbuf->pfd : -1, pid);
        if(entry->handle.pbuf != NULL)
        {
                if(pid >= 0 && (entry->flags & FL_NEEDS_SYNC))
                {
                        (void) pipe_sync(entry, TRUE);
                }
                pfd = entry->handle.pbuf->pfd;
                free_pbuf(entry->handle.pbuf);
        }
        if(pfd != -1)
        {
                if(close(pfd) == -1) 
                {
                        serror("pipe close: %s", entry->path);
                }
                /*
                 * The close should cause termination of the child
                 * as the child reads EOF. The child is wait()'ed
                 * upon synchronously in a loop in main().
                 */
        }
        entry->path[0] = 0;
        entry->handle.pbuf = NULL;
        entry->private = 0;
}


/*
 * N.B. New return convention:
 * returns ENOERR (0) or, on failure, the errno.
 */
/*ARGSUSED*/
static int
pipe_put(fl_entry *entry, const char *ignored,
                const void *data, size_t sz)
{
        int status = ENOERR;

        udebug("    pipe_put: %d",
                entry->handle.pbuf ? entry->handle.pbuf->pfd : -1);
        TO_HEAD(entry);
        if(entry->handle.pbuf == NULL)
                return EINVAL;

        if (!(entry->flags & FL_NODATA)) {
            status = pbuf_write(entry->handle.pbuf,
                    data, sz, pipe_timeo, entry->path);

            if(status != ENOERR && status != EINTR)
            {
                    uerror("pipe_put: %s write error", entry->path);
                    /* don't waste time syncing an errored entry */
                    entry->flags &= ~FL_NEEDS_SYNC;
                    delete_entry(entry);
                    return status;
            }
        }
        entry->flags |= FL_NEEDS_SYNC;
        return ENOERR;
}


static struct fl_ops pipe_ops = {
        argcat_cmp,
        pipe_open,
        pipe_close,
        pipe_sync,
        pipe_put,
};

static int
pipe_putmeta(
    fl_entry*           entry,
    const prod_info*    info,
    uint32_t            sz)
{
    /*
     * metadata-length in bytes                                 uint32_t
     * data-product signature (MD5 checksum)                    uchar[16]
     * data-product size in bytes                               uint32_t
     * product creation-time in seconds since the epoch:
     *     integer portion                                      uint64_t
     *     microseconds portion                                 int32_t
     * data-product feedtype                                    uint32_t
     * data-product sequence number                             uint32_t
     * product-identifier:
     *     length in bytes (excluding NUL)                      uint32_t
     *     non-NUL-terminated string                            char[]
     * product-origin:
     *     length in bytes (excluding NUL)                      uint32_t
     *     non-NUL-terminated string                            char[]
     */

    int32_t     int32;
    uint32_t    uint32;
    uint64_t    uint64;
    uint32_t    identLen = (uint32_t)strlen(info->ident);
    uint32_t    originLen = (uint32_t)strlen(info->origin);
    uint32_t    totalLen = 4 + 16 + 4 + 8 + 4 + 4 + 4 + (4 + identLen) + 
        (4 + originLen);
    int         status;

    status = pbuf_write(entry->handle.pbuf, (void*)&totalLen,
        (u_int)sizeof(totalLen), pipe_timeo, entry->path);
    if (status != ENOERR) return status;

    status = pbuf_write(entry->handle.pbuf, (void*)&info->signature,
        (u_int)sizeof(info->signature), pipe_timeo, entry->path);
    if (status != ENOERR) return status;

    status = pbuf_write(entry->handle.pbuf, (void*)&sz,
        (u_int)sizeof(sz), pipe_timeo, entry->path);
    if (status != ENOERR) return status;

    uint64 = (uint64_t)info->arrival.tv_sec;
    status = pbuf_write(entry->handle.pbuf, (void*)&uint64,
        (u_int)sizeof(uint64), pipe_timeo, entry->path);
    if (status != ENOERR) return status;

    int32 = (int32_t)info->arrival.tv_usec;
    status = pbuf_write(entry->handle.pbuf, (void*)&int32,
        (u_int)sizeof(int32), pipe_timeo, entry->path);
    if (status != ENOERR) return status;

    uint32 = (uint32_t)info->feedtype;
    status = pbuf_write(entry->handle.pbuf, (void*)&uint32,
        (u_int)sizeof(uint32), pipe_timeo, entry->path);
    if (status != ENOERR) return status;

    uint32 = (uint32_t)info->seqno;
    status = pbuf_write(entry->handle.pbuf, (void*)&uint32,
        (u_int)sizeof(uint32), pipe_timeo, entry->path);
    if (status != ENOERR) return status;

    status = pbuf_write(entry->handle.pbuf, (void*)&identLen,
        (u_int)sizeof(identLen), pipe_timeo, entry->path);
    if (status != ENOERR) return status;

    status = pbuf_write(entry->handle.pbuf, (void*)info->ident,
        identLen, pipe_timeo, entry->path);
    if (status != ENOERR) return status;

    status = pbuf_write(entry->handle.pbuf, (void*)&originLen,
        (u_int)sizeof(originLen), pipe_timeo, entry->path);
    if (status != ENOERR) return status;

    status = pbuf_write(entry->handle.pbuf, (void*)info->origin,
        originLen, pipe_timeo, entry->path);

    return status;
}

static int
pipe_out(
    fl_entry*           entry,
    const product*      prodp,
    const void*         data,
    const uint32_t      sz)
{
    int         status = ENOERR;

    if (entry->flags & FL_METADATA) {
        status = pipe_putmeta(entry, &prodp->info, sz); 
    }
    if (status == ENOERR && !(entry->flags & FL_NODATA)) {
        status = pipe_put(entry, prodp->info.ident, data, sz); 
    }

    return status;
}


/*ARGSUSED*/
int
pipe_prodput(const product *prodp, int argc, char **argv,
                const void *ignored, size_t also_ignored)
{
        int status = 0;
        void *data = prodp->data;
        size_t sz = prodp->info.sz;
        fl_entry *entry = get_fl_entry(PIPE, argc, argv);

        udebug("    pipe_prodput: %d %s",
                (entry != NULL && entry->handle.pbuf)
                ?  entry->handle.pbuf->pfd : -1,
                prodp->info.ident);

        if(entry == NULL)
                return -1;

        if(entry->flags & FL_STRIP)
        {
                data = dupstrip(prodp->data, prodp->info.sz, &sz);
                if(data == NULL)
                        return -1;
        }

        status = pipe_out(entry, prodp, data, sz);
        if(status == EPIPE)
        {
                /*
                 * In case the decoder exited and we haven't yet reaped,
                 * try again once.
                 */
                uerror("pipe_prodput: trying again: %s",
                    s_prod_info(NULL, 0, &prodp->info, ulogIsDebug()));
                entry = get_fl_entry(PIPE, argc, argv);
                if(entry == NULL)
                        return -1;
                status = pipe_out(entry, prodp, data, sz);
        }
        if(data != prodp->data)
                free(data);

        if(status != ENOERR)
                return -1;

        return atFinishedArgs(argc, argv, entry);
}


/*ARGSUSED*/
int
spipe_prodput(const product *prod, int argc, char **argv,
                const void *ignored, size_t also_ignored)
{
        fl_entry *entry;
        char *buffer;
        size_t len;
        unsigned long offset;
        int status = ENOERR;

        typedef union {
                unsigned long u_long;
                char cu_long[sizeof(unsigned long)];
        } conv;
        conv key_len;
        conv data_len;
        conv sync;


        entry = get_fl_entry(PIPE, argc, argv);
        udebug("    spipe_prodput: %d %s",
                (entry != NULL && entry->handle.pbuf)
                        ?  entry->handle.pbuf->pfd : -1,
                        prod->info.ident);
        if(entry == NULL)
                return -1;

        /*
        **---------------------------------------------------------
        ** Place the following information into dbuf_val for 
        ** writing to the pipe:
        **
        ** unsigned long SPIPE_SYNC
        ** unsigned long key_len
        ** char *key
        ** unsigned long data_len  (this includes ETX/RS makers)
        ** char *data
        ** char SPIPE_ETX
        ** char SPIPE_RS
        **
        ** First, get lengths of key and data to allocate space
        ** in a temporary buffer.
        **
        **---------------------------------------------------------
        */
#ifndef SPIPE_SYNC
#define SPIPE_SYNC 0x1DFCCF1A
#endif /* !SPIPE_SYNC */

#ifndef SPIPE_ETX
#define SPIPE_ETX '\003'
#endif /* !SPIPE_ETX */

#ifndef SPIPE_RS
#define SPIPE_RS '\036'
#endif /* !SPIPE_ETX */

        key_len.u_long = strlen(prod->info.ident);
        data_len.u_long = prod->info.sz + 2;
        sync.u_long = SPIPE_SYNC;

        len = (unsigned ) (sizeof(unsigned long) +
          sizeof(key_len.cu_long) + strlen(prod->info.ident) +
          sizeof(data_len.cu_long) + prod->info.sz + 2);

        buffer = calloc(1, len);

        /*---------------------------------------------------------
        ** Now place the individual items into the buffer
        **-------------------------------------------------------*/

        offset = 0;

        memcpy (buffer+offset, sync.cu_long, sizeof(sync.cu_long));
        offset = offset + sizeof(unsigned long);

        memcpy(buffer+offset, key_len.cu_long, sizeof(key_len.cu_long));
        offset = offset + sizeof(key_len);

        memcpy(buffer+offset, prod->info.ident, (size_t)key_len.u_long);
        offset = offset + key_len.u_long;

        memcpy(buffer+offset, data_len.cu_long, sizeof(data_len.cu_long));
        offset = offset + sizeof(data_len);

        memcpy(buffer+offset, prod->data, prod->info.sz);
        
        /*---------------------------------------------------------
        ** Terminate the message with ETX & RS
        **-------------------------------------------------------*/
        buffer[len - 2] = SPIPE_ETX;
        buffer[len - 1] = SPIPE_RS;

        uerror("spipe_prodput: size = %d\t%d %d %d", prod->info.sz, buffer[len -3],
          buffer[len -2], buffer[len  -1]);

        /*---------------------------------------------------------
        ** Send this stuff and tidy up
        **-------------------------------------------------------*/
        status = pipe_put(entry, prod->info.ident, buffer, len);
        if(status == EPIPE)
        {
                /*
                 * In case the decoder exited and we haven't yet reaped,
                 * try again once.
                 */
                uerror("spipe_prodput: trying again");
                entry = get_fl_entry(PIPE, argc, argv);
                if(entry == NULL)
                        return -1;
                status = pipe_put(entry, prod->info.ident, buffer, len);
        }
        free(buffer);
        if(status != ENOERR)
                return -1;

        return atFinishedArgs(argc, argv, entry);

}


int
xpipe_prodput(const product *prod, int argc, char **argv,
                const void *xprod, size_t xlen)
{
        int status = ENOERR;
        fl_entry *entry;

        entry = get_fl_entry(PIPE, argc, argv);
        udebug("    xpipe_prodput: %d %s",
                (entry != NULL && entry->handle.pbuf)
                        ?  entry->handle.pbuf->pfd : -1, prod->info.ident);
        if(entry == NULL)
                return -1;

        status = pipe_put(entry, prod->info.ident, xprod, xlen);
        if(status == EPIPE)
        {
                /*
                 * In case the decoder exited and we haven't yet reaped,
                 * try again once.
                 */
                uerror("xpipe_prodput: trying again");
                entry = get_fl_entry(PIPE, argc, argv);
                if(entry == NULL)
                        return -1;
                status = pipe_put(entry, prod->info.ident, xprod, xlen);
        }

        if(status != ENOERR)
                return -1;

        return atFinishedArgs(argc, argv, entry);
}
/* End PIPE */


#ifndef NO_DB
# ifdef USE_GDBM
/* namespace conflict with gdbm_open, etc, so using prefix ldmdb_ */


/*
 * called in gdbm when it tries to punt
 * If we didn't provide this function, gdbm would print the
 * message and call exit(-1).
 */
static void
ldmdb_fatal ( char * str)
{
        serror("ldmdb_fatal(): %s", str);
}


/*
 * two or 3 args:
 *      pathname flag [dblocksize]
 *      if flag is 0 open read/write/create, otherwise open readonly
 */
static int
ldmdb_open(fl_entry *entry, int argc, char **argv)
{
        char *path;
        GDBM_FILE db;
        long tmp = 0;
        int read_write = GDBM_WRCREAT;
        /* default: choose to optimize for space over time */
#define DEFAULT_DBLOCKSIZE 512
        int dblocksize = DEFAULT_DBLOCKSIZE;

        entry->handle.db = NULL;
        path = argv[0];
        read_write = atoi(argv[1]);

        if(argc > 2)
        {
                if ( (tmp = atoi(argv[2])) > 0 )
                {
                        dblocksize = (int)tmp;
                }
                else 
                {
                        uerror("%s: ldmdb_open: -dblocksize %s invalid",
                                path, argv[1] );
                }
        }

        if(read_write != GDBM_READER) /* not read only */
        {
                /* create directories if needed */
                if(diraccess(path, (R_OK | W_OK), !0) == -1)
                {
                        serror("Couldn't access directories leading to %s", path);
                        return -1;
                }
        }

        /*
         * NB: It would be nice to set the GDBM file descriptor to close
         * on exec(), but that doesn't appear to be possible.
         */

        db = gdbm_open(path, dblocksize, read_write, 0664, ldmdb_fatal);
        if(db == NULL)
        {
                if(errno == EMFILE || errno == ENFILE)
                {
                        /* Too many open files */
                        close_lru(0);
                        close_lru(0);
                }
                serror("gdbm_open: %s", path);
                return -1;
        }
        entry->handle.db = db;
        entry->private = read_write;
        strncpy(entry->path, path, PATH_MAX);
        entry->path[PATH_MAX-1] = 0; /* just in case */
        udebug("    ldmdb_open: %s", entry->path);
        return 0;
}


static void
ldmdb_close(fl_entry *entry)
{
        udebug("    ldmdb_close: %s", entry->path);
        if(entry->handle.db != NULL)
                gdbm_close(entry->handle.db);
        entry->private = 0;
        entry->path[0] = 0;
        entry->handle.db = NULL;
}


static int
ldmdb_cmp(fl_entry *entry, int argc, char **argv)
{
        char *path;
        int read_write;
        int cmp;

        assert(argc > 1);
        assert(argv[0] != NULL);
        assert(*argv[0] != 0);

        path = argv[0];
        read_write = atoi(argv[1]);

        cmp = strcmp(path, entry->path);
        if(cmp == 0)
        {
                if(read_write != GDBM_READER &&
                        read_write != entry->private)
                {
                        /*
                         * the flags don't match, so close and reopen
                         */
                        ldmdb_close(entry);
                        if(ldmdb_open(entry, argc, argv) < 0)
                                cmp = -1;
                }
        }
        return cmp;
}


/*ARGSUSED*/
static int
ldmdb_sync(fl_entry *entry, int block)
{
        /* there is no gdbm_sync */
        udebug("    ldmdb_sync: %s", 
                entry->handle.db ? entry->path : "");
        entry->flags &= ~FL_NEEDS_SYNC;
        return(0);
}


/*ARGSUSED*/
static int
ldmdb_put(fl_entry *entry, const char *keystr,
                const void *data, size_t sz)
{
        datum key, content;
        int status;

        key.dptr = (char *) keystr /* N.B. cast away const */;
        key.dsize = (int) strlen(key.dptr) + 1; /* include the \0 */

        content.dptr = (char *) data; /* N.B. cast away const */
        content.dsize = (int)sz;

#if defined(DB_CONCAT) && !DB_XPROD
    /* concatenate duplicate keys  */
        /* 
         * Code for concatenating data when the key is a duplicate.
         * Contributed 9/17/91 JCaron/PNeilley/LCarson
         * Wrecks idea of "product" when applied at this layer, so
         * only define DB_CONCAT when DB_XPROD is not defined.
         */

        status = gdbm_store(entry->handle.db, key, content, GDBM_INSERT);
        if (status == 1 )
            {
            int         size;
            datum       old_stuff, new_stuff;
            old_stuff = gdbm_fetch(entry->handle.db, key);
                        udebug("\tConcatenating data under key %s", key.dptr);
            if (NULL == old_stuff.dptr)
                {
                serror("ldmdb_prodput: Inconsistent Duplicate Key storage");
                return -1;
                }
            size = content.dsize+old_stuff.dsize;
            if (NULL == (new_stuff.dptr = malloc(size)))
                {
                serror("ldmdb_prodput: malloc failed");
                free (old_stuff.dptr);
                return -1;
                }
            memcpy(new_stuff.dptr, old_stuff.dptr, old_stuff.dsize);
            memcpy(&new_stuff.dptr[old_stuff.dsize], content.dptr, content.dsize);
            new_stuff.dsize = size;
            status = gdbm_store(entry->handle.db, key, new_stuff, GDBM_REPLACE);
            free (new_stuff.dptr);
            free (old_stuff.dptr);
            }

#else
        /* TODO: replace flag */
        status = gdbm_store(entry->handle.db, key, content, GDBM_REPLACE);
#endif
        return status;
}

# else /*USE_GDBM*/

/*
 * two or 3 args:
 *      pathname flag [dblocksize]
 *      if flag is 0 open read/write/create, otherwise open readonly
 */
static int
ldmdb_open(fl_entry *entry, int ac, char **av)
{
        const char *path;
        int flags = (O_WRONLY|O_CREAT);

        assert(ac > 0);
        assert(av[ac -1] != NULL);
        assert(*av[ac -1] != 0);

        entry->handle.db = NULL;

        for(; ac > 1 && *av[0] == '-'; ac-- , av++)
        {
                if( strncmp(*av,"-overwrite",3) == 0)
                {
                        entry->flags |= FL_OVERWRITE;
                        flags |= O_TRUNC;
                }
                else if( strncmp(*av,"-strip",3) == 0)
                {
                        entry->flags |= FL_STRIP;
                }
        }

        path = av[ac-1];

        /* create directories if needed */
        if(diraccess(path, (R_OK | W_OK), 1) == -1)
        {
                serror("Couldn't access directories leading to %s", path);
                return -1;
        }

        /*
         * NB: It would be nice to set the DBM file descriptor to close
         * on exec(), but that doesn't appear to be possible.
         */

        entry->handle.db = dbm_open(path, flags, 0666);
        if(entry->handle.db == NULL)
        {
                if(errno == EMFILE || errno == ENFILE)
                {
                        /* Too many open files */
                        close_lru(0);
                        close_lru(0);
                        close_lru(0);
                        close_lru(0);
                }
                serror("ldmdb_open: %s", path);
                return -1;
        }
        strncpy(entry->path, path, PATH_MAX);
        entry->path[PATH_MAX-1] = 0; /* just in case */
        udebug("    ldmdb_open: %s", entry->path);
        return 0;
}


static void
ldmdb_close(fl_entry *entry)
{
        udebug("    ldmdb_close: %s", entry->path);
        if(entry->handle.db != NULL)
                dbm_close(entry->handle.db);
        entry->private = 0;
        entry->path[0] = 0;
        entry->handle.db = NULL;
}


static int
ldmdb_cmp(fl_entry *entry, int argc, char **argv)
{
        return str_cmp(entry, argc, argv);
}


/*ARGSUSED*/
static int
ldmdb_sync(fl_entry *entry, int block)
{
        /* there is no dbm_sync */
        udebug("    ldmdb_sync: %s", 
                entry->handle.db ? entry->path : "");
        entry->flags &= ~FL_NEEDS_SYNC;
        return(0);
}


/*ARGSUSED*/
static int
ldmdb_put(fl_entry *entry, const char *keystr,
                const void *data, size_t sz)
{
        datum key, content;
        int status;

        key.dptr = (char *) keystr /* N.B. cast away const */;
        key.dsize = (int) strlen(key.dptr) + 1; /* include the \0 */

        content.dptr = (char *) data; /* N.B. cast away const */
        content.dsize = (int)sz;

#if defined(DB_CONCAT) && !DB_XPROD
    /* concatenate duplicate keys  */
        /* 
         * Code for concatenating data when the key is a duplicate.
         * Contributed 9/17/91 JCaron/PNeilley/LCarson
         * Wrecks idea of "product" when applied at this layer, so
         * only define DB_CONCAT when DB_XPROD is not defined.
         */

        status = dbm_store(entry->handle.db, key, content, DBM_INSERT);
        if (status == 1 )
            {
            int         size;
            datum       old_stuff, new_stuff;
            old_stuff = dbm_fetch(entry->handle.db, key);
                        udebug("\tConcatenating data under key %s", key.dptr);
            if (NULL == old_stuff.dptr)
                {
                serror("ldmdb_prodput: Inconsistent Duplicate Key storage");
                return -1;
                }
            size = (int)(content.dsize+old_stuff.dsize);
            if (NULL == (new_stuff.dptr = malloc(size)))
                {
                serror("ldmdb_prodput: malloc failed");
                free (old_stuff.dptr);
                return -1;
                }
            memcpy(new_stuff.dptr, old_stuff.dptr, old_stuff.dsize);
            memcpy(&((char *)new_stuff.dptr)[old_stuff.dsize],
                        content.dptr, content.dsize);
            new_stuff.dsize = size;
            status = dbm_store(entry->handle.db, key, new_stuff, DBM_REPLACE);
            free (new_stuff.dptr);
            free (old_stuff.dptr);
            }

#else
        /* TODO: replace flag */
        status = dbm_store(entry->handle.db, key, content, DBM_REPLACE);
#endif
        return status;
}
# endif /*USE_GDBM*/


static struct fl_ops ldmdb_ops = {
        ldmdb_cmp,
        ldmdb_open,
        ldmdb_close,
        ldmdb_sync,
        ldmdb_put,
};


/*ARGSUSED*/
int
ldmdb_prodput(const product *prod, int ac, char **av,
                const void *xp, size_t xlen)
{
        fl_entry *entry;
        int status;
        int closeflag = 0;

        const char *keystr;
        char *dblocksizep = NULL;
        char *gdbm_wrcreat = "2";

        for(; ac > 1 && *av[0] == '-'; ac-- , av++)
        {
                if( strncmp(*av,"-close",3) == 0)
                        closeflag = 1;
                else if( strncmp(*av,"-dblocksize",3) == 0)
                {
                        ac--; av++;
                        dblocksizep = *av;
                } else
                        uerror("dbfile: Invalid argument %s", *av);

        }

        {
                /* set up simple argc, argv for ldmdb_open */
                int argc = 0;
                char *argv[4];
                argv[argc++] = av[0];
                argv[argc++] = gdbm_wrcreat;
                if(dblocksizep != NULL)
                        argv[argc++] = dblocksizep;
                argv[argc] = NULL;
                entry = get_fl_entry(FT_DB, argc, argv);
                udebug("    ldmdb_prodput: %s %s",
                        entry == NULL ? "" : entry->path, prod->info.ident);
                if(entry == NULL) return -1;
        }

        ac--; av++;

        if(ac >= 0 && av[0] != NULL && *av[0] != 0) 
        {
                /* use command line arg as key */
                keystr = av[0];
        }
        else
        {
                /* use product->ident */
                keystr = prod->info.ident;
        }

#if DB_XPROD
        status = ldmdb_put(entry, keystr, xp, xlen);
#else
        status = ldmdb_put(entry, keystr, prod->data, prod->info.sz);
#endif

        if(status == -1)
        {
                uerror("db_put: %s error for %s, dbkey %s",
                        entry->path, prod->info.ident, keystr);
        }
        if(closeflag || status == -1)
        {
                delete_entry(entry);
        }

        return status;
}

#endif /* !NO_DB */


static fl_entry *
new_fl_entry(ft_t type, int argc, char **argv)
{
        fl_entry *entry = NULL;

        entry = Alloc(1, fl_entry);
        if(entry == NULL)
        {
                serror("new_fl_entry: malloc");
                return NULL;
        }
        entry->path[0] = 0;

        switch (type) {
        case UNIXIO :
                entry->ops = &unio_ops;
                break;
        case STDIO :
                entry->ops = &stdio_ops;
                break;
        case PIPE :
                entry->ops = &pipe_ops;
                break;
        case FT_DB :
#ifndef NO_DB
                entry->ops = &ldmdb_ops;
#else
                uerror("new_fl_entry: DB type not enabled");
                goto err;
                /*NOTREACHED*/
#endif /* !NO_DB */
                break;
        default :
                uerror("new_fl_entry: unknown type %d", type);
                goto err;
        }

        entry->flags = 0;
        entry->type = type;
        entry->next = NULL;
        entry->prev = NULL;
        entry->path[0] = 0;
        entry->private = 0;

        if( entry->ops->open(entry, argc, argv) == -1 )
                goto err;

        return entry;
err :
        free_fl_entry(entry);
        return NULL;
}


/*
 * Set the number of available file descriptors.
 *
 * Parameters:
 *      fdCount         The number of available file descriptors.
 * Returns:
 *      0               Success.
 *      -1              Failure.  Reason is logged.
 */
int
set_avail_fd_count(
    unsigned    fdCount)
{
    int         error;

    if (fdCount <= 1) {
        uerror("set_avail_fd_count(): Invalid file-descriptor count: %ld",
            fdCount);
        error = -1;
    }
    else {
        /*
         * Ensure that two file descriptors will be available to the last entry
         * in the list because, if this entry is a PIPE action, then it will
         * need two because it uses the pipe(2) system-call.
         */
        maxEntries = fdCount - 1;
        error = 0;
    }

    return error;
}

int
set_shared_space(
    int shid,
    int semid,
    unsigned size)
{
    int error;
    if(shid == -1 || semid == -1) {
        uerror("Shared memory is not available.  Notification system disabled.");
        error = -1;
    }
    else {
        shared_id = shid;
        sem_id = semid;
        shared_size = size;
        semarg.val = size;
        semctl(sem_id, 0, SETVAL, semarg);
        semarg.val = -1;
        semctl(sem_id, 1, SETVAL, semarg); 
        largest_queue_element = shared_size - 1;
        error = 0;
    }
    return error;
}


/*
 * Returns the maximum number of file-descriptors that one process can have 
 * open at any one time.
 *
 * NOTE: Under FreeBSD 4.9-RELEASE-p11, OPEN_MAX is 64 but 
 * sysconf(_SC_OPEN_MAX) returns 11095!
 */
long
openMax()
{
    static long max = 0;

    if (0 == max) {
#       ifdef OPEN_MAX
            max = OPEN_MAX;             /* minimum value: 20 */
#       else
            /*
             * The value must be determined using sysconf().
             */
            max = sysconf(_SC_OPEN_MAX);

            if (-1 == max) {
                /*
                 * The value can't be determined.  Fallback to the Standard
                 * UNIX value.
                 */
                max = _POSIX_OPEN_MAX;  /* 16 by definition */
            }
#       endif
    }

    return max;
}
