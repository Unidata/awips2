/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: filel.h,v 1.144.16.2.2.5 2008/09/17 16:35:45 steve Exp $ */
#ifndef _FILEL_H_
#define _FILEL_H_

#include <sys/types.h> /* pid_t */
#include "ldm.h"

/*
 * fl_entry.flags, args to close_lru()
 */
#define FL_NEEDS_SYNC 1
#define FL_OVERWRITE 2
#define FL_NOTRANSIENT 16
#define FL_STRIP 32
#define FL_LOG 64
#define FL_METADATA 128	/* write data-product metadata */
#define FL_NODATA 256 /* don't write data */
#define FL_EDEX 512 /* send message to memory segment */

#ifdef __cplusplus
extern "C" {
#endif

extern int unio_prodput( const product *prod, int argc, char **argv,
	const void *xprod, size_t xlen);
extern int stdio_prodput( const product *prod, int argc, char **argv,
	const void *xprod, size_t xlen);
extern int pipe_prodput( const product *prod, int argc, char **argv,
	const void *xprod, size_t xlen);
extern int spipe_prodput( const product *prod, int argc, char **argv,
	const void *xprod, size_t xlen);
extern int xpipe_prodput( const product *prod, int argc, char **argv,
	const void *xprod, size_t xlen);
#ifndef NO_DB
extern int ldmdb_prodput( const product *prod, int argc, char **argv,
	const void *xprod, size_t xlen);
#endif /* !NO_DB */
extern pid_t reap(pid_t pid, int options);
extern void fl_sync(int nentries, int block);
extern void close_lru(int skipflags);
extern void fl_close_all(void);
extern void endpriv(void);
extern int set_avail_fd_count(unsigned fdCount);
extern int set_shared_space(int shid, int semid, unsigned size);
extern long openMax();

struct edex_message {
  char filename[4096];
  char ident[256];
};
typedef struct edex_message edex_message;

union semun {
    int              val;    /* Value for SETVAL */
    struct semid_ds *buf;    /* Buffer for IPC_STAT, IPC_SET */
    unsigned short  *array;  /* Array for GETALL, SETALL */
    struct seminfo  *__buf;  /* Buffer for IPC_INFO
                                (Linux specific) */
};

#ifdef __cplusplus
}
#endif

#endif /* !_FILEL_H_ */
