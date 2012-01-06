/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: mkdirs_open.h,v 1.8 1999/04/03 00:26:12 davis Exp $ */
#ifndef _MKDIRS_H_
#define _MKDIRS_H_

/*
 * Like mkdir(2), but will create components as necessary.
 * The specified mode is used to create the directories.
 * Returns 0 if successful, -1 on failure.
 */
#ifdef __cplusplus
extern "C" int mkdirs(const char *path, mode_t mode);
#elif defined(__STDC__)
extern int mkdirs(const char *path, mode_t mode);
#else /* Old Style C */
extern int mkdirs();
#endif


/*
 * Like open(2), but will create components as necessary.
 * Returns valid file descriptor if successful, -1 on failure.
 */
#ifdef __cplusplus
extern "C" int mkdirs_open(const char *path, int flags, mode_t mode);
#elif defined(__STDC__)
extern int mkdirs_open(const char *path, int flags, mode_t mode);
#else /* Old Style C */
extern int mkdirs_open();
#endif

/*
 * Check to see if we have access to all components of 'path'
 * up to the last component. (Doesn't check the access of the full path)
 * If 'create' is no zero, attempt to create path components (directories)
 * as necessary.
 * Returns 0 if access is ok, -1 on error.
 */
#ifdef __cplusplus
extern "C" int diraccess(const char *path, int access_m, int create);
#elif defined(__STDC__)
extern int diraccess(const char *path, int access_m, int create);
#else /* Old Style C */
extern int diraccess();
#endif

#endif /* !_MKDIRS_H_ */
