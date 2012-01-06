/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: fdnb.h,v 1.2 1995/10/19 00:34:28 davis Exp $ */
#ifndef _FDNB_H_
#define _FDNB_H_

#ifdef __cplusplus
extern "C" int set_fd_nonblock( int fd );
extern "C" int clr_fd_nonblock( int fd );
#elif defined(__STDC__)
extern int set_fd_nonblock( int fd );
extern int clr_fd_nonblock( int fd );
#else
extern int set_fd_nonblock();
extern int clr_fd_nonblock();
#endif

#endif /* !_FDNB_H_ */
