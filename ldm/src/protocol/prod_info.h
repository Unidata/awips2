#ifndef PROD_INFO_H
#define PROD_INFO_H

#include "ldm.h"

#include <stddef.h>
#include <stdio.h>

typedef struct {
    prod_info	info;
    unsigned	magic;
    char	origin[HOSTNAMESIZE+1];
    char	ident[KEYSIZE+1];
} InfoBuf;

prod_info*
ib_init(
    InfoBuf* const		buf);

prod_info*
pi_new(void);

void
pi_setOrigin(
    prod_info* const		info,
    const char* const		origin);

int
pi_copy(
    prod_info* const		dest,
    const prod_info* const	src);

prod_info*
pi_clone(
    const prod_info* const	info);

int
pi_equals(
    const prod_info* const	info1,
    const prod_info* const	info2);

void		pi_free(
    prod_info* const		info);

/*
 * Prints a prod_info to an output stream.
 *
 * Arguments:
 *	info	Pointer to the prod_info to be written to "file".
 *	file	Pointer to the output stream.
 * Returns:
 *	-1	Failure.  log_errno() called.
 *	else	Number of bytes written.
 */
int
pi_print(
    const prod_info* const	info,
    FILE*			file);

/*
 * Scans a prod_info from an input stream.
 *
 * Arguments:
 *	info	Pointer to prod_info created by pi_new() or pi_clone().
 *	file	Pointer to the input stream.
 * Returns:
 *	-1	Failure.  log_*() called.
 *	else	Number of bytes scanned.
 */
int
pi_scan(
    prod_info* const	info,
    FILE*		file);

#endif
