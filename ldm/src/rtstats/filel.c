/*
 *   Copyright 1994, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: filel.c,v 1.5 2003/04/02 23:19:33 steve Exp $ */

/* 
 *
 */

#include <ldmconfig.h>
#include <limits.h> /* PATH_MAX */
#ifndef PATH_MAX
#define PATH_MAX 255
#endif /* !PATH_MAX */
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "ldm.h"
#include "ldmalloc.h"
#include "ldmprint.h"
#include "ulog.h"

struct fl_entry {
	struct fl_entry *next;
	struct fl_entry *prev;
	char path[PATH_MAX+1];
	FILE *fp;
} ;
typedef struct fl_entry fl_entry ;

#define MAXENTRIES 8
