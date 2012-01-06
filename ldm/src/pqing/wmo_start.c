/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: wmo_start.c,v 1.12 2001/08/24 22:54:13 russ Exp $ */
#include <ldmconfig.h>
#include <stdio.h>
#include <ctype.h>
#include "wmo_start.h"
#include "ldmalloc.h"
#include "xbuf.h"
#include "tokens.h"


#ifndef NOTUSED
void
free_wmo_start(wmo_start_t *st)
{
	if(st == NULL) return;
	free(st);
}
#endif /* NOTUSED */


static void 
clear_wmo_start(wmo_start_t *st)
{
	st->seqno = -1;
	st->clss = CLASSIFICATION_UNKNOWN;
	st->origin = st->Lsub3 = st->Lsub4 = 0;		
}


/*
 * parse wmo "Starting Line" from the xbuf, xbuf gets updated to end of scan
 */
wmo_start_t *
get_wmo_start(xbuf *buf, wmo_start_t *st)
{
	int tmp;

	if(st==NULL) return NULL;
	clear_wmo_start(st);

	if( get_wnum(buf, &st->seqno, 3) == EOB ) return NULL;

	if( get_wnum(buf, &tmp, 1) == EOB) return NULL;
	if(tmp >= 0)
	{
		st->clss = (wmo_classification_t)tmp;
		if( get_num(buf, &st->origin, 2) == EOB) return NULL;
		if( get_num(buf, &st->Lsub3, 1) == EOB) return NULL;
		if( get_num(buf, &st->Lsub4, 1) == EOB) return NULL;
	}

	return st;
}


#if 0
fprint_wmo_start(FILE *fp, const wmo_start_t *st)
{
	fprintf(fp,
		"%03d", st->seqno);
	if(st->class != CLASSIFICATION_UNKNOWN)
	{
		fprintf(fp,
			" %1d%02d%1d%1d",
			st->class, st->origin, st->Lsub3, st->Lsub4);
	}
	return ferror(fp);
}
#endif
