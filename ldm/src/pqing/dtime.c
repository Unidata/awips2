/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: dtime.c,v 1.12 2001/08/24 22:54:12 russ Exp $	*/

#include <ldmconfig.h>
#include <stdio.h>
#include <time.h>
#include "ldmalloc.h"
#include "dtime.h"
#ifdef NO_GMTIME /* set your local clock to GMT */
#define gmtime(xx) localtime((xx))
#endif


void
free_dtime(dtime *time)
{
	if(time == NULL) return;
	free(time);
}


void
clear_dtime(dtime *dt)
{
	if(dt == NULL) return;
	dt->min  = 0;
	dt->hour = 0;
	dt->mday = 0;
	dt->mon  = 0;
	dt->year = 0;
}


dtime *
set_dtime(
	dtime *dt, 
	int mday,
	int hour,
	int min)
{
	time_t current;
	struct tm *tm;

	if(dt==NULL) return NULL;

	if(time(&current) == -1) return NULL;

	if((tm = gmtime(&current)) == NULL) return NULL;

	dt->year = 1900 + tm->tm_year;
	dt->mon = tm->tm_mon + 1;

	/* more sophisticated checking here? */
	if(mday <= 31 && mday >= 1) 
		dt->mday = mday;
	else
		dt->mday = tm->tm_mday;

	if(hour <= 23 && hour >= 0)
	{
		dt->hour = hour;
		dt->min = 0;
	}
	else 
	{
		dt->hour = tm->tm_hour;
		dt->min = tm->tm_min;
	}

	if(min <= 59 && min >= 0 )
		dt->min = min;

	/* if it is early in the month now and the report from later in the month,
		roll back month */
	if(tm->tm_mday < dt->mday )
	{
		dt->mon--; 
		/* if the month was January, roll back year too */
		if(dt->mon == 0)
		{
			dt->mon = 12;
			dt->year--;
		}
	}

	return dt;
}


dtime *
new_dtime(
	int mday,
	int hour,
	int min)
{
	dtime *ret;

	ret = Alloc(1, dtime);
	if(ret == NULL) return NULL;

	if( set_dtime(ret, mday, hour, min) == NULL )
	{
		free_dtime(ret);
		return NULL;
	}

	return ret;
}


dtime *
dtimeNow(void)
{
	dtime *ret;
	time_t current;
	struct tm *tm;

	ret = Alloc(1, dtime);
	if(ret == NULL) goto err;

	if(time(&current) == -1) goto err;

	if((tm = gmtime(&current)) == NULL) goto err;

	ret->year = 1900 + tm->tm_year;
	ret->mon = tm->tm_mon + 1;
	ret->mday = tm->tm_mday;
	ret->hour = tm->tm_hour;
	ret->min = tm->tm_min;

	return ret;

err:
	free_dtime(ret);
	return NULL;
}


#if 0
fprint_dtime(FILE *fp, const dtime *tm)
{
	return( fprintf(fp,	"%04d %02d %02d %02d:%02d",
		tm->year, tm->mon, tm->mday, tm->hour, tm->min));
}
#endif
