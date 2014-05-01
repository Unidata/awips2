/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: dtime.h,v 1.5 1995/10/24 01:30:57 davis Exp $	*/
#ifndef _DTIME_H_
#define _DTIME_H_

/*
 * A time structure
 */
typedef struct {
	int min;	
	int hour;	
	int mday;	
	int mon;	
	int year;	
} dtime;


extern void free_dtime(dtime *);
extern void clear_dtime(dtime *);
extern dtime *set_dtime(dtime *dt, int mday, int hour, int min);
extern dtime *new_dtime(int mday, int hour, int min);
extern dtime *dtimeNow(void);
/* extern fprint_dtime(FILE *fp, const dtime *time); */

#endif /* !_DTIME_H_ */
