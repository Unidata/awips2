/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: wmo_start.h,v 1.6 2000/08/10 14:04:56 russ Exp $ */
#ifndef _WMO_START_H_
#define _WMO_START_H_
#include "xbuf.h"

/*
 * C: "main type of the information"
 * Attachment II-5, section 2 (a).
 */
typedef enum {
	CLASSIFICATION_UNKNOWN = -1,
	Addressed_message = 0,
	Surface_data_global = 1,
	Surface_data_other = 2,
	Upper_air_data = 3,
	Analyses = 4,
	Forecasts = 5,
	Warnings = 6,
	Satellite_data = 7,
	Grid_point_value_message = 8,
	Pictorial_information = 9
} wmo_classification_t;


/*
 * Lsub1Lsub2: Originating Center
 * Attachment II-5, section 2 (b).
 * Don't currently have the table to decode...
 */
typedef int bulletin_originating_centre_t;


/* starting line, 2.3.1.2 */
typedef struct {
	int seqno;	/* nnn: Transmission sequence number, 000 - 999 */
	wmo_classification_t clss;
	bulletin_originating_centre_t origin;
	int Lsub3;
	int Lsub4;
} wmo_start_t;


extern void free_wmo_start(wmo_start_t *st);
extern wmo_start_t *new_wmo_start(char **line);
/* extern fprint_wmo_start(FILE *fp, const wmo_start_t *st); */
extern wmo_start_t *get_wmo_start(xbuf *buf, wmo_start_t *st);

#endif /* _WMO_START_H_ */
