/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: wmo_header.h,v 1.12 2000/06/23 15:55:14 rkambic Exp $ */
#ifndef _WMO_HEADER_H_
#define _WMO_HEADER_H_

#include "xbuf.h"
#include "dtime.h"

typedef enum {
	MESSAGE_TYPE_UNKNOWN = 0 ,
	SYNOP,	/* FM 12 */
	SHIP,	/* FM 13 */
	METAR,	/* FM 15 or SAO!! */
	SPECI   /* FM 16 */
} message_type_t;


typedef enum {
	ORIGINAL = 0, 
	RTD ,	/* delayed */
	COR ,	/* correction */
	AMD ,	/* amended */
	PIE	/* pieces */
} retransmit_t;


/* abbreviated heading, 2.3.2 */
typedef struct {
	char TT[3];	/* Tsub1Tsub2 : Data type and/or form */
	char AA[3];	/* Asub1Asub2 : Geograph. and/or time */
	int ii;
	char CCCC[5];  /* station of origin or compilation */
	char PIL[10];
	dtime *time;
	retransmit_t retransmit; /* BBB delay, correction or amendment ind */
	int retrans_seq; /* the sequence from BBB */
} wmo_header_t;

extern void free_wmo_header(wmo_header_t *hdr);
extern wmo_header_t *new_wmo_header(char **line);
extern wmo_header_t *get_wmo_header(xbuf *buf, wmo_header_t *hdr);

extern char *
s_wmo_header(wmo_header_t *hdr);

/* 
extern int
fprint_wmo_header(FILE *fp , const wmo_header_t *hdr);
*/

extern char *sRetransmit(wmo_header_t *hdr );
extern char *sMessage_type(message_type_t type);
extern message_type_t decode_type(char *tt, char *aa, char *PIL);

#endif /* _WMO_HEADER_H_ */
