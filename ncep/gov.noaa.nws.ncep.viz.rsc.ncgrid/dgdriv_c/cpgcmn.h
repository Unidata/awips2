/************************************************************************
 * CPGCMN.H                                                             *
 *                                                                      *
 * This header file declares the structures needed for the product 	*
 * generation library.							*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi        6/96    Created  	                        *
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/

#ifndef CPGCMN_H
#define CPGCMN_H

#define MAX_NAMESZ	20
#define MAX_DESCRSZ	100
#define MAX_CUTS 10

#define ZEROS_MODE 0
#define ONES_MODE 1
#define NO_MODE  2


typedef struct prdrec
{
	/* ---  Common to both AFOS and FAX products --- */
	char 	pfname[60];		/* filename of product */
	char 	psubset[MAX_NAMESZ];  	/* product subset or pil # */
	char 	ptype[MAX_NAMESZ];	/* contains either AFOS or FAX */
	Cardinal xsize;			/* Size of output product */
	Cardinal ysize;			/* Size of output product */
	int	origx;			/* origin along X direction */
	int	origy;			/* origin along Y direction */
	float	sszx;			/* source size along x direction */
	float	sszy;			/* source size along Y direction */

	/* --- FAX settings only (not AFOS) --- */
	char 	pdesc[MAX_DESCRSZ];	/* product description */
	char 	pwheel[MAX_NAMESZ];	/* wheel number for fax product */
	char 	rle[MAX_NAMESZ];	/* type of Run Lenght Encoding */
	float 	rotate;			/* rotation of fax cut */
	int 	bitpix;			/* bits per pixel */

	/* --- AFOS settings only (not FAX) --- */
	int	tau;			/* time span indicator */
	char	prjkey[MAX_NAMESZ];	/* projection key */
	int	msgadr;			/* message addressee */
	int 	msgtyp;			/* message type */
	int	geoscl;			/* geography scale */
	char	ccc[MAX_NAMESZ];	/* NMC */
	char	nnn[MAX_NAMESZ];	/* GPH or other AFOS category of prod */
	char 	utfhed[MAX_DESCRSZ];	/* utf header */
	int	prjind;			/* projection indicator */
	int	fontsz;			/* font size */

        /* --- Printer settings only (not FAX, and not AFOS) --- */ 
	int	pagesz;			/* size of paper for a printer */
	
} PrdRec;


/* 
 * Declare a conversion record.  This record is used to perform 
 * conversion from internal format (raster) to external format
 * (6 bit, or UTF, etc.)
 */
typedef struct convert_rec
{
    int ixlen;    /* number of pixels in X direction  */
    int iylen;    /* number of pixels in Y direction */
    char pname[MAX_NAMESZ];   /* name of the product */
    int xsize;
    int ysize;
    int bpp;	  /* bits per pixel in input plane */
    char psubset[MAX_NAMESZ];  /* product subset number */
    char pdesc[MAX_DESCRSZ];  /* product description */
    char ptype[MAX_NAMESZ];   /* product type (fax,afos) */
    char rle[MAX_NAMESZ];     /* type of runlength encoding */
    float rot;
    int origx;
    int origy;
    float sszx;
    float sszy;

    
} ConvertRec;

#include "proto_nfax.h"

#endif
