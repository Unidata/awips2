/************************************************************************
 * proto_bfr.h                                                          *
 *                                                                      *
 * This include file contains function prototypes for the BFR directory.*
 *                                                                      *
 **                                                                     *
 * D. Kidwell/NCEP      10/03   From proto_sigbenc.h                    *
 * M. Li/SAIC		10/03	Changed call sequences			*
 * M. Li/SAIC		09/04	Added bfr_mcld; add new variable idcent	*
 * M. Li/SAIC		10/04	Added info_bufr and section2 to bfr*.c	*
 ***********************************************************************/


#ifndef PROTO_BFR
#define PROTO_BFR

#include "cascmn.h"

void bfr_cld    ( char 	*ofname, 
		  int 	numcld, 
		  cloud_t *ptrc, 
		  int 	itime[],
               	  int 	jtime[], 
		  int 	*nfxy, 
		  int 	*fxy_i, 
		  int 	*fxy_vals,
		  int	idcent,
		  int	info_bufr[],
		  char	*section2,
		  int 	*iret );

void bfr_frt    ( char 	*ofname, 
		  int 	numfrt, 
		  front_t *ptrf, 
		  int 	itime[],
               	  int 	jtime[], 
                  int   *nfxy, 
                  int   *fxy_i, 
                  int   *fxy_vals,
		  int	idcent,
                  int   info_bufr[],
                  char  *section2,
		  int 	*iret );


void bfr_jet    ( char 	*ofname, 
		  int 	numjet, 
		  jets_t *ptrj, 
		  int 	itime[],
                  int 	jtime[], 
                  int   *nfxy, 
                  int   *fxy_i, 
                  int   *fxy_vals,
	     	  int	idcent,
                  int   info_bufr[],
                  char  *section2,
		  int 	*iret );

void bfr_mcld   ( char  *ofname,
                  int   nummcld,
                  mcloud_t *ptrm,
                  int   itime[],
                  int   jtime[],
                  int   *nfxy,
                  int   *fxy_i,
                  int   *fxy_vals,
                  int   idcent,
                  int   info_bufr[],
                  char  *section2,
                  int   *iret );

void bfr_trp    ( char 	*ofname, 
		  int 	rnum, 
		  trop_t *ptrr, 
		  int 	hnum,
		  trophi_t *ptrh, 
		  int 	lnum, 
		  troplo_t *ptrl, 
		  int 	itime[],
		  int 	jtime[], 
                  int   *nfxy, 
                  int   *fxy_i, 
                  int   *fxy_vals,
		  int	idcent,
                  int   info_bufr[],
                  char  *section2,
		  int 	*iret );

void bfr_tur    ( char 	*ofname, 
		  int 	numtur, 
		  turb_t *ptrb, 
		  int 	itime[],
        	  int 	jtime[], 
                  int   *nfxy, 
                  int   *fxy_i, 
                  int   *fxy_vals,
		  int	idcent,
                  int   info_bufr[],
                  char  *section2,
		  int 	*iret );

void bfr_vts    ( char 	*ofname, 
		  int 	numstm, 
		  storm_t *ptrs, 
		  int 	numvlr,
		  volrad_t *ptrv, 
		  int 	itime[], 
		  int 	jtime[], 
                  int   *nfxy, 
                  int   *fxy_i, 
                  int   *fxy_vals,
		  int	idcent,
                  int   info_bufr[],
                  char  *section2,
		  int	*iret );

void bfr_make  (  char 	*ofname, 
		  int 	nfxy, 
		  int 	*fxy_i,
		  float *values,
		  int 	num_vals,
		  int 	jtime[], 
		  Data_MixVal_t *values_mx,
		  int	idcent,
                  int   info_bufr[],
                  char  *section2,
		  int	*iret );

void bfr_rdfxy (  char	*fxytbl,
		  int   maxfxy,
		  char  *datatype,
		  int	*nfxy,
		  int 	*fxy_i,
		  int   *fxy_vals,
		  int	*iret);

void bfr_rdmt  (  bfrtbl_t *ptrtbl,
		  int	*nitem,
		  int 	*iret );

void bfeinp    (  char*, char*, char*, char*, int*, size_t, size_t, size_t, size_t );

#endif /* PROTO_BFR */
