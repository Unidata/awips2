
/************************************************************************
 * proto_uka.h                                                      	*
 *                                                                      *
 * This include file contains function prototypes for the UKA files   	*
 * in the cgemlib UKA library.						* 
 *									*
 **                                                                     *
 * M. Li/SAIC		05/04	Modified from proto_sig.h		*
 * M. Li/SAIC		07/04	Added flight level to uka_jtsp		*
 * M. Li/SAIC		08/04	Added uka_ptmcld			*
 * M. Li/SAIC		09/04	Added chlvl to uka_pttur, uka_ptjet,	*
 *					       uka_ptfrt, and uka_pttrp	*
 ***********************************************************************/


#ifndef PROTO_UKA
#define PROTO_UKA

void uka_ptcld ( FILE 	*ifpout, 
		 char 	*fname, 
		 FILE 	*fptr, 
		 long 	size,
                 char 	gptyp, 
		 char 	*gtstr, 
		 int 	maxgnm, 
		 int 	*iret );

void uka_ptfrt ( FILE 	*ifpout, 
		 char 	*fname, 
		 FILE 	*fptr,	
		 long 	size,
	         char 	gptyp, 
		 char 	*gtstr, 
		 int 	maxgnm, 
		 char	*chlvl,
		 int 	*iret );

void uka_grpchk ( char      *fname, 
		  FILE      *fptr, 
                  long      size, 
                  char      *gtstr, 
                  int       members[], 
                  int       ingrp, 
                  Boolean   *process, 
                  int       *iret );

void uka_ptjet ( FILE 	*ifpout, 
		 char 	*fname, 
		 FILE	*fptr,
		 long 	size,
                 char 	gptyp, 
		 char 	*gtstr, 
		 int 	maxgnm, 
		 char	*chlvl,
		 int 	*iret);

void uka_jtin (  int 	njp, 
		 float 	jlat[], 
	 	 float 	jlon[], 
		 int 	nwp,
                 float 	wlat[], 
		 float  wlon[], 
		 float  wspd [],
                 float 	wlvl[], 
		 float	wlvla[],
		 float 	wlvlb[],		 
		 int  	wtyp[], 
		 int    *nop, 
		 float  olat[],
                 float 	olon[], 
  		 float 	ospd[], 
 		 float  olvl[], 
		 float 	olvla[],
		 float	olvlb[],
		 int 	*iret );

void uka_jtsp (  int 	tmppt, 
		 int 	idxtyp[], 
                 float 	indx[], 
		 float	tlvl[],
		 int 	*iret);

void uka_ptmcld( FILE   *ifpout,
                 char   *fname,
                 FILE   *fptr,
                 long   size,
                 char   gptyp,
                 char   *gtstr,
                 int    maxgnm,
                 int    *iret );

void uka_ptrad ( FILE 	*ifpout, 
		 char 	*fname, 
		 FILE 	*fptr,	
		 long 	size,
		 char 	gptyp, 
		 char 	*gtstr, 
		 int 	*iret);

void uka_ptstm ( FILE 	*ifpout, 
		 char 	*fname, 
		 FILE 	*fptr,	
		 long 	size,
		 char 	gptyp, 
		 char 	*gtstr, 
		 int 	maxgnm, 
		 int 	*iret);

void uka_pttrp ( FILE 	*ifpout, 
		 char 	*fname, 
		 FILE	*fptr, 
		 long 	size,
	         char 	gptyp, 
		 char 	*gtstr, 
		 char	*chlvl,
		 int 	*iret);

void uka_pttur ( FILE 	*ifpout, 
	         char 	*fname, 
		 FILE	*fptr, 
		 long 	size,
                 char	gptyp, 
		 char 	*gtstr, 
		 int 	maxgnm, 
		 char	*chlvl,
		 int 	*iret);

void uka_ptvol   ( FILE *ifpout, 
		   char *fname, 
		   FILE *fptr, 
		   long size,
                   char gptyp, 
		   char *gtstr, 
		   int	maxgnm, 
		   int  *iret);

void v2uinp    	(  char*, char*, char*, char*, char*, char*, int*, size_t, size_t, size_t, size_t, size_t, size_t );

#endif /* PROTO_UKA */
