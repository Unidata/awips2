
/************************************************************************
 * proto_vf.h                                                      	*
 *                                                                      *
 * This include file contains function prototypes for the VF files   	*
 * in the gempak VF library.						* 
 *									*
 **                                                                     *
 * A. Hardy/GSC		12/00   Created					*
 * A. Hardy/GSC		 5/01   Removed parameter 'type' from vfarea,   *
 *                              'vmin' from vfgtod			*
 * R. Tian/SAIC         06/02   Changed vfwwcl, added vfsval            *
 * R. Tian/SAIC         07/02   Added vfwnmst	   	                *
 * H. Zeng/SAIC		09/04	Added vfwrep				*
 * T. Piper/SAIC	07/05	Removed vfwawn and vfwpwn		*
 * F. J. Yen/NCEP	 1/07	Added vfasaw				*
 * ***********************************************************************/


#ifndef PROTO_VF
#define PROTO_VF

void vfasaw  ( 	char 	*rcntsw,
               	int 	*iwtnum,
               	char 	*amdcde,
               	int 	*iret );

void vfcnsaw ( 	int 	*iret );

void vfcnsel ( 	int 	*iret );

void vfctim  ( 	int 	*iret );

void vfgname ( 	int 	*iret );

void vfrdrp  ( 	int 	*iret );

void vfread  ( 	int 	*iret );

void vfstate ( 	int 	*iret );

void vfwsaw  ( 	int 	*iret );

void vfwsev  ( 	int 	*iret );

void vfwwcl  (  char 	*preid, 
		char 	*filnam, 
		char 	*outstr, 
		int 	*iret );

void vfwoui  ( 	int 	*iret );

void vfwwcp  ( 	int 	*iret );

void vfgttxt ( 	char 	fname[], 
		int 	*iret );   

void vfrptxt ( 	char 	fname[], 
		int 	*iret );   

void vfspc   ( 	char 	fname[], 
		char 	strin[], 
		int	*iret );

void vfwsel  ( 	char 	strin[], 
		int 	*iret );

void vfampm ( 	int 	time, 	
		int 	*newtime, 	
		char	*ampm, 
		int 	*iret );

void vfeftm ( 	int 	*hrtm, 
		int 	*endtm, 
		char 	ampm1[], 
		int 	*iret );

void vfgdat ( 	int 	dtmonth, 
		int 	daywk, 
		char 	*pmm, 
		char 	*pdwk, 
		int 	*iret );

void vfgtod ( 	int 	vhour, 
		int 	ehour, 
		int 	emin,
		char 	*vampm, 
		char 	*eampm, 
		int 	daywk,
		char 	*genday, 
		int 	*iret );

void vftomin (	float 	anclat, 
		float 	anclon, 
		float 	*newlat, 
		float 	*newlon,
		int 	*iret);

void vfarea ( 	char 	*locnam, 
		char 	*vorstr,
		char 	*areastr, 
		int 	*iret );

void vfavcd ( 	char 	*locnam, 
		char 	*type, 
		float 	plat, 
		float 	plon,
		char 	*disdir, 
		char 	*stn, 
		int 	*iret );

void vfvors ( 	char 	*locnam, 
		char 	*type, 
		char 	*vorstr, 
		float 	*vorlat1,
		float 	*vorlon1, 
		float 	*vorlat2, 
		float 	*vorlon2,
		int 	*iret );

void vfsort ( 	int 	*iind, 
		int 	*iret );

void vfsval (	char 	*wtype, 
		char 	*etime, 
		char 	*fcstr, 
		int 	ncyfip,
                int 	icyfip[], 
		int 	*iret );

void vfwnmst (	char	*wfoid,
		char 	*wname, 
		char	 *wstate, 
		int	 *iret );

void vfwrep (   int      *iret );

#endif /* PROTO_VF */
