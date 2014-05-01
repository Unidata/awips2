/************************************************************************
 * proto_textlib.h                                                      *
 *                                                                      *
 * This include file contains function prototypes for all the c files   *
 * in the textlib libraries.						*
 *									*
 * Functions that use VG_DBStruct are prototyped in proto_vg.h.		*
 *									*
 **                                                                     *
 * A. Hardy/NCEP	 7/03   Created                                 *
 * A. Hardy/NCEP	11/03   Modified wbc_dhdl-added status		*
 * A. Hardy/NCEP	 1/04	Added VTEC parameters to wbc_dcty	*
 * A. Hardy/NCEP	 3/04	Added wbc_mzhv and wbc_mzrm		*
 * A. Hardy/NCEP	 3/05	Added irmzn to wbc_dsts;rm len2 also	*
 * A. Hardy/NCEP	 4/05	Added utl_gdat and wbc_wcp		*
 * H. Zeng/SAIC		10/05	added af_displayPts			*
 * H. Zeng/SAIC		06/06	changed calling sequence for utl_ivet	*
 * E. Safford/SAIC	05/07	added af_getAirmetXml			*
 * E. Safford/SAIC	12/07	use G_Boolean to rm X/Motif dependency  *
 ***********************************************************************/

#ifndef PROTO_TEXTLIB
#define PROTO_TEXTLIB

/*
 * af prototypes
 */

void	af_getAirmetXml ( char *vgFile, int nAreas, char areas[][ 8 ], 
		           int nTypes, char *types[3], char *day, 
			   char *cycle, char *issueTm, char *outputXml[6][3], 
			   int *iret );


void    af_displayPts ( char    *xml_str );

/*
 *  utl prototypes
 */

void 	utl_ampm ( 	int 	itime, 
			int 	*newtime, 
			char	*ampm, 
			int 	*iret );

void 	utl_avcd ( 	char 	*locnam, 
			float	*plat, 
			float	*plon,
			char 	*disdir, 
			char 	*stn, 
			int 	*iret );

void 	utl_ctim ( 	int 	len, 
			char    *curtim, 
			int 	*iret );

void 	utl_gdat ( 	int 	dtmonth, 
			int 	daywk, 
			char 	*pmm, 
			char 	*pdwk, 
			int 	*iret );

void 	utl_gmon ( 	int 	dtmonth, 
			char 	*pmm,
			int 	*iret );

void 	utl_gdwk ( 	int 	daywk, 
			char 	*pdwk, 
			int 	*iret );

void 	utl_gname ( 	char 	*acstn1, 
			char 	*acnam1,
		 	char 	*acst1, 
			int 	*iret );

void 	utl_gtod ( 	int 	vhour, 
			int 	ehour, 
			int 	emin, 
			char 	*vampm,
			char 	*eampm, 
			int 	daywk, 
			int 	len, 
			char 	*genday, 
			int 	*iret );

void 	utl_ivet ( 	char 	*lclzn, 
			int 	itmarr[], 
			int 	vtime[], 
			int 	etime[],
			int 	iarr[], 
			int 	*inewtm, 
			char 	*iampm,
			char 	*chmon, 
			char 	*chdwk, 
			int 	varr[], 
			int 	*vnewtm,
			char 	*vampm, 
			int 	earr[], 
			int 	*enewtm, 
			char 	*eampm,
			int 	*datwk, 
			int 	*iret);

void	utl_sort (	int	*iind, 
			char 	**ind_arr, 
			int 	*iret );

void 	utl_state ( 	char 	*acst1, 
			int 	len, 
			char 	*stnam, 
			int 	*iret );

void 	utl_tomin ( 	float 	*anclat, 
			float 	*anclon, 
			float 	*newlat,
                 	float 	*newlon, 
			int 	*iret );

void	utl_ugcp (	char 	**ugc_arr, 
			int 	*nugc, 
			char 	*eday, 
			char 	*ehour,
               		int 	*len1, 
			char 	*ugcstr, 
			int 	*iret );

void 	utl_wfos ( 	char 	**wfoarr, 
			int 	numwfo, 
			char 	*wfostr, 
			int 	*iret );

void 	utl_wnmst ( 	char 	*wfoid, 
			char 	*wname, 
			char 	*wstate, 
			int	*iret );


/*
 *  wbc prototypes
 */

void 	wbc_area ( 	char 	*locnam, 
			char 	*vorstr, 	
			int 	len, 
			char 	*areastr,
     	           	int 	*iret );

void 	wbc_davn ( 	char 	*wtype, 
			float 	*hailsz, 
			int 	*maxgust, 
			int 	*maxtops,
                	int 	*degree, 
			int 	*speed, 
			int 	len1, 
			char 	*avnstr,
			int 	*iret );

void 	wbc_dcty ( 	char 	**ugc_arr, 
			char 	**cnam_arr, 
			char 	**st_arr, 
			int 	*ncnty,
			char	*eday, 
			char 	*ehour, 
			int	*len1,
			int	*ugcln,
			int	*vtecln,
			char    *prdcod,
			char    *actn,
			char    *offid,
			char    *phen,
			char    *sigcd,
			char    *etn,
			int     vtime[],
			int     etime[],
			char	**ind_arr, 
			char    *cntystr,
			int 	*iret );

void 	wbc_dcon ( 	char 	*wtype, 
			int 	len1, 
			int 	len2, 
			char 	*constr,
                	char 	*perstr, 
			int 	*iret );

void	wbc_defl (	int 	vhour, 
			int 	vmins, 
			char 	*vampm, 
			int 	ehour,
			int 	emins, 
			char 	*eampm, 
			char 	*lclzn, 
			int 	len1,
			int 	len2, 
			char 	*efst, 
			char 	*efen, 
			int 	*iret );

void 	wbc_dhdl ( 	char 	*wtype, 
			char 	*status, 
			int 	*wnum, 
			int 	lenh, 
			char 	*hdlstr, 
			int 	*iret );

void 	wbc_dhwm ( 	char 	*sep, 
			float 	*hailsz, 
			int 	*maxgust, 
			int 	*maxtops,
                	int 	*degree, 
			int 	*speed, 
			int 	len1, 
			char 	*hwmstr, 
			int 	*iret );

void 	wbc_dsts ( 	char 	*states, 
			int 	*len1, 
			int 	*irmzn, 
			char 	*stzstr,
                	char 	*sttstr, 
			int 	*iret );

void	wbc_mzhv (	char	*states,
			G_Boolean *hvmz,
			int	*iret);

void 	wbc_mzrm ( 	char 	*states, 
			char 	*ststr, 
			int 	*len1, 
			int 	*iret );

void 	wbc_vors ( 	char 	*locnam, 
			int	type, 
			int 	*vpdst1, 
			char 	*vpdir1,
			char 	*vpstn1, 
			int 	*vpdst2, 
			char 	*vpdir2, 
			char 	*vpstn2,
			float 	*wclat1, 
			float 	*wclon1, 
			float 	*wclat2, 
			float 	*wclon2,
			float 	*wclat3, 
			float 	*wclon3, 
			float 	*wclat4, 
			float 	*wclon4,
			int 	len, 
			char 	*vorstr, 
			float 	*vorlat1, 
			float 	*vorlon1,
			float 	*vorlat2, 
			float 	*vorlon2, 
			int 	*iret );


void	wbc_wcp (	int 	*ibun, 
			char 	*systim, 
			char 	**wtype, 
			char 	**wstart,
	       		char 	**wend, 
			char 	**wnum, 
			char 	**wlatlon, 
			int 	*iret );


#endif /* PROTO_TEXTLIB */
