/************************************************************************
 * nmpdef                                                               *
 *                                                                      *
 * This header file contains the declaration and structures for 	*
 * character string used in the NMP library.                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            12/00   Created                                 *
 * M. Li/GSC		02/00	Add map to nmp_gmapattr & nmp_setmapattr*
 *				add nmp_getLTLNstr, nmp_makeSTNstr,  	*
 *				nmp_setMapstrs, nmp_getMapstrs, 	*
 *				nmp_sovlflg, nmp_simf, nmp_sproj	*
 * H. Zeng/EAI          08/01   added nmp_sdefmap                       *
 * M. Li/SAIC		01/02	added nmp_gtruattr			*
 * H. Zeng/EAI          05/02   removed nmp_gmapstr                     *
 * T. Piper/SAIC	04/05	garea parameter chsnge for nmp_smapattr	*
 * E. Safford/SAIC	12/07	use G_Boolean to rm X/Motif dependency	*
 ***********************************************************************/
#ifndef	NMPDEF_H
#define	NMPDEF_H

#define NMP_STR         60
#define NMP_OVL_STR     128
#define	MAX_STR		256
#define	MAX_OVL		50
#define	MAX_MAP		50

typedef	char	nmpstr_t[NMP_STR];
typedef	char	nmpovlstr_t[NMP_OVL_STR];

void nmp_getLTLNstr (	int		lp,
			char		*ltln_str,
			int		*iret );

void nmp_getMapstrs ( 	int		lp, 
			char		mapfile[], 
			char		mapattr[], 
			int		*iret );

void nmp_gltln (	int     	lp,
			char    	*ltln_str,
			int     	*iret );
	       
void nmp_gmapattr  (    int     	lp,
			nmpstr_t	map,
                        nmpstr_t        proj,
                        nmpstr_t        garea[2],
			int		*iret );

void nmp_gmapnms ( 	nmpstr_t        mapnms[],
			int             *iret );
		    
void nmp_govlattr ( 	int             ovl,
			int             lp,
			int		*itype, 
			nmpovlstr_t     ovlattr,
			int             *iret );

void nmp_govlnms ( 	nmpovlstr_t     ovlnms[],
			int             *iret );

void nmp_gtruattr ( 	int		lp, 
			nmpstr_t	tru_proj, 
			nmpstr_t	tru_garea[2], 
			int		*iret );

void nmp_makeSTNstr (	int		lp,
			int		ovl,
			char		*stn_str,
			int		*iret);

void nmp_mkstn (	int  		lp,
			int  		ovl,
			char 		*stn_str,
			int  		*iret );
		    
void nmp_setmapstr (	int		lp,
			int		*iret);

void nmp_simf (		int		lp, 
			char		imgfile[], 
			int		imgtyp,
			int		*iret );

void nmp_sdefmap   (    int             lp, 
                        int             *iret );


void nmp_smapattr ( 	int             lp,
                        nmpstr_t        map,
			nmpstr_t        proj,
			nmpstr_t        garea[2],
			G_Boolean	allp,
			int             *iret );

void nmp_sovlattr ( 	int             lp,
			int             ovl,
			nmpovlstr_t     ovlattr,
			int             *iret );

void nmp_sovlflg ( 	int		lp,
			int		ovl,
			G_Boolean	flg,
			int		*iret);

void nmp_sproj (	int		lp, 
			int		*iret );

void nmp_szoom ( 	int             lp,
			nmpstr_t        zmarea,
			int             *iret );

#endif
