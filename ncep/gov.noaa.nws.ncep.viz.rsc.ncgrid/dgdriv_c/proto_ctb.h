
/************************************************************************
 * proto_ctb.h                                                      	*
 *                                                                      *
 * This include file contains function prototypes for the CTB files   	*
 * in the CTB libraries with structures defined in ctbcmn.h.		* 
 *									*
 **                                                                     *
 * A. Hardy/GSC		11/00   Created					*
 * D.W.Plummer/NCEP	 2/01	Added ctb_ccrd				*
 * A. Hardy/NCEP	 3/04   Added ctb_mzrd				*
 * A. Hardy/NCEP	10/04   Added ctb_permccrd			*
 * S. Gilbert/NCEP	11/04   Added ctb_g2rdcntr, ctb_g2rdlvl,        *
 *                                    ctb_g2rdvar       		*
 ***********************************************************************/


#ifndef PROTO_CTB
#define PROTO_CTB

/*
 *  ctb prototypes
 */

void 	ctb_ccrd ( 	char		*tblnam,
    			char		*dirsym,
			Clustcnty_t   	*cc,
			int		*iret );

void 	ctb_permccrd ( 	char		*tblnam,
    			char		*dirsym,
			Permclust_t   	*pc,
			int		*iret );

void	ctb_rbul (	char                   *tblnam,
			char                   *dirsym,
			int                    *maxbul,
			int                    *nbul,
			struct bulletin_list   *bularr,
			int                    *iret );

void 	ctb_rdtyp ( 	char		*tblnam,
			char		*dirsym,
			int		*maxdtyp,
			int		*ndtyp,
			struct datatype_list  *dtyparr,
			int		*iret );

void 	ctb_rmtyp ( 	char		*tblnam,
			char		*dirsym,
			int		*maxmtyp,
			int		*nmtyp,
			struct maptype_list   *mtyparr,
			int		*iret );

void 	ctb_rstn ( 	const char	*tblnam,
    			const char	*dirsym,
			int		*nstn,
			StnLst		**stnarr,
			int		*iret );

void 	ctb_mzrd ( 	char 		*tblnam, 
			char 		*dirsym, 
			int 		*nstn, 
			Marzon_t 	*mznms, 
			int 		*iret );

void	ctb_g2rdcntr (	char		*tbname, 
			G2wmocntrs	*cntrtbl, 
			int		*iret );

void	ctb_g2rdlvl (	char		*tbname,
			G2lvls		*lvltbl,
			int		*iret );

void 	ctb_g2rdvar ( 	char 		*tbname, 
			G2vars_t 	*vartbl, 
			int		*iret );

#endif /* PROTO_CTB */
