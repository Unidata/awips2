/************************************************************************
 * proto_nmaplib.h                                                     	*
 *                                                                      *
 * This include file contains function prototypes for all the c files   *
 * in the NMAPLIB library.						*
 *									*
 * Log:									*
 **                                                                     *
 * A. Hardy/GSC 	11/00	Created					*
 * E. Safford/GSC	12/00	clean up some pgwfmt, and utls call seq *
 * M. Li/GSC		01/01	added pggrpw_getGrpTypColr		*
 * E. Safford/GSC	01/01	remove private pggst functions          *
 * H. Zeng/EAI          01/01   added&deleted several functions         *
 * E. Safford/GSC	01/01   add a pggrpw func, rmv prvt pggrpw func *
 * M. Li/GSC		01/01	added pggrpw_initOlk			*
 * E. Safford/GSC	01/01   remove private pgpalw functions         *
 * H. Zeng/EAI          02/01   added pgwlst_getClstStatus              *
 * E. Safford/GSC	02/01	added *iret to pgutls_getSmoothedPt     *
 * H. Zeng/EAI          02/01   removed pggrpw_initType()               *
 * H. Zeng/EAI          03/01   removed a few pggrpw_xxxx() functions   *
 * S. Jacobs/NCEP	 3/01	Added ngd_gnms				*
 * H. Zeng/EAI          03/01   removed pggrpw_getGrpTypColr            *
 * E. Safford/GSC	03/01	added grptyp to pgline_getAttr          *
 * E. Safford/GSC	04/01	added pg*_getGrptyp()                   *
 * J. Wu/GSC		04/01	added pgundo_*IsActv()                  *
 * E. Safford/GSC	05/01	updated nmp prototypes			*
 * D.W.Plummer/NCEP      5/01   Added pgaofmt_update and pgsfp_getfname *
 * J. Wu/GSC		05/01	added pgolk_check()                     *
 * H. Zeng/EAI          05/01   Added some pggrpw_xxxx functions        *
 * M. Li/GSC		05/01	Added nmp_save and nmp_restore		*
 * H. Zeng/EAI          06/01   Added pgwlst_getInaAncPts()             *
 * S. Jacobs/NCEP	 7/01	Added ngd_rsfl; Updated ngd_tlst	*
 * E. Safford/SAIC	 8/01	added nmp_valid				*
 * J. Wu/SAIC		 8/01	add nmp_rdeflts				*
 * T. Piper/SAIC	10/01	fixed prototype for pgpalw_exit		*
 * H. Zeng/EAI          10/01   added some pg*_**** functions           *
 * E. Safford/SAIC	11/01	mv _pghdlb_refresh to pgutls_refresh	*
 * E. Safford/SAIC	11/01	add mcanvw_getCurPos			*
 * E. Safford/SAIC      11/01   add pgutls_checkNumDataCb               *
 * H. Zeng/EAI          11/01   added some pgwfmt_xxxx functions        *
 * D.W.Plummer/NCEP     12/01   add pgwlst_getWFO			*
 * H. Zeng/EAI          12/01   added pgwfmt_WCCselectCb()              *
 * H. Zeng/EAI          12/01   added pgwlst_setPanesSen()              *
 * J. Wu/SAIC           12/01   add pglayer functions                   *
 * E. Safford/SAIC      12/01   pgutls_checkNumDataCb -> _vrfyNumericCb *
 * D.W.Plummer/NCEP     12/01   rm pgaofmt_update; add pgolk_ptsprod &  *
 * 							pgolk_udpts	*
 * E. Safford/SAIC      12/01   add *_isUp functions                    *
 * H. Zeng/EAI          01/02   added pggst_touchGhost()                *
 * J. Wu/SAIC           02/02   add more pglayer functions 		*
 * H. Zeng/EAI          01/02   added some pglay*_**** functions.       *
 * E. Safford/SAIC      02/02   rm pgline_setColor, add pgline_saveAttr *
 * E. Safford/SAIC      03/02   add pglayer_*etDefGrp()                 *
 * E. Safford/SAIC	03/02	add pgutls_regroup()			*
 * E. Safford/SAIC	03/02	add pglayrw_exit()			*
 * D. Plummer/NCEP	03/02	rm pgolk_ptsprod & pgolk_udpts		*
 * E. Safford/SAIC	03/02	add pgolk_getGroup()			*
 * J. Wu/SAIC		03/02	add pglayrxt_****() functions		*
 * J. Wu/SAIC		03/02	add ***_setExit*** functions		*
 * J. Wu/SAIC		03/02	add mmenuw_exitNMAPCb			*
 * J. Wu/SAIC		03/02	add mmenuw_setExitNMAP			*
 * J. Wu/SAIC		03/02	add pgpalw_isExitPGEN			*
 * H. Zeng/EAI          03/02   added pggrpch_xxxx functions            *
 * M. Li/SAIC		04/02	added pglabel_s(g)etLabelPending	*
 * T. Lee/SAIC		04/02	added pgfilw_openVGF, pglpfw_xxxx,	*
 *				pgpalw_operCb, pgpalw_getOperWid	*
 * H. Zeng/EAI          04/02   removed some pggrpw_xxxx functions.     *
 * E. Safford/SAIC	05/02	param change for pgwfmt_getcontWtch	*
 * J. Wu/SAIC           05/02   remove pgutls_vrfyNumericCb            	*
 * M. Li/SAIC		05/02	Added pg*_updtGrpMenu			*
 * M. Li/SAIC		05/02	Added pgtrk_isUp			*
 * H. Zeng/EAI          05/02   added pgwfmt_getIssTmStr                *
 * M. Li/SAIC           06/02   Added pgsymb_createVolLst               *
 * E. Safford/SAIC	06/02	add pgwfmt_createWCL, pgwfmt_popdownWCL	*
 * M. Li/SAIC		06/02	add pgnew_setGrpInfo			*
 * J. Wu/SAIC           07/02   add pgline_getSymbInfo/pgline_getLabType*
 *            			pgpalw_getObjID/pgsymb_unmanageLabForm	*
 * J. Wu/SAIC           08/02   add pgsymb_getSize/pgsymb_getWidth	*
 *            			pgnew_getGrpInfo/pglabel_getlabType	*
 *            			pglabel_setlabType/pglabel_symbpopup	*
 * J. Wu/SAIC           08/02   add pglabel_getOldObject		*
 * J. Wu/SAIC           10/02   add pgextrap_**** package		*
 * J. Wu/SAIC           11/02   add pglist_**** package			*
 * J. Wu/SAIC           11/02   add color for pglist_setAttr & _getAttr	*
 * J. Wu/SAIC           12/02   add pglist_createItems			*
 * J. Wu/SAIC           12/02   add a few pglayrw_**** functions	*
 * J. Wu/SAIC           12/02   add pgpalw_isLayerActv()		*
 * H. Zeng/XTRIA        01/03   modified pgwbxw_setAttr&pgwbxw_getAttr  *
 * J. Wu/SAIC           03/03   add pgtxt_getSelMcloud			*
 * S. Jacobs/NCEP	 3/03   Modified pglist_createItems		*
 * H. Zeng/XTRIA	04/03   added pgpalw_rmDefVGF()			*
 * M. Li/SAIC		04/03	Added iclrs2 to nms_dspl		*
 * H. Zeng/XTRIA	06/03   added pgwlst_getCtlkStatus()		*
 * H. Zeng/XTRIA	07/03   added pgvolw_xxxx() functions		*
 * T. Lee/SAIC		08/03	added time interval to calling seq.	*
 * H. Zeng/XTRIA	09/03   added pgvacw_xxxx() functions		*
 * E. Safford/SAIC	11/03	added pgfilw_save(), pgfilw_saveAll()   *
 *                                rm pgfilw_saveAcceptCb		*
 * E. Safford/SAIC	11/03	rm pglayrw_switchColor			*
 * J. Wu/SAIC		11/03	added pgjet_xxxx functions		*
 * E. Safford/SAIC	12/03	add pgsmear_* functions			*
 * E. Safford/SAIC	12/03	add pgtrkw_getNumExtra 			*
 * H. Zeng/XTRIA	02/04	added pglpfw_setPath			*
 * T. Lee/SAIC		02/04	added auto-update and reference time	*
 * J. Wu/SAIC		02/04	added pggfaw_xxxx functions		*
 * A. Hardy/NCEP	 2/04   add pglist_grpToWtch, pglist_wrtLstWtch	*
 * B. Yin/SAIC		03/04   added pgtca_xxxx functions		*
 * J. Wu/SAIC		03/04	add pginterp_* functions		*
 * E. Safford/SAIC	04/04	add pgdel_keyboardDelete		*
 * B. Yin/SAIC		04/04	added pgtca_isNewSeg			*
 *				moved pgtca_popup into proto_vg.h	*
 * J. Wu/SAIC		04/04	add pgmsel_start; rm pgnew_resetListLoc,*
 *				    pglist_addItem & pglist_createItems *
 * T. Lee/SAIC		04/04	added idelrt to ngd_*, nsf_*, nsn_*	*
 * B. Yin/SAIC		06/04	added pglayrw_setLayer			*
 * E. Safford/SAIC	07/04	rm pgdsel*, rename msel*, add asel_start*
 * J. Wu/SAIC		07/04	add pgfilterw_xxxx functions		*
 * E. Safford/SAIC	07/04	add pgsymb_getAttrStr                   *
 * J. Wu/SAIC		07/04	add pgsmear GUI & pggfaw_cmpSeverity	*
 * J. Wu/SAIC		08/04	add more pgfilterw_*, pglayrw_*, and	*
 *				pggfaw_* functions for interval linkage	*
 * T. Piper/SAIC	08/04	Added nmp_mkscl				*
 * H. Zeng/SAIC		10/04	CSC to several pgwbxw_xxxx func.	*
 * T. Lee/SAIC          10/04   Added bin hours to nsf/n_plot           *
 * m.gamazaychikov/SAIC 11/04   Added pglist_updtStatusCntys and 	*
 *				      pgwbxw_updtWcnFlag		*
 * E. Safford/SAIC	11/04	param change pggfaw_cmpSeverity		*
 * T. Piper/SAIC	12/04	Added pgpalw_refresh			*
 * B. Yin/SAIC		12/04	added pgutls_vrfyTimeCb			*
 * T. Piper/SAIC	12/04	Added pggst_cursorGhost			*
 * B. Yin/SAIC		12/04	Added pgairmet functions		*
 * E. Safford/SAIC	01/05	add pggfaw_combineSigStrs       	*
 * A. Hardy/NCEP	02/04	added pgwatch_numsort			*
 * B. Yin/SAIC		03/05	Added pgtca_stepList			*
 * H. Zeng/SAIC		03/05	added pggfmt_GetTxtW			*
 * T. Piper/SAIC	04/05	garea parameter change for nmp_valid	*
 * B. Yin/SAIC		06/05	Added pggfaw_okToDraw			*
 * B. Yin/SAIC		06/05	Modified pggfaw_okToDraw		*
 * H. Zeng/SAIC		06/05	added pgwlst_getStatesForm, pgwlst_	*
 *				setShowQC, pgwatch_gQCcnty		*
 * M. Li/SAIC		09/05	Removed pgggc_update			*
 * E. Safford/SAIC	11/05	add pggfaw_get/set OK & cancelOnWarning	*
 * H. Zeng/SAIC		11/05	added pgwpts_expand			*
 * T. Piper/SAIC	12/05	changed parameters for pgxrain_setltlnp *
 * B. Yin/SAIC		12/05	added pggfaw_isClosed()			*
 * H. Zeng/SAIC		01/06	added pgwatch_clrNumCwas() &		*
 *				      pgwlst_getInoutRC()  &		*
 *				      pgwatch_loadStCwa()		*
 * T. Piper/SAIC	02/06	removed pgpalw_manage			*
 * H. Zeng/SAIC		04/06	added pgvolw_getProdInfo()		*
 * B. Yin/SAIC		07/06 	added pggfaw_getHazardType() &		*
 *				      pggfaw_makeSubtype()		*
 * H. Zeng/SAIC		07/06	added pgdist_xxxx functions		*
 * H. Zeng/SAIC		09/06	added mainw_getToplevel()		*
 * H. Zeng/SAIC		09/06	added pgprd_getTxtSiz()			*
 * M. Li/SAIC		09/06	added increment to pgwndw_getData	*
 * H. Zeng/SAIC		10/06	added pginterp_toBeContinued()		*
 * E. Safford/SAIC	01/07	added pggfaw_reorderGFA()		*
 * H. Zeng/SAIC         01/07   added mbotw_startLocDspl()              *
 * B. Yin/SAIC          01/07   added pggfaw_getFzlRangesFromFile()     *
 * D.W.Plummer/NCEP	01/07	added computational window lib fxns	*
 * H. Zeng/SAIC		04/07	added pgofmt_bulkProcessEnd()		*
 * H. Zeng/SAIC		06/07	changed the CS for nms_dspl()		*
 * E. Safford/SAIC	07/07	added pgairmetp* and pgcycle* functions *
 * T. Piper/SAIC        09/07   Modified nim_flnm and nim_gtim		*
 * T. Piper/SAIC        09/07   Removed nim_dspl, nim_ftot, nim_gfln,   *
 *                                                       & nim_tlst     *
 * J. Wu/SAIC           09/07   added pgfrom_*() & pgundo_getCurStep()	*
 * M. Li/SAIC           09/07   added pgwndw_getEditFlag                *
 * J. Wu/SAIC           10/07   added pgcycle_getIssue()		*
 * B. Yin/SAIC		12/07	removed pgfilter_getFirstTime()		*
 * J. Wu/SAIC           12/07   added pgutls_vrfyLmtPunctCb             *
 * T. Piper/SAIC	12/07	Added pgvolw_rdWords			*
 * E. Safford/SAIC	12/07	include XmAll.h -- this has been removed*
 *				 from geminc.h, and is needed here now  *
 * T. Piper/SAIC	01/08	Moved NxmWarn_show to proto_nxmlib	*
 * J. Wu/SAIC           02/08   added ncw_isup         			*
 * B. Yin/SAIC		04/08	added pgpalw_setClassMet		*
 * B. Yin/SAIC		04/08	added pggfawp_setStatesBtn		*
 * F. J. Yen/NCEP	04/08	Added minutes & most recent only flag to*
 *				nsf_plot and nsn_plot			*
 * L. Hinson/AWC        06/08   Add pgcycle_updateCycleTags             *
 ***********************************************************************/

#ifndef PROTO_NMAPLIB
#define PROTO_NMAPLIB

#include <Xm/XmAll.h>

/*
 *  ncw prototypes
 */
Widget  ncw_create    (       Widget  parent);

void    ncw_set       (       void );

void    ncw_unset     (       void );

void    ncw_sproj     (       const char* source );

Boolean ncw_isup     (        void );

/*
 *  ngd prototypes
 */
void	ngd_dspl (	char	*panel,
			char	*dattim,
			char	*alias,
			int	*isbcat,
			char	*cycle,
			char	*rstfil,
			char	*endtim,
			int	*mrange,
			int	*intrvl,
			int	*match,
			int	*minutes,
			int	*ititl,
			int	*iret,
			... );

void	ngd_gnms (	char	*alias,
			int	*nname,
			nmlst_t namarr[],
			int	*iret );

void	ngd_gtim (	int	indx,
			dattm_t	endtim,
			int	mrange,
			int	intrvl,
			Boolean	jflag,
			int	*idelrt,
			int	*ntime,
			dattm_t	timarr[],
			int	*iret );

void	ngd_init (	int	*iret );

void	ngd_nlst (	char	*alias,
			char	*namstr,
			int	*lenstr,
			int	*iret,
			size_t, size_t );

void	ngd_plot (	Pixmap	pixmap,
			int	indx,
			char	panel[],
			dattm_t	dattm,
			dattm_t	endtim,
			int	mrange,
			int	intrvl,
			int	match,
			int	minutes,
			int	ititl,
			int	*iret );

void	ngd_qatt (	int	indx,
			char	alias[],
			int	*isbcat,
			char	cycle[],
			char	rstfil[],
			int	*iret );

void	ngd_rest (	int	*iret );

void	ngd_satt (	int	iindex,
			char	alias[],
			int	isbcat,
			char	cycle[],
			char	rstfil[],
			int	*jindex,
			int	*iret );

void	ngd_save (	int	*iret );

void	ngd_tlst (	char	*alias,
			char	*cycle,
			char	*rstfil,
			int	*isbcat,
			char	*endtim,
			int	*mrange,
			int	*intrvl,
			int	*iflag,
			int	*idelrt,
			char	*timstr,
			int	*lenstr,
			int	*ntime,
			int	*iret,
			... );

/*
 *  nim prototypes
 */

void	nim_flnm (	const char	*imgtyp,
			const char	*imginf,
			char		*basetm,
			char		*endtim,
			int		mrange,
			int		intrvl,
			Boolean		bflag,
			Boolean		bauto,
			char		*path,
			char		***namarr,
			char		***timarr,
			int		*ntime,
			int		*iret );

void	nim_gfil (	int	indx,
			dttms_t	dattim,
			dttms_t	endtim,
			int	mrange,
			int	intrvl,
			int	match,
			int	minutes,
			char	*filnam,
			int	*iret );

void	nim_gtim (	int	indx,
			dttms_t	endtim,
			int	mrange,
			int	intrvl,
			Boolean	jflag,
			Boolean	jauto,
			dttms_t	lastim,
			int	*ntime,
			char	***timarr,
			int	*iret );

void	nim_init (	int	*iret );

void	nim_plot (	Pixmap	pixmap,
			int	indx,
			char	garea[],
			char	panel[],
			dttms_t	dttms,
			dttms_t	endtim,
			int	mrange,
			int	intrvl,
			int	match,
			int	minutes,
			int	ititl,
			int	*iret );

void	nim_qatt (	int	indx,
			char	imtype[],
			char	iminfo[],
			char	imlutf[],
			int	*iret );

void	nim_rest (	int	*iret );

void	nim_satt (	int	iindex,
			char	imtype[],
			char	iminfo[],
			char	imlutf[],
			int	*jindex,
			int	*iret );

void	nim_save (	int	*iret );

/*
 *	nmp prototypes
 */
void	nmp_dspl (	char	*panel, 
			int	*itype,
			char	*file,
			char	*attribs,
			int	*iret,
			int, int, int );

void	nmp_gmapnum (	int	*mapnum,
			int	*iret );

void	nmp_govlflg (	int	lp,
			Boolean  ovlflg[],
			int	*iret );

void	nmp_govlnum (	int	*ovlnum,
			int	*iret );

void	nmp_init (	int	*iret );

void	nmp_mkscl (	int	lp,
			int	ovl,
			char	*stn_str,
			int	*iret );

void	nmp_plot (	int	lp,
			Pixmap	pixmap,
			char	panel[],
			int	*iret );

void	nmp_rdeflts (	int	*iret );

void	nmp_restore (	int	*iret );

void	nmp_rstrproj (	int	lp,
			int	*iret );

void	nmp_save (	int	*iret );

void	nmp_setmap (	nmpstr_t map,
			int	lp,
			Boolean	allp,
			int	*iret );

void	nmp_simmap (	int	imgtyp,
			int	lp,
			Boolean	allp,
			int	*iret );

void	nmp_valid (	int	lp,
			nmpstr_t map,
			nmpstr_t garea[2],
			nmpstr_t proj,
			int	*iret );

/*
 *	nms prototypes
 */
void	nms_dspl (	char	*panel,
			char	*dattim,
			char	*alias,
			int	*isbcat,
			char	*filnam,
			int	*numc,
			int	*ionoff,
			int	*iclrs,
			int	*iclrs2,
			int	*numf,
			int	*iflgs,
			float	*fvalu,
			float	*fline,
			float	*fsym1,
			float	*sym2,
			float	*farrw,
			int	*ititl,
			int	*iret,
			... );

void	nms_plot (	Pixmap  pixmap,
			int     indx,
			char    panel[],
			dttms_t dttms,
			dttms_t endtim,
			int     mrange,
			int     match,
			int     minutes,
			int     ititl,
			int     *iret );

void	nms_rest (	int 	*iret	);

void	nms_save (	int	*iret	);

/*
 *  nsf prototypes
 */
void	nsf_dspl (	char	*panel,
			char	*dattim,
			char	*alias,
			int	*isbcat,
			char	*cycle,
			char	*parms,
			char	*color,
			char	*filter,
			char	*txtatt,
			char	*endtim,
			int	*mrange,
			int	*intrvl,
			int	*match,
			int	*ititl,
			int	*iret,
			... );

void	nsf_gtim (	int	indx,
			dttms_t	endtim,
			int	mrange,
			int	intrvl,
			Boolean	jflag,
			int	*idelrt,
			int	*ntime,
			dttms_t	timarr[],
			int	*iret );

void	nsf_init (	int	*iret );

void	nsf_plot (	Pixmap	pixmap,
			int	indx,
			char	panel[],
			dttms_t	dttms,
			dttms_t	endtim,
			int	mrange,
			int	intrvl,
			int	match,
			int	minutes,
			int	ititl,
			int	ibfr,
			int	mbfr,
			int	iaftr,
			int	maftr,
			int	mstrct,
			int	*iret );

void	nsf_qatt (	int	indx,
			char	alias[],
			int	*isbcat,
			char	cycle[],
			char	parms[],
			char	color[],
			char	filter[],
			char	txtatt[],
			int	*iret );

void	nsf_rest (	int	*iret );

void	nsf_satt (	int	iindex,
			char	alias[],
			int	isbcat,
			char	cycle[],
			char	parms[],
			char	color[],
			char	filter[],
			char	txtatt[],
			int	*jindex,
			int	*iret );

void	nsf_save (	int	*iret );

void	nsf_tlst (	char	*alias,
			char    *cycle,
			int     *isbcat,
			char    *endtim,
			int     *mrange,
			int     *intrvl,
			int	*iflag,
			int	*idelrt,
			char    *timstr,
			int     *lenstr,
			int     *ntime,
			int     *iret,
			... );

/*
 *  nsn prototypes
 */
void	nsn_dspl (	char	*panel,
			char	*dattim,
			char	*alias,
			int	*isbcat,
			char	*cycle,
			char	*parms,
			char	*color,
			char	*level,
			char	*vcord,
			char	*filter,
			char	*txtatt,
			char	*endtim,
			int	*mrange,
			int	*intrvl,
			int	*match,
			int	*minutes,
			int	*ititl,
			int	*iret,
			... );

void	nsn_gtim (	int	indx,
			dttms_t	endtim,
			int	mrange,
			int	intrvl,
			Boolean	jflag,
			int	*idelrt,
			int	*ntime,
			dttms_t	timarr[],
			int	*iret );

void	nsn_init (	int	*iret );

void	nsn_plot (	Pixmap	pixmap,
			int	indx,
			char	panel[],
			dttms_t	dttms,
			dttms_t	endtim,
			int	mrange,
			int	intrvl,
			int	match,
			int	minutes,
			int	ititl,
			int 	ibfr,
			int 	mbfr,
                	int 	iaftr,
                	int 	maftr,
                	int 	mstrct,
			int	*iret);

void	nsn_qatt (	int	indx,
			char	alias[],
			int	*isbcat,
			char	cycle[],
			char	parms[],
			char	color[],
			char	level[],
			char	vcord[],
			char	filter[],
			char	txtatt[],
			int	*iret);

void	nsn_rest (	int	*iret );

void	nsn_satt (	int	iindex,
			char	alias[],
			int	isbcat,
			char	cycle[],
			char	parms[],
			char	color[],
			char	level[],
			char	vcord[],
			char	filter[],
			char	txtatt[],
			int	*jindex,
			int	*iret );

void	nsn_save (	int	*iret );

void	nsn_tlst (	char	*alias,
			char	*cycle,
			int	*isbcat,
			char	*endtim,
			int	*mrange,
			int	*intrvl,
			int	*iflag,
			int	*idelrt,
			char	*timstr,
			int	*lenstr,
			int	*ntime,
			int	*iret,
			... );

/*
 *  pgen prototypes  *******************************************
 *
 *  pgactv functions  ----------------------------------------
 */
void	pgactv_addClosedPt (	void );
void	pgactv_addPts (		float		add_x[],
				float		add_y[],
				int		add_n,
				int		indx,
				int		*iret );
void	pgactv_clearActv (	void );
void	pgactv_deleteAll (	void );
void	pgactv_deletePt (	int		delpt );
void	pgactv_getDevPts (	int		*np,
				float		**dx,
				float		**dy );
int	pgactv_getElmLoc (	void );
int	pgactv_getNearPt (	void );
void	pgactv_modPt (		int		pt,
				float		new_x,
				float		new_y );
void	pgactv_setNearPt (	int		nearest_pt );

/*
 *  pgairmet functions  (airmet format) -----------------------
 */
void 	pgairmet_create ( 	Widget		 parent );

Boolean pgairmet_isUp ( 	void );

void 	pgairmet_popdown ( 	void );

void 	pgairmet_popup	( 	void );

/*
 *  pgairmetp functions  (airmet prime format) -----------------------
 */
void 	pgairmetp_create ( 	Widget		 parent );

Boolean pgairmetp_isUp ( 	void );

void 	pgairmetp_popdown ( 	void );

void 	pgairmetp_popup	( 	void );

/*
 *  pgasel functions  (area selection) -----------------------
 */
void pgasel_start ( void (*_selSingleFunc)(float lat, float lon),
		    void (*_selAreaFunc)( int np, const float flat[], const float flon[]),
		    void (*_selDoneFunc)(void) );

/*
 *  pfccfp functions  -------------------------------------------
 */
void	pgccfp_create (		Widget		parent ); 
void	pgccfp_getfname (	char		*fname,
				int		*iret );
Boolean pgccfp_isUp (		void );
void	pgccfp_popdown (	void );
void	pgccfp_popup ( 		void );
void	pgccfp_update (		char		*fname,
				int		*iret );
/*
 *  pgccfw functions  --------------------------------------------
 */
void	pgccfw_create (		Widget		parent );
Boolean pgccfw_isUp (		void );
void	pgccfw_popdown (	void );
void	pgccfw_saveNew (	int		np,
				float		*lats,
				float		*lons );

/*
 *   pgcirc functions  --------------------------------------------
 */
void	pgcirc_create (		Widget		parent );
void	pgcirc_getAttr (	int		*color,
				int		*width );
char	pgcirc_getGrptyp (	void );
Boolean pgcirc_getLabColorFlag (void );
Boolean pgcirc_getLabFlag (	void );
void	pgcirc_getLabValue (	char		*label );
void	pgcirc_getLocation (	float		*xx1, 
				float		*yy1,
				float		*xx2, 
				float		*yy2 );
Boolean pgcirc_isUp (		void );
void	pgcirc_popdown (	void );
void	pgcirc_popup (		int		subtyp,
				int		eltyp,
				int		show_width,
				int		show_ctl,
				XtCallbackProc	callback,
				char		*label_ptr );
void	pgcirc_saveAttr (	void );
void	pgcirc_setAttr (	int		color,
				int		width );
void	pgcirc_setLabFlag (	Boolean		lab_flag);
void	pgcirc_setLocation (	float		xx1, 
				float		yy1, 
				float		xx2, 
				float		yy2 );
void	pgcirc_updateGstCirc (	void );

/*
 *  pgcycle functions ---------------------------------------------
 */
void pgcycle_create ( 		Widget 		parent );
void pgcycle_popup ( 		void );
void pgcycle_popdown ( 		void );
void pgcycle_showCycleTime ( 	int 		*iret );
void pgcycle_removeCycleTime ( 	void );
Boolean pgcycle_useCycle ( 	void );
void pgcycle_getCycle ( 	char 		*day, 
				char 		*cycle, 
				int 		*iret );
int pgcycle_getIssue ( 		void );
void pgcycle_updateCycleTags (  void );

/*
 *  pgdel functions  -----------------------------------------------
 */
void	pgdel_deletEh (		Widget, XtPointer, XEvent*, Boolean* );
void	pgdel_deletAll (	int		*iret );
void	pgdel_keyboardDelete(	void );
void	pgdelobj_deletStart (	Widget		wid,
				int		class,
				int		obj );

/*
 *  pgdist functions  (distance options) -----------------------
 */
Widget 	pgdist_create ( 	Widget		parent );

void 	pgdist_popup  ( 	void );

void 	pgdist_popdown  ( 	void );

Boolean pgdist_isUp     ( 	void );

Boolean pgdist_isDplOn  ( 	void );

void    pgdist_start    (       int		event_x, 
				int		event_y );

void    pgdist_update   (       int		event_x, 
				int		event_y );

void    pgdist_stop     (       void );

void    pgdist_stopUpdate (     void );

/*
 *  pgedit functions  -----------------------------------------------
 */
void	pgedit_editCb (		Widget, XtPointer, XtPointer );
Boolean pgedit_isActive (	void );
void	pgedit_multiEditCb (	Widget, XtPointer, XtPointer );

/*
 *  pgevt functions  -------------------------------------------------
 */
int	pgevt_changeType (	int		obj );
void	pgevt_locateElmCb (	Widget, XtPointer, XEvent*, Boolean* );
void	pgevt_multiSelHdl (	Widget, XtPointer, XEvent*, Boolean* );
void	pgevt_radiusHdl (	Widget, XtPointer, XEvent*, Boolean* );
void	pgevt_rotateHdl (	Widget, XtPointer, XEvent*, Boolean* );
void	pgevt_selectElmEh (	Widget, XtPointer, XEvent*, Boolean* );
void	pgevt_selectHdl (	Widget, XtPointer, XEvent*, Boolean* );
void	pgevt_setWindPlActv (	char		status );
void	pgevt_ungroup (		void );
void	pgevt_unsetOper (	Boolean		reset );

/*
 *  pgextrap functions  -------------------------------------------- 
 */
void	pgextrap_create (	Widget		parent );
int	pgextrap_getMode (	void );
Boolean pgextrap_isUp (		void );
void	pgextrap_popdown (	void );
void	pgextrap_popup (	void );
void	pgextrap_start (	char		grptyp,
				int		grpnum );
/*
 *  pgfilterw functions  -----------------------------------------
 */
void	pgfilterw_create (	Widget		parent );
void	pgfilterw_getFirstTime( char		*selectedTime );
Boolean pgfilterw_isUp (	void );
void	pgfilterw_popdown (	void );
void	pgfilterw_popup (	void );
void	pgfilterw_stepOneDown (	void );
void	pgfilterw_stepOneUp (	void );
void	pgfilterw_turnOnTime (	const char	*newTime );

/*
 *  pgfilw functions  -----------------------------------------
 */
void	pgfilw_clearFileName (	void );
void	pgfilw_clearSaveFlag (	void );
void	pgfilw_create (		Widget		parent );
void	pgfilw_getFileName (	Boolean		path_flag,
				char		*file_name );
void	pgfilw_getPathName (	char		*path_name );
Boolean pgfilw_isFileSaved (	void );
Boolean pgfilw_isUp (		void );
void	pgfilw_openVGF (	Boolean		append,
				int		*iret );
void	pgfilw_popdown (	void );
void	pgfilw_popup (		int		func );
void	pgfilw_save (		void );
void	pgfilw_setFileName (	int		func_flag,
				char		*file_name );
void	pgfilw_showFileName (	void );
void	pgfilw_startSaveAll (	void );

/*
 *  pgfrom functions  -------------------------------------------
 */
void	pgfrom_create	(	Widget		parent );
int	pgfrom_getUndoStep(	void );
void    pgfrom_clearUndoStep(	void );
Boolean	pgfrom_isUp	(	void );
void	pgfrom_popdown	(	void );
void	pgfrom_selectElmEh(	Widget, XtPointer, XEvent*, Boolean* );
void	pgfrom_startSmear(	void );

/*
 *  pglpfw functions  -----------------------------------------
 */
void	pglpfw_create (		Widget		parent );
Boolean	pglpfw_isUp (		void );
void	pglpfw_loadLPF (	int		*iret );
void	pglpfw_popdown (	void );
void	pglpfw_popup (		void );
void	pglpfw_setName (	char*		name );
void	pglpfw_setPath (	char*		path );

/*
 *  pgfrtw functions  -------------------------------------------
 */
void	pgfrtw_create (		Widget		parent );
void	pgfrtw_getAttr (	char 		*fsmth,
				int		*fmajcolor,
				int		*fmincolor );
int	pgfrtw_getFrtColor (	void );
char	pgfrtw_getGrptyp (	void );
Boolean pgfrtw_getLabColorFlag (void );
Boolean pgfrtw_getLabFlag (	void );
void	pgfrtw_getLabValue (	char		*label );
Boolean pgfrtw_isUp (		void );
void	pgfrtw_parseFcode (	int		fcode,
				int		*f_type,
				int		*strength,
				int		*front_code );
void	pgfrtw_popdown (	void );
void	pgfrtw_popup ( 		int		frt_typ,
				int		show_strngth,
				int		show_size,
				int		show_ctl,
				XtCallbackProc	callback,
				char		*label_ptr,
				Boolean		init_grp );
void	pgfrtw_setAttr (	int		fcode,
				int		pipsz,
				char		csmth,
				int		maj_color,
				int		min_color );
void	pgfrtw_setLabFlag (	Boolean		lab_flag );
void	pgfrtw_setLabItems (	int		group_type );
void	pgfrtw_setmajColor (	void ); 
void	pgfrtw_setminColor (	void );
void	pgfrtw_updtGrpMenu (	int		indx );

/*
 *  pggfaw functions  ----------------------------------------
 */
void 	pggfaw_cmpSeverity ( 	char		*hazard,
				char		*descriptor,
				char		*value1,
				char		*value2,
				char		*sevValue,
				int		*iret );
void	pggfaw_combineSigStrs(  const char	*str1,
				const char	*str2,
				int		maxOutStr,
				char		*outStr,
				int		*iret );
void 	pggfaw_create ( 	Widget		parent );
int 	pggfaw_getHazardType ( 	char 		hazard[] );
Boolean pggfaw_getFzlRangesFromFile (
	                        char            vgfile[],
				char            ranges[] );
Boolean pggfaw_isAddMode ( 	void );
Boolean pggfaw_isClosed ( 	void );
Boolean pggfaw_isTxtActive ( 	void );
Boolean pggfaw_isUp ( 		void );
void 	pggfaw_makeSubtype ( 	int 		hazType, 
				int 		catType, 
				int 		*subtype, 
				int 		*iret );
void 	pggfaw_popdown ( 	void );
void	pggfaw_saveNew (	int		np,
				float		*lats,
				float		*lons );
void 	pggfaw_setFrom ( 	int		np,
				float		*lat,
				float		*lon );
void 	pggfaw_setHour ( 	const char	*newHour );
void 	pggfaw_setType ( 	const char	*newType );
Boolean pggfaw_okToDraw(	Boolean );
void    pggfaw_setOkOnWarning( 	Boolean value );
void 	pggfaw_setCancelOnWarning( Boolean value );
Boolean pggfaw_getOkOnWarning(	void );
Boolean pggfaw_getCancelOnWarning( void ) ;
void	pggfaw_reorderGFA( 	int 		subtype, 
				int 		npts, 
				float 		*lat, 
				float 		*lon,
				int 		*iret );

/*
 *  pggfawp functions  ----------------------------------------
 */
void 	pggfawp_cmpSeverity ( 	char		*hazard,
				char		*descriptor,
				char		*value1,
				char		*value2,
				char		*sevValue,
				int		*iret );
void	pggfawp_combineSigStrs( const char	*str1,
				const char	*str2,
				int		maxOutStr,
				char		*outStr,
				int		*iret );
void 	pggfawp_create ( 	Widget		parent );
int 	pggfawp_getHazardType ( char 		hazard[] );
Boolean pggfawp_getFzlRangesFromFile (
	                        char            vgfile[],
				char            ranges[] );
Boolean pggfawp_isAddMode ( 	void );
Boolean pggfawp_isClosed ( 	void );
Boolean pggfawp_isTxtActive ( 	void );
Boolean pggfawp_isUp ( 		void );
void 	pggfawp_makeSubtype ( 	int 		hazType, 
				int 		catType, 
				int 		*subtype, 
				int 		*iret );
void 	pggfawp_getHazCat ( 	int 		subtype, 
				int 		*hazType, 
				int 		*catType, 
				int 		*iret );
void 	pggfawp_popdown ( 	void );
void	pggfawp_saveNew (	int		np,
				float		*lats,
				float		*lons );
void 	pggfawp_setFrom ( 	int		np,
				float		*lat,
				float		*lon );
void 	pggfawp_setHour ( 	const char	*newHour );
void 	pggfawp_setStatesBtn( 	Boolean		flag	 );
void 	pggfawp_setType ( 	const char	*newType );
Boolean pggfawp_okToDraw(	Boolean );
void    pggfawp_setOkOnWarning( Boolean value );
void 	pggfawp_setCancelOnWarning( Boolean value );
Boolean pggfawp_getOkOnWarning(	void );
Boolean pggfawp_getCancelOnWarning( void ) ;
void	pggfawp_reorderGFA( 	int 		subtype, 
				int 		npts, 
				float 		*lat, 
				float 		*lon,
				int 		*iret );

/*
 *  pggfmt functions  ------------------------------------------
 */
Widget	pggfmt_create (		Widget 		parent );
Boolean pggfmt_isUp (		void );
void	pggfmt_popdown (	void );
void	pggfmt_popup (		void );
void    pggfmt_GetTxtW (        char            *parm, 
                                char            *value, 
				int		*iret );
/*
 * pggrpch functions ---------------------------------------------
 */
void	pggrpch_create (	Widget		parent );
Boolean pggrpch_isUp (		void ); 
void	pggrpch_popdown (	void ); 
void	pggrpch_popup (		void );

/*
 *  pggrpw functions  --------------------------------------------
 */
Widget	pggrpw_create (		Widget		parent );
Boolean pggrpw_getEmptFlg (	void );
int	pggrpw_getGrpNum (	void );
int	pggrpw_getGrpType (	void );
int	pggrpw_grpCount (	void );
Boolean pggrpw_isUp (		void );
void	pggrpw_popdown (	void );
void	pggrpw_popup (		void );
void	pggrpw_setEmptFlg (	char		val );
void	pggrpw_startGrp (	void );

/*
 *  pggst functions  ----------------------------------------
 */
void	pggst_addGhostPts (	int		np,
				float		*xx,
				float		*yy,
				int		*iret );
void	pggst_clearGhost (	Boolean		reset_attr );
void	pggst_cursorGhost (	float		*xx,
				float		*yy,
				int             *iret );
void	pggst_disableGhost (	void );
void	pggst_drawGhost (	char		ghost_typ );
void	pggst_enableGhost (	void );
void	pggst_initGc (		void );
void	pggst_redrawGhost (	void );
void	pggst_replaceGhostPts (	int		np,
				float		*xx, 
				float		*yy,
				int		*iret );
void	pggst_setLineAttr (	char		smooth,
				char	 	closed );
void	pggst_tempLine (	int		np,
				float		rx[],
				float		ry[] );
void	pggst_touchGhost (	void );
void	pggst_veilGhost (	Boolean		veil_on );

/*
 *  pghdlb functions  -----------------------------------------
 */
void	pghdlb_deselectAll (	void );
void	pghdlb_deselectEl (	int		el_num,
				char		updt_dsply );
void	pghdlb_deselectGrp (	char		grptyp,
				int		grpnum );
void	pghdlb_displayAllSel (	void );
int	pghdlb_elemSelected (	void );
void	pghdlb_getNextIndex (	int		cur_index,
				int		*sel_el,
				int		*sel_loc,
				int		*iret );
void	pghdlb_grpAllSel (	char		grptyp,
				int		grpnum );
void	pghdlb_selectAll (	int		sflag );
void	pghdlb_selectGrp (	char		grptyp,
				int		grpnum   );
void	pghdlb_setSelect (	int		location );
void	pghdlb_showAllSel (	void );

/*
 *  pginterp functions  -------------------------------------------
 */
void 	pginterp_create ( 	Widget		parent );
Boolean pginterp_isUp ( 	void );
void 	pginterp_popdown ( 	void );
void 	pginterp_popup ( 	void );
void	pginterp_selectElmEh(	Widget, XtPointer, XEvent*, Boolean* );
void	pginterp_startInterp(	 void);
Boolean pginterp_toBeContinued ( void);
void	pginterp_resumeOper (	 void);

/*
 *  pglabel functions  -------------------------------------------
 */
Boolean pglabel_getLabelPending ( void );
Boolean pglabel_getLabFlag (	void );
int	pglabel_getLabType (	void );
int	pglabel_getOldObject (	void );
Boolean pglabel_ifpopup (	void );
void	pglabel_popupOldWin (	void );
void	pglabel_saveOldInfor (	int		class,
				int		object );
void	pglabel_setLabelPending ( Boolean	flag );
void	pglabel_setLabFlag (	Boolean		lab_flag );
void	pglabel_setLabType (	int		lab_type );
void	pglabel_symbpopup (	void );
void	pglabel_txtpopup (	void );

/*
 *  pgjet functions  ---------------------------------------------
 */
Widget	pgjet_create (		Widget		parent );
void	pgjet_getAttr (		int		*splcol,
				int		*splwdth,
				char		*smth,
				int		*brbcol,
				int		*brbwdth,
				float		*brbsiz,
				int		*hshcol,
				int		*hshwdth,
				float		*hshsiz );
int	pgjet_getCurAction (	void );	
int	pgjet_getCurSubtyp (	void );	
int	pgjet_getSelectedSub (	void );	
Boolean pgjet_isUp (		void );
void	pgjet_popdown (		void );
void	pgjet_popdownAttr (	void );
void	pgjet_popup (		XtCallbackProc	callback );	
void	pgjet_popupAttr (	void );	

/*
 *  pglayer functions  -------------------------------------------
 */
void	pglayer_init (		void );
int	pglayer_getChngLayer (	int		st_layer );
int	pglayer_getCurLayer (	void );
int	pglayer_getDefGrp (	int		layer_index );
void	pglayer_getDirPos (	int		layer,
				int		*dir_pos );
Boolean pglayer_getDsplClr (	int		layer_index );
Boolean pglayer_getDsplOn (	int		layer_index );
void	pglayer_getFileName (	int		layer,
				char		*file_name );
void	pglayer_getFilePath (	int		layer,
				char		*file_path );
Boolean pglayer_getFill (	int		layer_index );
Boolean pglayer_getInUse (	int		layer_index );
int	pglayer_getMonoClr (	int		layer_index );
char	*pglayer_getName (	int		layer_index );
int	pglayer_getNumLayers (	void );
Boolean pglayer_isChngMade (	int		layer );
Boolean pglayer_isFileSaved (	int		layer );
void	pglayer_setChngMade (	int		layer,
				Boolean		changes_made );
void	pglayer_setCurLayer (	int		layer );
void	pglayer_setDefGrp (	int		layer_index, 
				int		value );
void	pglayer_setDirPos (	int		layer,
				int		dir_pos );
void	pglayer_setDsplClr (	int		layer_index,
				Boolean		dsply_all_color );
void	pglayer_setDsplOn (	int		layer_index,
				Boolean		dsply_on );
void	pglayer_setFileName (	int		layer,
				char		*file_name );
void	pglayer_setFilePath (	int		layer,
				char		*file_path );
void	pglayer_setFileSaved (	int		layer,
				Boolean		file_saved );
void	pglayer_setFill (	int		layer_index,
				Boolean		fill );
void	pglayer_setInUse (	int		layer_index,
				Boolean		active );
void	pglayer_setMonoClr (	int		layer_index, 
				int		value );
void	pglayer_setName (	int		layer_index,
				char		*name );

/*
 *  pglayred functions  -----------------------------------------------
 */
Widget	pglayred_create (	Widget 		parent );
Boolean pglayred_isUp (		void ); 
void	pglayred_popdown (	void );
void	pglayred_popup (	int             layer );

/*
 *  pglayrnm functions  -----------------------------------------------
 */
Widget	pglayrnm_create (	Widget 		parent );
Boolean pglayrnm_isUp (		void ); 
void	pglayrnm_popdown (	void );
void	pglayrnm_popup (	int             layer );

/*
 *  pglayrw functions  -----------------------------------------------
 */
void	pglayrw_addLayer (	void );
Widget	pglayrw_create (	Widget		parent );
void	pglayrw_exit (		void );
void	pglayrw_getLayerName (	char		*layerName );
Boolean pglayrw_isUp (		void ); 
void	pglayrw_manageLayers (	void );
void	pglayrw_matchLayer (	const char	*newLayer );
void	pglayrw_nameCb (	Widget, long, XtPointer );
void	pglayrw_popdown (	void );
void	pglayrw_popup (		void );
void	pglayrw_refresh (	void );
void	pglayrw_setActvLayer (	long newLayer, int *iret );
void 	pglayrw_setLayer ( 	Widget, XEvent*, String*, Cardinal* );
void	pglayrw_updtSettings (	long		layer );

/*
 *  pglayrxt functions  -----------------------------------------------
 */
Widget	pglayrxt_create (	Widget		parent );
void	pglayrxt_exitAll (	void ); 
Boolean pglayrxt_isUp (		void ); 
void	pglayrxt_popdown (	void );
void	pglayrxt_popup (	void );
void	pglayrxt_setExitMsg (	void );

/*
 *  pgline functions  -------------------------------------------- 
 */
void	pgline_create ( 	Widget		parent );
void	pgline_getAttr ( 	signed char	*fill,
				char		*clos,
				int		*color,
				int		*width,
				float		*size,
				int		*kpos,
				char		*smooth,
				char		*grptyp );
void	pgline_getAttrStr (	char            *info  );
void	pgline_getColor (	int 		*colr );
Boolean pgline_getLabFlag (	void );
Boolean pgline_getLabColorFlag (void );
int	pgline_getLabType (	void );
void	pgline_getLabValue (	char		*label );
char	pgline_getGrptyp (	void );
void	pgline_getSymbInfo (	int 		*symb_cls,
				int		*symb_id );
Boolean pgline_isUp ( 		void );
void 	pgline_popdown ( 	void );
void 	pgline_popup ( 		int		subtyp,
				int		eltyp,
				int		show_width,
				int		show_size,
				int		show_kpos,
				int		show_ctl,
				XtCallbackProc	callback,
				char		*label_ptr,
				int		grptyp,
				Boolean		init_grp );	
void	pgline_saveAttr(	void );
void 	pgline_setAttr (	char		fill,
				char		clos,
				int		color,
				int		width,
				float		size,
				int		kpos,
				char		smooth );
void	pgline_setAttrStr (	char		*info );
void	pgline_setLabFlag (	Boolean		lab_flag );
void	pgline_setLabItems (	int		group_type );
void	pgline_updtGrpMenu (	int		indx	);

/*
 *  pglist functions  -------------------------------------------- 
 */
void	pglist_create (		Widget		parent );
void	pglist_getAttr ( 	int		*mrktyp,
				int		*mrkclr,
				float		*mrksiz,
				int		*mrkwid );
int	pglist_getMode (	void );
void 	pglist_grpToWtch (	int		list_offset );
Boolean pglist_isUp (		void );
void	pglist_popdown (	void );
void	pglist_popup (		int		lsttyp,
				int		show_ctl,
				XtCallbackProc  callback );	
void	pglist_saveAttr (	void );
void	pglist_setAttr (	int		mrktyp,
				int		mrkclr,
				float		mrksiz,
				int		mrkwid );
void	pglist_updtStatusCntys (int		watch_offset );
void	pglist_wrtLstWtch (	char		*fname,
				int		*location );

/*
 *  pgmdfy functions  ------------------------------------------
 */
void	pgmdfy_modifyStartEh (	Widget, XtPointer, XEvent*, Boolean* );

/*
 *  pgmodv functions  ------------------------------------------
 */
void	pgmodv_start (		int		funcflg,
				int		smooth_lvl,
				char		closed );

/*
 *  pgmsel functions  ------------------------------------------
 */
void	pgmsel_endMultiSel(	void );
void	pgmsel_multiPtSel(	int		np,
				const float	xx[],
				const float	yy[] );
void	pgmsel_singlePtSel(	float		xx,
				float		yy );

/*
 *  pgmvcp functions  ------------------------------------------
 */
void	pgmvcp_start (		char		grptyp,
				int		grpnum,
				char		closed,
				float		xx,
				float		yy,
				int		smooth );

/*
 *  pgnew functions  ----------------------------------------------
 */
void	pgnew_dummyDrop (	Widget, XtPointer, XEvent*, Boolean* );
void	pgnew_getGrpInfo (	char		*grptyp,
				int 		*grpnum );
void	pgnew_resetMouse (	Widget, XtPointer, XEvent*, Boolean* );
void	pgnew_setArmDynamic (	void );
void	pgnew_setGrpInfo (	char		grptyp, 
				int		grpnum );
void	pgnew_setMultiPt (	Boolean		mp_state );
void	pgnew_setWatchLn (	Boolean		wl_state );

/*
 *  pgnumb functions  -----------------------------------------------
 */
void	pgnumb_create ( 	Widget  	parent_w);
void	pgnumb_getNumb ( 	int		*new_numb );
Boolean pgnumb_isUp ( 		void );
void	pgnumb_popdown ( 	void );
void	pgnumb_popup ( 		void );
void	pgnumb_setNumb ( 	int		new_numb );
void	pgnumb_updateBtns ( 	Boolean	    	btn_sensitive);

/*
 *  pgobj functions  ------------------------------------------------
 */
void	pgobj_getId (  		int		class_id,
				int		obj_id,
				int		*elm_id,
				int		*gem_typ,
				int		*sub_typ ); 

/*
 *  pgofmt functions  -----------------------------------------------
 */
Widget	pgofmt_create ( 	Widget 		parent );
Boolean pgofmt_isUp ( 		void ); 
void	pgofmt_popdown (	void );
void	pgofmt_popup (		void );
void	pgofmt_update ( 	int     	*iret );
Boolean pgofmt_bulkProcessOn  ( void ); 
void    pgofmt_bulkProcessNext( void ); 
void	pgofmt_bulkProcessEnd ( void );

/*
 *  pgolk functions  --------------------------------------------------
 */
void	pgolk_check ( 		int		maxlen,
				char		*msgs,
				int		*iret );
void	pgolk_getfname ( 	char		*text,
				char		*fname,
				int		*iret );
void	pgolk_getGroup (	char		*grptype_exp,
				char		grpname_exp[] );
void	pgolk_update ( 		char		*fname,
				int		*iret );

/*
 *  pgpalw functions  -------------------------------------------------
 */
void	pgpalw_actvGrp (	void );
void	pgpalw_actvLayer (      Boolean		popup );
void	pgpalw_classPopdown (	void );
void	pgpalw_create ( 	int 		*iret );
void	pgpalw_deleteAll (	void );
void	pgpalw_dsplyObjPal ( 	char		vg_class );
void	pgpalw_exit (		Widget, XtPointer, XtPointer );
void	pgpalw_exitCheck ( 	Widget 		w );
int	pgpalw_getCurClassId (	void );
char	*pgpalw_getCurClassName(void );
int	pgpalw_getCurObjId (	void );
int	pgpalw_getCurOperId (	void );
char	*pgpalw_getCurOperName (void );
char	pgpalw_getMode (	void );
int	pgpalw_getObjID (	char		*obj_name );
Widget	pgpalw_getOperWid (	int		id );
Widget	pgpalw_getPrevOperWid ( void );
Widget	pgpalw_getObjWid (	int		id );
void	pgpalw_inactvGrp (      void );
void	pgpalw_inactvLayer(     void );
Boolean pgpalw_isExitPGEN (	void );
Boolean pgpalw_isGrpActv (      void );
Boolean pgpalw_isLayerActv (	void );
Boolean pgpalw_isUp (		void );
void    pgpalw_objCb  (		Widget, XtPointer, XtPointer );
void	pgpalw_operCb (		Widget, XtPointer, XtPointer );
void	pgpalw_popdown (	void );
void	pgpalw_popup (		void );
void    pgpalw_refresh (        void );
void	pgpalw_rfrshObjPal (	void );
void	pgpalw_rmDefVGF (	void );
void	pgpalw_setBtnSntv ( 	int     	btn_id, 
				Boolean	 	btn_snstv );
void	pgpalw_setClassMet(	void );
void 	pgpalw_setCurBtns ( 	int		oper,
				int		class,
				int		obj );
void	pgpalw_setCurObj ( 	Widget          wid );
void	pgpalw_setCurOper ( 	Widget          wid );
void	pgpalw_setExitPGEN (	Boolean		flag );
void	pgpalw_setPrevOper (	void );
void	pgpalw_setupOper (	void );
void	pgpalw_unmanageObjPal ( void );
Boolean pgpalw_isGFAp_Actv (	void );

/*
 *  pgprd functions  -------------------------------------------
 */
void	pgprd_clear ( 		void );
Widget	pgprd_create (		Widget		parent );
Widget	pgprd_createPopSv (	Widget		parent );
Boolean pgprd_isUp (		void );
void	pgprd_popdown (		void ); 
void	pgprd_popup (		void );
void	pgprd_putstr (		char		*str,
				int		*iret );
void	pgprd_update (		void );
int	pgprd_getTxtSiz (	void );

/*
 *  pgqpf functions  -------------------------------------------
 */
void	pgqpf_update (		char		*fname,
				int		*iret );

/*
 *  pgrad functions  -------------------------------------------
 */
void	pgrad_radiusStart (	float		x,
				float		y );
void	pgrad_setCircPlActv (	Boolean		status );

/*
 *  pgrot functions  ------------------------------------------
 */
void	pgrot_rotateStart (	float		x,
				float		y );

/*
 *  pgsfp functions  ------------------------------------------
 */
void	pgsfp_getfname (	char		*fname,
				int		*iret );
void	pgsfp_udlist ( 		float		*latlon,
				int		npts,
				int		pipdir,
				int		*iret ); 
void	pgsfp_update (		char		*fname,
				int		*iret );
char*   pgsfp_getNewFormatTxt ( void );

/*
 *  pgsigw functions  ----------------------------------------
 */
void	pgsigw_create (		Widget		parent );
void	pgsigw_getStrings (	int		area_num,
				int		ids_num,
				char		*area_str,
				char		*id_str );
int	pgsigw_getSubType (	void );
Boolean pgsigw_isUp (		void );
void	pgsigw_popdown (	void );
void	pgsigw_setFrom (	int		np,
				float		*lat,
				float		*lon );

/*
 *  pgsmear functions  -------------------------------------------
 */
void	pgsmear_create	(	Widget		parent );
Boolean	pgsmear_isUp	(	void );
void	pgsmear_popdown	(	void );
void	pgsmear_selectElmEh(	Widget, XtPointer, XEvent*, Boolean* );
void	pgsmear_startSmear(	void );

/*
 *  pgsymb functions  -------------------------------------------
 */
void	pgsymb_create (		Widget		parent );
void	pgsymb_createVolLst (	Widget 		parent );
void	pgsymb_getAttrStr (	char		*info );
void	pgsymb_getColor (	int		*colr );
void	pgsymb_getSize (	float		*size );
void	pgsymb_getWidth (	int		*width );
Boolean pgsymb_getLabColorFlag (void );
Boolean pgsymb_getLabFlag (	void );
void	pgsymb_getLabValue (	char		*label );
void	pgsymb_getXxYyCoord (	float		*xx, 
				float		*yy );
char	pgsymb_getGrptyp (	void );
int	pgsymb_getObjId	(	void );
Boolean pgsymb_isUp (		void );
void	pgsymb_popdown (	void );
void	pgsymb_popup (		int		subtyp,
				int		elmid,
				int		show_ctl,
				XtCallbackProc  callback,
				char		*label_ptr,
				Boolean         init_grp );
void	pgsymb_saveAttr (	void );
void	pgsymb_setAttr (	int		colr,
				float		size,
				int		width );
void	pgsymb_setLabFlag (	Boolean		lab_flag );
void	pgsymb_setLabItems (	int		group_type );
void	pgsymb_setLatLon (	float		latitude, 
				float		longitude );
void	pgsymb_setUnRedo (	Boolean		un_redo );
void	pgsymb_updtGrpMenu (	int		indx );
void	pgsymb_unmanageLabForm (void );

/*
 *  pgtrk functions  ---------------------------------------------
 */
void	pgtrk_create (		Widget		parent );
void	pgtrk_getfname (	char		*text, 
				char		*fname, 
				int		*iret );
Boolean	pgtrk_isUp (		void );
void	pgtrk_popdown (		void );
void	pgtrk_popup (		void );
void	pgtrk_update(		char		*fname,
				int		*iret );

/*
 *  pgtrkw functions  --------------------------------------------
 */
void	pgtrkw_create (		Widget		parent );
void	pgtrkw_createResults (	Widget		parent );
void	pgtrkw_getInterv (	char		*text );
int	pgtrkw_getNumExtra (	void );
Boolean pgtrkw_isUp (		void );
void	pgtrkw_popdown (	void );
void	pgtrkw_popup (		int		vgtype,
				Boolean		show_ctl,
				XtCallbackProc	callback );
void	pgtrkw_resultsPopup (	void );
void	pgtrkw_setFrmTime (	Boolean		first_point );
Boolean pgtrkw_validateTimes (	void );

/*
 *  pgtca functions  ------------------------------------------------
 */
void	pgtca_create (		Widget	parent_w );
void	pgtca_popdown (		void );
Boolean pgtca_isNewSeg (	void );
Boolean pgtca_isUp ( 		void );
void 	pgtca_stepList ( 	Arrow_Dir_t 	direction, 
				int 		*iret );

/*
 *  pgtxt functions  ------------------------------------------------
 */
void	pgtxt_create (		Widget	parent_w );
void	pgtxt_decodeFont (	int		font_val,
				int		font_class,
				int		*font_type,
				int		*font_style );
void	pgtxt_getEditFlag (	Boolean		edit_flags[11] );
void	pgtxt_getRotnType (	int		*rotatype );
void	pgtxt_getSelMcloud (	int		*multi_sel,
				int		*mixed_sel );
void	pgtxt_getTextValue (	int		warnflag,
				char 		*text );
Widget 	pgtxt_getTxtW (		void );
Boolean pgtxt_isUp (		void );
void	pgtxt_popdown (		void );
void	pgtxt_popup (		void );
void	pgtxt_resetFocus (	void );
void	pgtxt_setEditFlag ( 	int		which_flag,
				Boolean		flag_valu);
void	pgtxt_setLabelValue (	char		*text );
void	pgtxt_setRotn (		int		new_dir );
void	pgtxt_updateGstTxt (	void );

/*
 *  pgundo functions  -----------------------------------------------
 */
void	pgundo_endStep (	void );
int	pgundo_getCurStep (	void );
void	pgundo_initUndo (	void );
void	pgundo_newStep (	void );
void	pgundo_redo (		void );
Boolean	pgundo_redoIsActv(	void );
void	pgundo_storeThisLoc (	int		loc,
				int		purpose,
				int		*iret );
void	pgundo_undo (		void );
Boolean	pgundo_undoIsActv(	void );

/*
 *  pgutls functions  ------------------------------------------------
 */
void	pgutls_createOptionMenu(Widget		parent,
				int		nbuttons,
				XtPointer	pstart_item,
				String		label_str,
				XtCallbackProc  callback,
				Widget		*form,
				Widget		*label,
				Widget		*menu,
				Widget		pbs[],
				char		*btnstrs[] );
void	pgutls_fReverseArray (	int		np,
				float		*farray_x,
				float		*farray_y );
void	pgutls_getSmoothPts (	int		np,
				float		*x_pts,
				float		*y_pts,
				int		smooth_lvl,
				int		max_pts,
				int		i_trnc,
				int		f_trnc,
				int		*smooth_pts,
				float		*smooth_x, 
				float		*smooth_y, 
				int		*iret );
void	pgutls_getSmoothedPt (	float		xx, 
				float		yy, 
				int		start,
				int		end,
				Boolean		closed,
				int		smooth_lvl,
				float		*smooth_x, 
				float		*smooth_y,
				int		*iret );
Boolean pgutls_isComsym (	void );
Boolean pgutls_isNumber (	char		*string );	
int	pgutls_nextIndex (	int		num_index,
				int		cur_index,
				char		direction );
void	pgutls_optPbCb (	Widget, long, XtPointer );
void	pgutls_refresh (	float		llx,
				float		lly,
				float		urx,
				float		ury,
				int		*iret );
int	pgutls_regroup (	char		frm_grptyp,
				int		frm_grpnum,
				char		to_grptyp,
				int		to_grpnum,
				int		*iret );
void    pgutls_vrfyLmtPunctCb ( Widget, XtPointer, XtPointer );
void	pgutls_vrfyPosFltCb (	Widget, XtPointer, XtPointer );
void	pgutls_vrfyPosIntBlkCb (Widget, XtPointer, XtPointer );
void	pgutls_vrfyPosIntCb ( 	Widget, XtPointer, XtPointer );
void 	pgutls_vrfyTimeCb ( 	Widget, XtPointer, XtPointer );
void 	pgutls_vrfyUpperCaseCb( Widget, XtPointer, XtPointer ); 
void 	pgutls_vrfyNoPunctCb(   Widget, XtPointer, XtPointer ); 
void 	pgutls_vrfyNoDigitsCb(  Widget, XtPointer, XtPointer ); 

/*
 *  pgvacw functions  ---------------------------------------
 */
void	pgvacw_create (		Widget 		parent );
char	pgvacw_getGrptyp (	void ); 
int	pgvacw_getSubType (	void );
Boolean pgvacw_isUp (		void ); 
void	pgvacw_popdown (	void ); 

/*
 *  pgvgf functions  ---------------------------------------
 */
void	pgvgf_dsp (		int		location,
				int		*iret );
void	pgvgf_save (		char		grptyp,
				int		grpnum,
				int		np,
				float		lat[],
				float		lon[],
				int		*location,
				int		*iret );

/*
 *  pgvolw functions  ---------------------------------------
 */
Widget	pgvolw_create (		Widget 		parent );
Widget	pgvolw_editCreate (	Widget		parent );
Boolean pgvolw_editIsUp (	void );
void	pgvolw_editPopdown (	void ); 
char	pgvolw_getGrptyp (	void );
Boolean pgvolw_isUp (		void ); 
void	pgvolw_popdown (	void ); 
void	pgvolw_popup (		void );
int	pgvolw_getFcstNum (	void );
char*** pgvolw_getFcstInfo (	void );  
void    pgvolw_getProdInfo (    char*		volnm, 
				int*		latest_y, 
				int*		latest_n, 
				int*		iret );
void    pgvolw_rdWords ( const char*, const char*, char**, int*);

/*
 *  pgwatch functions  -------------------------------------
 */

void	pgwatch_clrcnty (	void );
void	pgwatch_clrNumCwas (	void );
void	pgwatch_gattr (		int		*hwsm,
				int		*hwnm,
				int		*shape,
				int		*area );
void	pgwatch_gcnty (		int		n,
				int		*nc,
				char		*str );
void	pgwatch_gQCcnty (	int		*n_inactv, 
				char		***inactv_cnty_out, 
				int		*n_actv,   
				char		***actv_cnty_out );
void	pgwatch_gstate (	char		*str );
void	pgwatch_gvert (		int		nv,
				char		*str );
void    pgwatch_loadStCwa (     void );
void 	pgwatch_numsort ( 	int 		*nin, 
				int 		*inpint, 
				int 		*nout, 
				int 		*outnum, 
				int 		*iret);
void	pgwatch_redocnty (	void );
int	pgwatch_restore (	void );

/*
 *  pgwbxw functions  --------------------------------------
 */
void	pgwbxw_addWtchLn (	int		line_offset );
Widget 	pgwbxw_create (		Widget		parent );
void	pgwbxw_getAnchor (	int		which,
				char		*id,
				float		*flat,
				float		*flon,
				int		*dis,
				char		*dir,
				int		*iret );
void	pgwbxw_getAttr (	int		*wcolr,
				int 		*wstyle,
				int 		*wshape,
				int		*mtype,
				float		*msize,
				int		*mwidth,
				signed char	*cnty_fill,
				int		*cnty_colr    );
int	pgwbxw_getWtchOffset (	void );
Boolean	pgwbxw_isUp (		void );
void	pgwbxw_popdown (	void ); 
void	pgwbxw_popup ( 		int		style,
				int		color,
				int		shape,
				int		mtype,
				float		msize,
				int		mwidth,
				signed char	cnty_fill,
				int		cnty_colr,
				int		show_ctl,
				XtCallbackProc  callback );
void	pgwbxw_setAnchor (	int		which,
				char		*id,
				float		flat,
				float		flon,
				int		dis,
				char		*dir,
				int		*iret  );
void	pgwbxw_setAttr (	int		wcolr,
				int		wstyle,
				int		wshape,
				int		mtype,
				float		msize,
				int		mwidth,
				signed char	cnty_fill,
				int		cnty_colr    );
void	pgwbxw_setDspLabel (	int		label_number );
void	pgwbxw_setUpWtchLn (	int		watch_offset );
void	pgwbxw_setWlst (	int		location,
				Boolean		selectit );
void	pgwbxw_setWtchOffset (	int		file_location );
void	pgwbxw_waitEh (		Widget, XtPointer, XEvent*, Boolean* );

void    pgwbxw_updtWcnFlag (    int             watch_offset );

/*
 *  pgwcnsl functions  -------------------------------------
 */
void	pgwcnsl_create (	Widget		parent );
Boolean pgwcnsl_isUp (		void );
void	pgwcnsl_popdown (	void );
void	pgwcnsl_popup (		void );

/*
 *  pgwfmt functions  ---------------------------------------
 */
Widget	pgwfmt_create (		Widget 		parent );
Widget	pgwfmt_createWCC (	Widget		parent );
void	pgwfmt_createWCL (	Widget 		parent );
void	pgwfmt_formatSave (	Boolean		firstime ); 
void	pgwfmt_getcontWtch (	char		**cntstr );
void	pgwfmt_getfname (	char		*fname,
				int		*iret );
void	pgwfmt_getIssTmStr (	char		*time_str);
Boolean pgwfmt_isUp (		void ); 
void	pgwfmt_popdown (	void ); 
void	pgwfmt_popdownWCC (	void );
void	pgwfmt_popdownWCL (	void );
void	pgwfmt_popup (		void );
void	pgwfmt_popupWCC (	void );
void	pgwfmt_selectEh (	Widget, XtPointer, XEvent*, Boolean* );
void	pgwfmt_update (		int		*iret );
void	pgwfmt_WCCselectEh (	Widget, XtPointer, XEvent*, Boolean* );

/*
 *  pgwlst functions  -----------------------------------------
 */
void	pgwlst_clear ( 		void );
void	pgwlst_cmOptCb (	Widget, long, XtPointer );
Widget	pgwlst_create (		Widget		parent );
int	pgwlst_getClstStatus (	void );
Boolean pgwlst_getCtlkStatus(	void );
void	pgwlst_getInaAncPts (	char		*ina_ancpts );
Widget  pgwlst_getStatesForm(   void );
Widget  pgwlst_getInoutRC   (   void );
void	pgwlst_getWFO (		char		*wfolist );
Boolean pgwlst_isUp (		void ); 
void	pgwlst_popdown (	void ); 
void	pgwlst_popup (		Boolean		reset );
void	pgwlst_popupWarn (	void );
void	pgwlst_setBtnSen (	Boolean		stensitive );
void	pgwlst_setPanesSen (	Boolean		sensitive );
void	pgwlst_setShowCnty (	Boolean		what );
void	pgwlst_setShowFlg  (	Boolean		what );
void	pgwlst_setShowInfo (	Boolean		what );
void	pgwlst_setShowQC   (	Boolean		what );
void	pgwlst_updVorLst (	char		*sysin,
				int		np,
				float		*x,
				float		*y,
				int		iv,
				char		*disdir,
				char		*stn,
				int		*iret );

/*
 *  pgwndw functions  ------------------------------------------
 */
void	pgwndw_create (		Widget		parent );
void	pgwndw_getData (	float		*dir,
				float		*speed,
				int		*color,
				int		*increment);
void 	pgwndw_getEditFlag ( 	Boolean 	edit_flags[6] );
Boolean pgwndw_isUp (		void );
void	pgwndw_popdown (	void );
void	pgwndw_popup (		int		wind_type,
				int		show_ctl,
				XtCallbackProc	callback );
void	pgwndw_saveColor (	void );
void	pgwndw_setAttr (	float		dir,
				float		speed,
				float		brbsiz,
				int		brbwidth,
				float		hdsiz,
				int		wndtype );
void	pgwndw_setColr (	int		colr );

/*
 *  pgwpts functions  -------------------------------------------------
 */
void    pgwpts_expand (		float		dist, 
				int		shape, 
				float		*lat, 
				float		*lon, 
				int		*iret );

void	pgwpts_get (		int		point,
				int		shape,
				float		*ilat,
				float		*ilon,
				float		*olat,
				float		*olon,
				int		*iret );
void	pgwpts_init (		float		*ilat,
				float		*ilon,
				int		shape,
				float		*olat,
				float		*olon,
				int		*iret );
void	pgwpts_save (		float		*lat,
				float		*lon );
void	pgwpts_setSnap (	Boolean		snap );

/*
 *  pgwsmw functions  ----------------------------------------
 */
void	pgwsmw_create (		Widget		parent );
Boolean pgwsmw_isUp (		void );
void	pgwsmw_popdown (	void );
void	pgwsmw_selectEh (	Widget, XtPointer, XEvent*, Boolean* );

/*
 *  pgwxd functions  -----------------------------------------
 */
void	pgwxd_getfname (	char		*fname,
				int		*iret );
void	pgwxd_update (		char		*fname,
				int		*iret );

/*
 *  pgxrain functions  ----------------------------------------
 */
void	pgxrain_setltlnp (	int		raintyp,
				float		*elat,
				float		*elon,
				int		npts,
				char		closed,
				int		*iret );
void	pgxrain_udlist (	int		npts,
				int		raintyp,
				float		*lat,
				float		*lon,
				int		*iret );
void	pgxrain_update (	char		*fname,
				int		*iret );

/*
 * NMAP functions called by NMAPLIB routines
 *
 * dataw functions  ---------------------------------------
 */
void	dataw_getFrameTime (	int		iframe,
				char		*dttm );

/*
 *  nmap_mainw functions
 */
Widget  mainw_getToplevel  (    void    );

/*
 *  nmap_mbotw functions
 */
void	mbotw_actionClear (	void );
void	mbotw_actionSet	(	char		*aname );
void	mbotw_classClear (	void );
void	mbotw_classSet (	char		*cname );
void	mbotw_disabledSet (	void );
void	mbotw_enabledSet (	void );
void	mbotw_fadeReset (	void );
void	mbotw_getFadeColor (	void );
void	mbotw_hintRedraw (	void );
void    mbotw_startLocDspl (    void );
void    mbotw_endLocDspl (      void );
void    mbotw_updateLocDspl(    float           lat,
                                float           lon );
void	mbotw_loadingLoopSet (	Boolean		flag,
				int		lp );
void	mbotw_mouseClear (	void	);
void	mbotw_mouseSet (	char		*lstr,
				char		*mstr );
void	mbotw_pageSet (		int		current,
				int		total );
void	mbotw_pgfileClear (	void );
void	mbotw_pgfileSet (	char		*fname );

/*
 *  mcanvw functions  -------------------------------------
 */
void	mcanvw_disarmDrag (	void );
void	mcanvw_disarmDrop (	void );
void	mcanvw_disarmDynamic (	void );
void	mcanvw_disarmPress (	void );
void	mcanvw_getCurPos (	float		*xx,
				float		*yy );
Widget	mcanvw_getDrawingW (	void );
Boolean mcanvw_getDynActFlag (	void );
void	mcanvw_setCursor (	int		cursor );
void	mcanvw_setDragFunc (	XtEventHandler	func,
				int		cursor );
void	mcanvw_setDropFunc (	XtEventHandler	func,
				int		cursor );
void	mcanvw_setDynActFlag (	Boolean		dynflg );
void	mcanvw_setDynamicFunc (	XtEventHandler  start_func,
				XtEventHandler  drag_func,
				XtEventHandler  drop_func,
				int		cursor );
void    mcanvw_setLocDspl (     void );
void	mcanvw_setPressFunc (	XtEventHandler  func,
				int		cursor );

/*
 *  mmenuw functions  -------------------------------------
 */
void	mmenuw_exitNMAPCb (	Widget, XtPointer, XtPointer );
void	mmenuw_setExitNMAP (	Boolean		flag );

#endif /* PROTO_NMAPLIB */
