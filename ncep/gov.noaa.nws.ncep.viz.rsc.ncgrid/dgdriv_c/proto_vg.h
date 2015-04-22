/************************************************************************
 * proto_vg.h                                                           *
 *									*
 * This include file SHOULD ONLY CONTAIN prototypes for functions that	*
 * use any structures declared in vgstruct.h to type any variables,	*
 * including return values.  NOTE that this '.h' file is included at	*
 * the end of vgstruct.h, so any file that includes the vgstruct.h will	*
 * include all these prototypes.					*
 **                                                                     *
 * E. Safford/GSC	10/00	Created                                 *
 * A. Hardy/GSC		11/00   Added ces, crg, cvg 			*
 * J. Wu/GSC		12/00   Added cvg_initelm() and cvg_writelm()	*
 * A. Hardy/GSC		 2/01   Added avnbul 				*
 * J. Wu/GSC		02/01   Added cvg_write() & cvg_writef() and	*
 *	                        removed cvg_writ() & cvg_writelm()      *
 * J. Wu/GSC		02/01   Added cvg_rdoldele() and removed        *
 *                              cvg_rdwbx() & cvg_rdsig() 	 	*
 * H. Zeng/EAI          02/01   Added pggrpw_initType()                 *
 * E. Safford/GSC	05/01	added param to cvg_rdsel()		*
 * H. Zeng/EAI          09/01   added some pggrpw_**** functions        *
 * H. Zeng/EAI          11/01   added a pgwfmt_**** function            *
 * D.W.Plummer/NCEP	12/01	change pgwatch_editcnty			*
 * J. Wu/SAIC		12/01   add crg_mkRange        			*
 * J. Wu/SAIC		12/01   add cvg_level       			*
 * D.W.Plummer/NCEP     12/01   chg pgolk_udlist			*
 * E. Safford/SAIC	12/01	add pgutls_initHdr			* 
 * A. Hardy/SAIC        01/02   Added cvg_srchgrp and cvg_subtyp        *
 * J. Wu/SAIC		01/02	add layer param to cvg_scan		* 
 * E. Safford/SAIC	02/02	add cvg_setsubtyp			*
 * D.W.Plummer/NCEP      3/02   chg pgolk_udlist			*
 * J. Wu/SAIC		12/01   add cvg_rdrecnoc       			*
 * H. Zeng/EAI          04/02   removed pggrpw_initType()               *
 * H. Zeng/EAI          07/02   added crg_setsigmet()                   *
 * J. Wu/SAIC		09/02   add cds_list, crg_setlist		*
 * J. Wu/SAIC		11/02   add pg_list*** functions		*
 * H. Zeng/XTRIA	01/03   modified arguments of pgwatch_init()	*
 * H. Zeng/XTRIA	02/03	added cvg_cir2lin()			*
 * H. Zeng/XTRIA	03/03   added ces_getflag()			*
 * D.W.Plummer/NCEP	06/03	added cvg_t2v and cvg_v2t		*
 * D.W.Plummer/NCEP	06/03	added cds_ash and cds_vol		*
 * H. Zeng/XTRIA	07/03	added pgvolw_getAttr			*
 * J. Wu/SAIC		09/03   add cds_jet				*
 * H. Zeng/XTRIA	09/03   added pgvacw_getAttr			*
 * H. Zeng/XTRIA	10/03   added pgvacw_popup			*
 * R. Tian/SAIC         11/03   added cvg_snapjet                       *
 * E. Safford/SAIC	11/03	added cvg_gtnumpts			*
 * J. Wu/SAIC		01/04	add cvg_rdgfa & cvg_writgfa		*
 * J. Wu/SAIC		01/04   add cds_gfa				*
 * B. Yin/SAIC		02/04	added cvg_rdtca & cvg_writtca		*
 * B. Yin/SAIC		02/04	added cds_tca				*
 * J. Wu/SAIC		02/04   add pggfaw_****				*
 * A. Hardy/NCEP	 2/04   added pglist_createWtchLst 		*
 * B. Yin/SAIC		04/04	added pgtca_popup			*
 * B. Yin/SAIC		05/04	added cvg_loadTcaFileInfo		*
 * J. Wu/SAIC		07/04   add crg_setfilter()			*
 * M. Li/SAIC		08/04	added cvg_getElFilter			*
 * B. Yin/SAIC          07/04   added pgtca_freeBkpts               	*
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC		08/04   add cvg_scangfa()			*
 * H. Zeng/SAIC		10/04	CSC to pgwatch_init()			*
 * J. Wu/SAIC		10/04   add more cvg functions for GFA redesign	*
 * B. Yin/SAIC          11/04   Added af_selclip and af_format          *
 * B. Yin/SAIC           1/05   Added pgsmear_snapEl		        *
 * B. Yin/SAIC           3/05   Added pggfaw_checkHours		        *
 * E. Safford/SAIC	07/05	update cvgscangfa()			*
 * J. Wud/SAIC		07/05	replace af_format/af_selclip af_create	*
 * L. Hinson/AWC	09/05	added day param to af_create		*
 * H. Zeng/SAIC		09/05	moved cvg_srchgrp to proto_cgemlib.h	*
 * B. Yin/SAIC		01/06	added pggfaw_getLineWidth()		*
 * H. Zeng/SAIC		01/06	modified pgwatch_rmvGrpCnty &		*
 *					 pgwatch_addGrpCnty		*
 * S. Danz/AWC		03/06	Added cds_gfatxt                        *
 * T. Piper/SAIC	04/06	Removed all AVNBUL routines		*
 * S. Danz/AWC		07/06	add cvg_checkplace			*
 * S. Danz/AWC		07/06	modified arguments of  cvg_write(),	*
 *				cvg_writef() and pgvgf_saveNewElm()	*
 * B. Yin/SAIC		10/06	added af_useSnapshots()			*
 * E. Safford/SAIC	12/06	param change to af_useSnapshots()	*
 * S. Danz/AWC		01/07	add cvg_writed to keep out dev transf.  *
 * D.W.Plummer/NCEP     03/07   add af_qualityControl                   *
 * B. Yin/SAIC		03/07	added issuance in af_create()		*
 * E. Safford/SAIC	03/07	copy pggfaw_* to pggfawp_*		*
 * m.gamazaychikov/SAIC	05/07	add cvg_writTcInfo, cvg_writTcTrack,	*
 *				cvg_rdtce, cvg_rdtct, cvg_rdtcb, 	*
 *				cvg_writtce, cvg_writtct, cvg_writtcb	*
 * m.gamazaychikov/SAIC	06/07	add cds_tce, cds_tct, cds_tcb		*
 * J. Wu/SAIC		06/07	add pgsmear_smear()			*
 * J. Lewis/AWC		05/07	added pgsigw_getFIRs			*
 * J. Wu/SAIC		09/07	add "unionOnly" into pgsmear_smear()	*
 * J. Wu/SAIC		09/07	add pgsmear_*()				*
 * J. Wu/SAIC		09/07	add af_crtFBBA & af_copyGFA		*
 * B. Yin/SAIC		03/08	added pggfawp_checkGfa3Lines()		*
 * B. Yin/SAIC		04/08	added pggfawp_showStatesListWarning()	*
 ***********************************************************************/

#ifndef PROTO_VG
#define	PROTO_VG

/*
 *  af prototypes
 */
void af_copyGFA (	const VG_DBStruct	*elIn, 
			VG_DBStruct		*elOut, 
			int			*iret );

void af_crtFBBA (	char		*day, 
			char		*cycle, 
			int		issuance,
			int		nin, 
			VG_DBStruct	*elIn, 
			int 		*nout, 
			VG_DBStruct	**elOut, 
			int 		*iret );

void	af_create (     int		nareas,
			char            areas[][8],
                        int             ntypes,
                        char            *types[3],
                        char            *day,
                        char            *cycle,
                        int             nin,
                        VG_DBStruct     *el_in,
			int		issuance,
                        char            *string[6][3],
                        int             *iret );

void	af_useSnapshots ( 	
			VG_DBStruct	**elIn,
			int		nElIn,
			int		smearFlag,
			VG_DBStruct	***elOut,
			int		*nElOut,
			VG_DBStruct	***cancelElOut,
			int		*nCancelOut,
			int		*iret	);


void    af_qualityControl  (
                        int             nin,
                        VG_DBStruct     *el_in,
                        int             maxinfo,
                        char            *info,
                        int             *iret );
/*
 *	cds prototypes
 */

void	cds_ash (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_ccf (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_circ (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_dspelm (	VG_DBStruct     *el,
                        int             *iret );

void	cds_frnt (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_getinx (	VG_DBStruct     *el,
                        int             *indx,
                        int             *iret );

void	cds_gfa (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_gfatxt (	VG_DBStruct     const *el,
                        VG_DBStruct     *txt_el,
                        int             *iret );

void	cds_jet (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_line (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_list (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_sig (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_symb (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_text (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_track (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_tca (	VG_DBStruct     *el,
                        int             *iret );

void	cds_tcb (	VG_DBStruct     *el,
                        int             *iret );

void	cds_tce (	VG_DBStruct     *el,
                        int             *iret );

void	cds_tct (	VG_DBStruct     *el,
                        int             *iret );

void	cds_vol (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_wbox (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

void	cds_wind (	VG_DBStruct     *el,
                        int             indx,
                        int             *iret );

/*
 *  ces prototypes
 */

void	ces_get (	int		subtyp,
			VG_DBStruct  	*el,
			int		*iret );

Boolean ces_getflag (	int		subtyp,
			VG_DBStruct  	*el,
			int		*iret );

void	ces_getinx (	VG_DBStruct  	*el,
			int		subtyp,
			int		*indx,
			int		*iret );

void	ces_set (	int		subtyp,
			VG_DBStruct  	*el,
			int		*iret );

/*
 *  crg prototypes
 */

void	crg_gettxtbox ( VG_DBStruct   	*el,
			int		boxflag,
			float		*box_x,
			float		*box_y );

void	crg_mkRange ( 	VG_DBStruct   	*el,
			int		joffset,
			int		elnum,
			int		*iret );

void	crg_set ( 	VG_DBStruct   	*el,
			int		joffset,
			int		layer,
			int		*iret );

void	crg_setcir ( 	VG_DBStruct   	*el,
			int		joffset,
			int		elnum,
			float   	minlat,
			float		minlon,
			float   	maxlat,
			float   	maxlon,
			int     	*iret );

void    crg_setfilter ( int		elnum,
			VG_DBStruct   	*el,
			int     	*iret );
	
void	crg_setsym ( 	VG_DBStruct   	*el,
			int		joffset,
			int		elnum,
			int     	*iret );

void	crg_setlist ( 	VG_DBStruct   	*el,
			int		joffset,
			int		elnum,
			int     	*iret );

void	crg_settxt ( 	VG_DBStruct   	*el,
			int		joffset,
			int		elnum,
			int     	*iret );

void	crg_setwbx ( 	VG_DBStruct   	*el,
			int		joffset,
			int		elnum,
			int     	*iret );

void	crg_setwnd ( 	VG_DBStruct   	*el,
			int		joffset,
			int		elnum,
			int     	*iret );

void	crg_setsigmet (	VG_DBStruct   	*el,
			int		joffset,
			int		elnum,
			int     	*iret );

void	crg_setvol (	VG_DBStruct	*el, 
			int		joffset, 
			int		elnum, 
			int		*iret );

void	crg_setvac (	VG_DBStruct     *el, 
			int		joffset, 
			int		elnum, 
			int		*iret );


/*
 *  cvg prototypes
 */
void	cvg_allocGfaBlock ( VG_DBStruct	*const	el );

void	cvg_cir2lin (   VG_DBStruct     *el_cir, 
			int		res, 
			VG_DBStruct	*el_lin, 
			int		*iret );

void	cvg_checkplace (VG_DBStruct	*el,
			int		to_delete,
			int		location,
			int		*found,
			float		inf_bbox[4],
			int		*iret );

void	cvg_dump ( 	VG_DBStruct	el,
			int		size,
			int		nout,
			int		flag,
			int		*iret );

void    cvg_freeBkpts ( VG_DBStruct * el );

void	cvg_freeElPtr (	VG_DBStruct	*const	el );

void    cvg_freeTcb ( VG_DBStruct * el );
void    cvg_freeTct ( VG_DBStruct * el );

void	cvg_getElFilter ( VG_DBStruct   *el,
			  filter_t	filter,
			  int		*iret );

void	cvg_getFld (	const VG_DBStruct	*el,
			const char		*tag,
			char			*value,
			int			*iret );

int	cvg_gtnumpts ( 	VG_DBStruct	*el,
			int		*iret );

void	cvg_initelm (	VG_DBStruct	*el );

void	cvg_level (	VG_HdrStruct	*hdr,
			int		*level,
			int		*iret );

int 	cvg_loadTcaFileInfo ( TcaInfo 	info, 	
			      char 	**ptca, 
			      int 	*iret );

Boolean	cvg_placed (	int		location,
			VG_DBStruct	*el );

void	cvg_rdele ( 	VG_DBStruct	*el,
			int		start,
			int		numbytes,
			FILE		*fp,
			int		*iret );

void	cvg_rdgfa ( 	FILE		*fp,
			long		info_pos,
			VG_DBStruct	*el,
			int		*iret );

void	cvg_rdoldele ( 	VG_DBStruct	*el,
			int		el_start,
			int		el_size,
			FILE		*fp,
			int		*iret );

void	cvg_rdhdr ( 	char		*fname,
			FILE		*fp,
			int		start,
			int		size,
			VG_DBStruct	*el,
			int		*flag,
			int		*iret );

void	cvg_rdrec (	char		*fname,
			int 		fpos,
			VG_DBStruct	*el,
			int		*iret );

void	cvg_rdrecnoc (	char		*fname,
			FILE		*fptr,
			int 		fpos,
			VG_DBStruct	*el,
			int		*iret );

void	cvg_rdsel ( 	char		*fname,
			int		fpos,
			float		x,
			float		y,
			int		*hotpt,
			float		*distance,
			VG_DBStruct	*el,
			int		*iret );

void	cvg_rdtca (	FILE		*fptr,
			long		fil_pos,
			VG_DBStruct	*el,
			int		*iret );

void    cvg_rdtcb (     FILE            *fptr,
                        long            fil_pos,
                        VG_DBStruct     *el,
                        int             *iret );

void    cvg_rdtce (     FILE            *fptr,
                        long            fil_pos,
                        VG_DBStruct     *el,
                        int             *iret );

void    cvg_rdtct (     FILE            *fptr,
                        long            fil_pos,
                        VG_DBStruct     *el,
                        int             *iret );
                        
void	cvg_rmFld (	VG_DBStruct *const	el,
			const char		*tag,
			int			*iret );

void	cvg_scan ( 	char		*fname,
			int		layer,
			char		class,
			float		fx,
			float		fy,
			int		start,
			VG_DBStruct 	*el,
			int		*selected,
			int		*nearest,
			int		*iret );

void	cvg_scangfa ( 	char		*fname,
			int		layer,
			int		subtype,
			int		areatype,
			char		*tag,
			VG_DBStruct 	*el,
			int		*selected,
			int		*iret );

void	cvg_setFld (	VG_DBStruct *const	el,
			const char		*tag,
			const char		*value,
			int			*iret );

void	cvg_setsubtyp ( int		new_subtyp,
			VG_DBStruct	*el,
			int		*iret);

void	cvg_snapjet (   VG_DBStruct	*jet_in, 
			VG_DBStruct	*jet_out, 
			int		*iret );

void	cvg_subtyp (	VG_DBStruct	*el,
			int		*subtyp,
			int		*iret );

void	cvg_swap (	int		flag,
			int		readflg,
			VG_DBStruct	elold,
			VG_DBStruct	*elnew,
			int		*iret );

int	cvg_t2v (	char		*buffer,
			VG_DBStruct	*el,
			int		*iret );

void	cvg_todev (	VG_DBStruct	*el,
			int		*np,
			float		*dx,
			float		*dy,
			int		*iret );

void cvg_todev2 ( VG_DBStruct *el, int *np, float *dx, float *dy, int *iret );

int	cvg_v2t (	VG_DBStruct	*el,
			char		*buffer,
			int		*iret );

void	cvg_write (	VG_DBStruct	*el,
			int		start,
			int		numbytes,
			FILE		*fp,
			Boolean		inc_place,
			int		*iret );

void	cvg_writeD (	VG_DBStruct	*el,
			int		start,
			int		numbytes,
			FILE		*fp,
			int		*iret );

void	cvg_writef ( 	VG_DBStruct	*el,
			int		start,
			int		numbytes,
			char		*fname,
			Boolean		inc_place,
			int		*location,
			int		*iret );

void	cvg_writefD ( 	VG_DBStruct	*el,
			int		start,
			int		numbytes,
			char		*fname,
			int		*location,
			int		*iret );

void	cvg_writgfa ( 	FILE		*fp,
			VG_DBStruct	*el,
			int		*iret );

void	cvg_writtca (	FILE		*fptr,
			VG_DBStruct	*el,
			int		*iret );

void    cvg_writtcb (   FILE            *fptr,
                        VG_DBStruct     *el,
                        int             *iret );

void    cvg_writtce (   FILE            *fptr,
                        VG_DBStruct     *el,
                        int             *iret );

int     cvg_writTcInfo ( TcInfo         info, 
                        char            **ptcinfo, 
                        int             *iret );

void    cvg_writtct (   FILE            *fptr, 
                        VG_DBStruct     *el,
                        int             *iret );

int     cvg_writTcTrack ( VG_DBStruct     *el, 
                        char            **ptctrack, 
                        int             *iret );

/*
 *  pgen prototypes
 */

void	pgactv_setActvElm (	VG_DBStruct	*el,
				int		el_offset );


void	pgccfw_popup (		VG_DBStruct	*el,
				XtCallbackProc	callback );

void	pgccfw_saveEdit (	VG_DBStruct	*cur_el,
				int		el_location,
				int		np,
				float		*lats,
				float		*lons );

void	pgccfw_getAttr (	VG_DBStruct	*el );


void    pgcirc_setGhostFlag (   Boolean         flag,   
                                void            update(VG_DBStruct *) );
				
void	pgconn_start (		VG_DBStruct	*el,
				float		currx,
				float		curry );


void	pgdelpt_start (		VG_DBStruct	*el );


void	pgedit_editStart (	VG_DBStruct 	*el );

void    pgfrom_smear( 		int		 numSnapShots,
			   	int 		snapShots[], 
			   	Boolean 	makeSmearElm,
				int		color,
				int		numIn,
			   	VG_DBStruct	*el_in,
			   	Boolean 	timeSmear,
			   	Boolean 	skipFzlvl,
			   	Boolean 	useAllSnapShots,
			   	Boolean 	reducePts,
			   	Boolean 	unionOnly,
			   	VG_DBStruct	*el,
				int		*numPts,
			   	float		xPts[],
			   	float		yPts[] );


void 	pgfrom_snapEl ( 	Boolean 	expandOnly, 
				VG_DBStruct *const el, 
				int 		*iret );

void 	pggfaw_checkHours ( 	VG_DBStruct     *el );

void	pggfaw_getAttr (	VG_DBStruct	*el );

void	pggfaw_getLineWidth (	VG_DBStruct	*el );

void	pggfaw_popup (		VG_DBStruct	*el,
				XtCallbackProc	callback );


void	pggfaw_setAttr (	VG_DBStruct	*el );

void 	pggfawp_checkHours ( 	VG_DBStruct     *el );

void	pggfawp_getAttr (	VG_DBStruct	*el );

void	pggfawp_getLineWidth (	VG_DBStruct	*el );

void	pggfawp_popup (		VG_DBStruct	*el,
				XtCallbackProc	callback );

void	pggfawp_setAttr (	VG_DBStruct	*el );

void 	pggfawp_checkGfa3Lines( VG_DBStruct 	*el );

void  	pggfawp_statesListWarning( VG_DBStruct  *el );

void	pggrpw_addtoGrp (	VG_DBStruct     *el, 
                                int             location, 
                                int             *iret );

void	pggrpw_rmvfrmGrp (	VG_DBStruct     *el, 
                                int             location, 
                                int             *iret );


void	pggst_setText (		VG_DBStruct	*el );

void	pggst_setCircle (	VG_DBStruct	*el );


void	pghdlb_select (		VG_DBStruct 	*el,
				int		location );


void	pglist_createWtchLst (	VG_DBStruct	*el, 
				char		*fname);

VG_DBStruct *pglist_getCurList ( void );

void	pglist_setCurList (	const VG_DBStruct *list );


void	pgpdel_startPDel (	VG_DBStruct	*el );

void	pgsigw_getAttr (	VG_DBStruct	*el );

void	pgsigw_getState (	VG_DBStruct	*el,
				char		st[],
				int		*iret );

void	pgsigw_getFIRs (	VG_DBStruct	*el,
				char 		fir[],
				int		*iret );

void	pgsigw_popup (		VG_DBStruct	*el,
				XtCallbackProc	callback );

void	pgsigw_setAttr (	VG_DBStruct	*el );

void    pgsmear_smear( 		int		 numSnapShots,
			   	int 		snapShots[], 
			   	Boolean 	makeSmearElm,
				int		color,
				int		numIn,
			   	VG_DBStruct	*el_in,
			   	Boolean 	timeSmear,
			   	Boolean 	skipFzlvl,
			   	Boolean 	useAllSnapShots,
			   	Boolean 	reducePts,
			   	Boolean 	unionOnly,
			   	VG_DBStruct	*el,
				int		*numPts,
			   	float		xPts[],
			   	float		yPts[] );


void 	pgsmear_snapEl ( 	Boolean 	expandOnly, 
				VG_DBStruct *const el, 
				int 		*iret );

void    pgtca_popup (   VG_DBStruct *el  );

void	pgtrkw_extrapolate (	float		lat[],
				float		lon[],
				VG_DBStruct	*el );

void	pgtrkw_getAttr (	VG_DBStruct	*el );

void	pgtrkw_setAttr (	VG_DBStruct	*el );

void	pgtrkw_updResults (	VG_DBStruct	*el ); 


void	pgtxt_editPopup (	VG_DBStruct	*el,
				Boolean		attrb_edit,
				XtCallbackProc	callback );

void	pgtxt_fillElement (	int		sptype,
				VG_DBStruct 	*el );

void    pgtxt_setGhostFlag (    Boolean         flag,
				void            update(VG_DBStruct *));

void	pgutls_prepNew (	int		location,
				VG_DBStruct	*el,
				float		*llx,
				float		*lly,
				float		*urx,
				float		*ury,
				int		*iret );

void	pgutls_redraw (		int		location,
				VG_DBStruct	*el,
				int		*iret );

void	pgutls_initHdr (	VG_HdrStruct	*hdr );

void	pgvacw_getAttr (	VG_DBStruct     *el  );

void	pgvacw_popup (		VG_DBStruct	*el  );

void	pgvgf_saveNewElm ( 	char		*filnam,
				char            *sys,
				VG_DBStruct     *el,
				int		np,
				float		rx[],
				float		ry[],
				Boolean		updatepl,
				int		*location,
				int		*iret );

void	pgvolw_getAttr (	VG_DBStruct	*el );

void	pgvolw_editPopup (	VG_DBStruct	*el );

void	pgwatch_addGrpCnty (    int		grp_typ,
				char		*name,
                                VG_DBStruct     *el   );

void	pgwatch_editcnty ( 	int		eflag,
				VG_DBStruct	*el,
				int		nlocs,
				int		fips[] );

VG_DBStruct *pgwatch_getCurElm ( void );

void	pgwatch_init ( 		VG_DBStruct	*el,
				int		style,
				int		shape,
				int		color,
				int		mtype,
				float		msize,
				int		mwidth,
				char		cnty_fill,
				int		cnty_colr,
				int		np,
				float		*lat,
				float		*lon,
				int		*iret );

void	pgwatch_rmvGrpCnty (	int		grp_typ,
				char		*name,
                                VG_DBStruct     *el   );

void	pgwatch_save (		VG_DBStruct	*el );


void	pgwfmt_setWatch (	VG_DBStruct	*el );

void	pgwfmt_setWCC (		VG_DBStruct	*el );

void	pgwlst_update (		VG_DBStruct	*el );

void	pgwsmw_popup (		VG_DBStruct	*el,
				XtCallbackProc	callback );

void	pgwsmw_setWatch (	VG_DBStruct	*el );


#endif	/* PROTO_VG */
