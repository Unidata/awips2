/************************************************************************
 * proto_cgemlib.h                                                      *
 *                                                                      *
 * This include file contains function prototypes for all the c files   *
 * in the cgemlib libraries _except_ those libraries that have routines *
 * which use the VG_DBStruct (vg element) structure as a parameter.	*
 *									*
 * Functions that use VG_DBStruct are prototyped in proto_vg.h.		*
 *									*
 **                                                                     *
 * E. Safford/GSC       10/00   Created                                 *
 * A. Hardy/GSC		11/00   Added cbf,cgr,cpg,crg,css,cst,ctb,	*
 *				cvq, utf				*
 * J. Wu/GSC       	12/00   Removed all cvg_sv??? except cvg_svfhed *
 * J. Wu/GSC       	01/01   Added cvg_crelm() 			*
 * S. Jacobs/NCEP	 1/01	Changed ctb_pllist			*
 * D.W.Plummer/NCEP	 2/01	Added clo_findnum and ctb_ccfind	*
 * A. Hardy/GSC          2/01   Changed cvg_open/_qkopen ptr. int->FILE *
 * J. Wu/GSC 		02/01	Removed cvg_index and cvg_fndhl		*
 * D. Plummer/NCEP	02/01	Added cgr_qsol				*
 * H. Zeng/EAI		03/01	Added ces_gtgid, ces_gtgnam, ces_gtrtbl	*
 * H. Zeng/EAI          03/01   Added more ces routines                 *
 * H. Zeng/EAI          03/01   Replace ces_gtgcolr with ces_gtginfo    *
 * T. Piper/GSC		04/01	Removed cpg_fndgrp			*
 * D.W.Plummer/NCEP	04/01	Added several CLO routines		*
 * S. Jacobs/NCEP	 5/01	Added cfl_tinq				*
 * D.W.Plummer/NCEP	 5/01	Added cst_gtag				*
 * J. Wu/GSC	 	 6/01	add SPF library				*
 * J. Wu/GSC	 	 6/01	add ctb_dcatitos(), ctb_dcatstoi()      *
 *				   and ctb_dscatitos(), ctb_dscatstoi()	*
 * F. J. Yen/NCEP	 8/01	removed parameter icmp in clo_cmpdir	*
 * T. Piper/SAIC	 8/01	removed clo_bqinfo			*
 * M. Li/SAIC           08/01   add cgr_intersect, cpcg_newpoly,        *
 *                                 clo_reorder, cpcg_rdln, cpcg_rdbnd,  *
 *                                 and cpcg_srch                        *
 * J. Wu/SAIC	 	11/01	change prototypes of cvg_load/cvg_load2 *
 * S. Jacobs/NCEP	11/01	Added cgr_centroid			*
 * J. Wu/SAIC	 	12/01	add/modify CRG/CVG layer-related func.  *
 * J. Wu/SAIC	 	12/01	add cvg_redraw()			*
 * R. Tian/SAIC          1/02	added cst_ctod				*
 * A. Hardy/SAIC        01/02	added ces_gtinit, cvg_gtgnum, cvg_rdgtn *
 * J. Wu/SAIC	 	01/02	add layer to crg_get/cvg_deall/cvg_fscan*
 * D.W.Plummer/NCEP	01/02	add clo_bstag, clo_bqinfo		*
 * J. Wu/SAIC	 	02/02	change param for ctb_trkitv()		*
 * T. Lee/SAIC           2/01	Added ctb_lyrd, ctb_lygetname,          *
 *                              ctb_lygetfile, ctb_lygetcmode,          *
 *                              ctb_lygetcolor, ctb_lygetfmode          *
 * A. Hardy/SAIC         2/01	Removed CAS library, put in proto_cas.h *
 * E. Safford/SAIC	02/02	add crg_mvallayer()			*
 * J. Wu/SAIC	 	02/02	add cvg_drawLayer()			*
 * E. Safford/SAIC	02/02	rm ces_gtinit, add ces_gtlmstr		*
 * T. Lee/SAIC		02/02	add ctb_lygetgrptyp			*
 * J. Wu/SAIC	 	03/02	add cvg_rfrshLayer()			*
 * J. Wu/SAIC	 	03/02	add parameter to crg_mvallayer()	*
 * E. Safford/SAIC      03/02   param chg ces_gtggrps, add ces_gtgavid  *
 * M. Li/SAIC		04/02   add crg_ggnhl				*
 * E. Safford/SAIC	04/02   param chg crg_clearLayer                *
 * E. Safford/SAIC	04/02   add cfl_isdir()				*
 * A. Hardy/SAIC        04/02	Added cgr_ptonln and clo_cetr		*
 * H. Zeng/EAI		04/02	Added ces_gtgmsid			*
 * T. Piper/SAIC	04/02	Added cfl_perms				*
 * T. Lee/SAIC		05/02	Added ctb_lygetdsply, cvg_valid		*
 * H. Zeng/EAI          05/02   added ces_gtgmgrps                      *
 * E. Safford/SAIC	06/02	added param to ctb_lpgetname  		*
 * R. Tian/SAIC		07/02	removed dsply_grpd from cvg_fscan	*
 * J. Wu/SAIC		08/02	remove clo_cetr				*
 * H. Zeng/EAI          08/02   added some ctb_pf**** func.             *
 * D.W.Plummer/NCEP	08/02	added clo_bgcent			*
 * m.gamazaychikov/SAIC	 9/02	change calling sequence of clo_reorder	*
 * D.W.Plummer/NCEP	11/02	added clo_bgrange			*
 * W.D.Plummer/NCEP	12/02	changed cgr_segdist parameters to ptrs	*
 * R. Tian/SAIC		02/03	added ctb_rdcpf and ctb_wrcpf		*
 * D.W.Plummer/NCEP	 3/03	modify ctb_rdcpf, ctb_wrcpf calling seq	*
 * m.gamazaychikov/SAIC	 5/03   added ctb_g2gnam, ctb_g2gnum, ctb_g2read*	
 * m.gamazaychikov/SAIC  5/03   added parm to ctb_g2gnam and ctb_g2gnum	*
 * R. Tian/SAIC		 5/03	added cgr_insert			*
 * D.W.Plummer/NCEP	 6/03	added cst_stag				*
 * T. Piper/SAIC	 7/03	corrected prototpye for scandir		*
 * m.gamazaychikov/SAIC	07/03	added shp prototypes			*
 * A. Hardy/NCEP	 8/03	added cst_sort, cst_ncat and ctb_rdwou	*
 * D.W.Plummer/NCEP	 8/03	added cgr_range 			*
 * D.W.Plummer/NCEP	 8/03	changed cgr_dang			*
 * D.W.Plummer/NCEP	11/03	added cgr_polyunion/polylink/reorder	*
 * T. Lee/SAIC          11/03   added cvg_getworkfile                   *
 * E. Safford/SAIC      11/03   added ctb_lygetoutfile                  *
 * D.W.Plummer/NCEP	11/03	added cgr_polysmear/init/done		*
 * D.W.Plummer/NCEP	 2/04	added cgr_polyinterp			*
 * B. Yin/SAIC		03/04	changed css_gtim and css_date		*
 * D.W.Plummer/NCEP	 1/04	calling seq chg to cgr_range 		*
 * B. Yin/SAIC          03/04   Added event clock functions to css      *
 * A. Hardy/NCEP	 4/04	Added ctb_mzgnm				*
 * M. Li/SAIC		 4/04	Added new parm to ctb_g2gnam		*
 * B. Yin/SAIC           5/04   Added css_evtison and css_evtadvtime    *
 * T. Piper/SAIC	05/04	Added cvg_getoutdir	 		*
 * J. Wu/SAIC		07/04	add filter related functions in crg/cvg	*
 * J. Wu/SAIC		08/04	add cst_padString()			*
 * m.gamazaychikov/SAIC	08/04   add cvg_rebun                           *
 * E. Safford/SAIC	08/04	add cvg_getFlghtLvl			*
 * H. Zeng/SAIC		09/04	changed spf_read calling sequence	*
 * T. Lee/SAIC          09/04   add bin hrs, ctb_dhrsitos, ctb_dhrsstoi *
 * B. Yin/SAIC		 9/04	added clo_clip/clo_clipget/clo_clipdone	*
 * J. Wu/SAIC		09/04	add cgr_reducePts()			*
 * E. Safford/SAIC	08/04	add cst_rmtag, mod cst_gtag, cst_stag	*
 * B. Yin/SAIC          10/04   changed cgr_polysmear                   *
 * m.gamazaychikov/SAIC 10/04   add clo_fastates                        *
 * R. Tian/SAIC		10/04	added cst_rspc, removed shp_*		*
 * B. Yin/SAIC          10/04   added ctb_gfa***                        *
 * E. Safford/SAIC      10/04   add cgr_polydiff and crg_goffsets       *
 * A. Hardy/NCEP	10/04	added ctb_permccfind			*
 * E. Safford/SAIC      10/04   add xml_transform                       *
 * M. Li/SAIC		10/04	Replaced ctb_rdwou with ctb_rdprf	*
 * B. Yin/SAIC          11/04   added ctb_gfagcat                       *
 * H. Zeng/SAIC		12/04	added cgr_ordrccw			*
 * m.gamazaychikov/SAIC 12/04   add ionoff flag to ctb_dhrsstoi, 	*
 *				ctb_dhrsitos, dtb_dtget			*
 * B. Yin/SAIC          12/04   added ctb_gfagtemp                      *
 * D.W.Plummer/NCEP	 1/05	added cgr_lineinterp			*
 * A. Hardy/NCEP	 4/05	changed ints in cvg_rebun to pointers	*
 * H. Zeng/SAIC		04/05	changed cgr_rolseg arguments		*
 * J. Wu/SAIC		05/05	add cgr_sphpolyarea()			*
 * J. Wu/SAIC		06/05	add clo_qmxpts()			*
 * R. Tian/SAIC		07/05	add cgr_vectxprod()			*
 * R. Tian/SAIC		07/05	add cgr_qrol()				*
 * E. Safford/SAIC	07/05   add ctb_airmetGetIssueTm, GetCycleTms   *
 * B. Yin/SAIC		09/05	changed clo_stngall()			*
 * H. Zeng/SAIC		10/05	moved cvg_srchgrp here from proto_vg.h	*
 * J. Wu/SAIC		10/05	add ctb_gfaCmpSeverity()		*
 * L. Hinson/AWC	10/05	add ctb_gfagiss()         		*
 * B. Yin/SAIC		12/05	add clo_snapPt()			*
 * T. Piper/SAIC	12/05	added cds_class and cds_match		*
 * m.gamazaychikov/SAIC 12/05   Added crg_build                         *
 * B. Yin/SAIC          12/05   add cgr_linepolyint()                   *
 * R. Tian/SAIC		 1/06	add cst_opcl, cfl_scnt			*
 * T. Piper/SAIC	 1/06	Modified cst_wrap CS                    *
 * B. Yin/SAIC           1/06   add cgr_linepoly() 	                *
 * E. Safford/SAIC	01/06	add xml_count, xml_value, xml_getsubdoc *
 * E. Safford/SAIC	02/06	add clo_snapOnePt			*
 * H. Zeng/SAIC		02/06	added clo_findcwa()			*
 * J. Wu/SAIC		02/06	add more parameters in cgr_reducePts()	*
 * S. Danz/AWC		03/06	add cgr_bisectpt, cgr_csegint,          *
 *                              cgr_inpolywn, cgr_objinpoly, cgr_objint,*
 *                              cgr_concave                             *
 * D.W.Plummer/NCEP     04/06   chg call seq of clo_snapPt              *
 * B. Yin/SAIC		04/06	add cgr_linelen()			*
 * E. Safford/SAIC	04/06	add cgr_reduceLinePoly			*
 * J. Wu/SAIC		04/06	add parameter newLineStr in cst_wrap()	*
 * E. Safford/SAIC	04/06	add xml_readTable			*
 * E. Safford/SAIC	04/06	add ctb_gfaGetWorstIssType		*
 * E. Safford/SAIC	04/06	add ctb_gfaCombineIFRTypes		*
 * m.gamazaychikov/SAIC 04/06   add dtmmtch to ctb_dtget CS             *
 * B. Yin/SAIC          06/06   add ctb_gfagid                          *
 * B. Yin/SAIC          06/06   add ctb_gfagdesk                        *
 * J. Wu/SAIC		06/06	add cvg_matchfilter & cvg_gettblfilter &*
 *					cvg_rdfilter			*
 * B. Yin/SAIC          07/06   remove ctb_gfaGetWorstIssTyp            *
 * S. Danz/AWC		07/06	add cvg_initplace, cvg_clearplace,      *
 *				cvg_rebuildplace, cvg_updateplace,	*
 *		        	cvg_plenabled				*
 * S. Danz/AWC		08/06	updated parameters for cvg_delall, 	*
 *				cvg_delet, and cvg_undel		*
 * D.W.Plummer/NCEP	09/06	Added some new DM library functions	*
 * D.W.Plummer/NCEP     09/06   Modified clo_snapPt			*
 * J. Wu/SAIC		09/06	added ctb_gfaWorstFL & ctb_gfaWorstIssue*
 * E. Safford/SAIC      09/06   modified cgr_linepoly, add cgr_segintwn *
 * J. Wu/SAIC		09/06	modified cgr_reducePts and added	*
 *				cgr_canBeFormatted & clo_cleanFmLine	*
 * D.W.Plummer/NCEP	12/06	cgr_vectxprod parameter list to double	*
 * D.W.Plummer/NCEP	12/06	cgr_qrol tolerance added		*
 * D.W.Plummer/NCEP	12/06	Add clo_tmatch				*
 * S. Jacobs/NCEP	12/06	Added cst_zpad				*
 * J. Wu/SAIC		02/07	add clo_snapPoly & clo_isCluster	*
 * J. Wu/SAIC		03/07	add clo_snapPtGFA			*
 * M. Li/SAIC           03/07   Updated cvg_matchfilter			*
 * J. Wu/SAIC		03/07	add parameters into clo_snapPtGFA	*
 * T. Piper/SAIC	04/07	Removed cfl_rscd and cfl_ffil		*
 * T. Piper/SAIC	04/07	Added cfl_scandir, modified cfl_scnt	*
 * S. Jacobs/NCEP	10/07	Added ctb_hfread, ctb_hfdump,		*
 * 				ctb_hfgetfont				*
 * E. Safford/SAIC	11/07	use G_Boolean instead of Boolean to rm	* 
 *				 the X/Motif dependency			*
 * J. Wu/SAIC           04/08   add crg_getsecrange()                   *
 * F. J. Yen/NCEP	04/08	Added parameters to ctb_dhrsstoi,	*
 *				ctb_dhrsitos, and ctb_dtget		*
 * B. Yin/SAIC          06/08   Added ctb_gfapCombineIFRTypes and       *
 *				ctb_gfaHasCV				*
 ***********************************************************************/

#ifndef PROTO_CGEMLIB
#define PROTO_CGEMLIB


/*
 *  cap and cmd prototypes
 */
#include "proto_cap.h"

/*
 *  cbf prototypes
 */

void    cbf_clos (      int	*iret );

void    cbf_open (      char	*filnam,
			int	*iret );

void	cbf_read ( 	unsigned char	*barray,
			int	*nbytes,
			int	*iret );


/*
 *      cds prototypes
 */

void    cds_atdeflt (   int	*iret );

void	cds_class (	char	*vg_classstr,
			const char *vg_typestr,
			int	*vg_class,
			int	*vg_type,
			int	*iret );

void    cds_gfill (     int	*ifill,
                        int	*iret );

void    cds_init (      int	*iret );

void	cds_match (	const int vg_class,
			const int vg_type,
			const int subtyp,
			int *indx,
			int *iret );

void    cds_ress (      int	*iret );

void    cds_rtbl (      char	*attrfnam,
                        int	*iret );

void    cds_scal (      char	*fname,
                        int	*iret );

void    cds_scol (      int	icol,
                        int	*iret );

void    cds_sfill (     int	*ifill,
                        int	*iret );


/*
 *  ces prototypes
 */
int	ces_gtgavid (	char	grptyp );

int	ces_gtgmsid (	int	indx );

void    ces_gtginfo (   int     grpid, 
                        char    *label, 
                        char    *info, 
                        int     *iret );

void    ces_gtggrps (   int     *ngrp, 
                        char    **names, 
                        int     *iret );

void    ces_gtgid (     char	*grpnam, 
                        int	*grpid, 
                        int	*iret );

void    ces_gtglbls (   int     grpid, 
                        int     *nlbl, 
                        char    *lbls, 
                        int     *iret );

void    ces_gtgmgrps(   G_Boolean incl_dev, 
                        int     *ngrp, 
                        char    **names, 
                        int     *iret );

void    ces_gtgnam (    int	grpid, 
                        char	*grpnam, 
                        int	*iret );

void	ces_gtlmstr (	int	*iret );

void    ces_gtrtbl (    int	*iret );

void    ces_rtbl (      int	*iret );


/*
 *  cfl prototypes
 */

FILE	*cfl_aopn ( 	char    *filnam,
		   	int     *iret );

void 	cfl_clos ( 	FILE 	*fptr, 
			int 	*iret );

void 	cfl_dopn ( 	char 	*filnam, 
			int 	*ifdes, 
			int 	*iret );

int 	cfl_gfil ( 	int     sortby,
			int     max_files,
			char    *dirname,
			char    *search, 
			char    flist[][MXFLSZ] );

void 	cfl_inqr ( 	const char 	*filnam, 
			const char 	*defdir, 
			long 		*flen, 
			char 		*newfil,
                	int  		*iret );

void 	cfl_iret ( 	int 	ierrno, 
			int 	*iflerr, 
			int 	*iret );

G_Boolean cfl_isdir (	char	*cfile );

void 	cfl_mdat ( 	char 	*filnam, 
			char 	*templt, 
			char 	*defdat,
                	char 	*dattim, 
			int  	*iret );

void 	cfl_mnam ( 	char 	*dattim,
			char 	*templt,
			char 	*filnam,   
			int  	*iret );

void 	cfl_path ( 	char 	*fulnam, 
			char 	*dirnam, 
			char 	*basnam, 
			int 	*iret );

void	cfl_perms (	char    *file,
			G_Boolean *can_read,
			G_Boolean *can_write,
			int     *iret );

int 	cfl_rdir ( 	int    	type,                                     
			char   	*dirname,
			char   	*search,           
			struct 	dirent **dnlist[],           
			int	*nsdir );

void 	cfl_rdln ( 	FILE 	*fptr, 
			int 	bufsiz, 
			char 	*buffer, 
			int 	*iret );

void 	cfl_read ( 	FILE 	*fptr, 
			int 	nbytes, 
			unsigned char *buffer, 
			int 	*nbin,
                	int 	*iret );

FILE 	*cfl_ropn ( 	char    *filnam,
			char    *defdir,
			int     *iret );

int	cfl_scandir (	const char	*dir,
			const char	*search,
			int		(*filter)(const struct dirent *),
			int		(*compar)(const void *, const void *),
			struct dirent	***namelist );

#ifdef IRIX
int _alphasort  ( void *, void * );
int _ralphasort ( void *, void * );
int _datesort   ( void *, void * );
int _rdatesort  ( void *, void * );
int _selectdir     ( struct dirent *check );
int _selectdirOnly ( struct dirent *check );
int _selectdirExcd ( struct dirent *check );
int _selecttmplt   ( struct dirent *check );
#else
int _alphasort  ( const void *, const void * );
int _ralphasort ( const void *, const void * );
int _datesort   ( const void *, const void * );
int _rdatesort  ( const void *, const void * );
int _selectdir     ( const struct dirent *check );
int _selectdirOnly ( const struct dirent *check );
int _selectdirExcd ( const struct dirent *check );
int _selecttmplt   ( const struct dirent *check );
#endif

void 	cfl_scnd ( 	char    *path,
			int     *plen,
			char    *tmplt,
			int     *tlen,                                
			char    *sep,
			int     *maxlen,
			int     *order,               
			char    *filstr,
			int     *flen,
                        int	*nf,
                        int	*iret );

void    cfl_scnt (      const char *path,
                        const char *tmplt,
                        int     isort,
                        struct dirent	***files,
                        int     *nfile,
                        int     *iret );

void 	cfl_seek ( 	FILE 	*fptr, 
			long 	loffset, 
			int 	iorigin, 
			int 	*iret );

void 	cfl_srch ( 	FILE 	*fptr, 
			char 	*pattrn, 
			int 	idir, 
			int 	*iret );

void    cfl_tbnr ( 	FILE    *fp,    
			int     *nr,
			int     *iret );

FILE 	*cfl_tbop ( 	char    *table,
			char    *type,
			int     *iret );

void 	cfl_tinq ( 	char 	*filnam, 
			char 	*type, 
			long 	*flen, 
			char 	*newfil,
                	int  	*iret );

FILE 	*cfl_tmpo ( 	int 	*iret );

void 	cfl_trln ( 	FILE 	*fptr, 
			int 	bufsiz, 
			char 	*buffer, 
			int 	*iret );

FILE 	*cfl_uopn ( 	char    *filnam,
			int     *iret );

void 	cfl_wher ( 	FILE 	*fptr, 
			long 	*lfaddr, 	
			int 	*iret );

FILE 	*cfl_wopn ( 	char    *filnam,
			int     *iret );

void 	cfl_writ ( 	FILE 	*fptr, 
			int 	nbytes, 
			unsigned char *buffer, 
			int 	*iret );

/*
 *  cgr prototypes
 */

int     cgr_bounds ( 	int 	ptx,
			int 	pty,
			int 	spanx,
			int 	spany,
			int 	ileft,
			int 	irght,
			int 	ibot,
			int 	itop );

int  	cgr_canBeFormatted( int npts,
			float	*xlat, 
			float	*ylon,
			char	*prefix );

void	cgr_centroid (	float	x[],
			float	y[],
			int	*np,
			float	*xcent,
			float	*ycent,
			float	*area,
			int	*iret );

int     cgr_cntstrks ( 	int 	on_len,
			int 	off_len,
			int	np,
			int 	ix[],
			int 	iy[],
			int 	*iret );

void    cgr_dang ( 	float 	*pt1x,
			float	*pt1y,
			float	*pt2x,
			float	*pt2y,
			float   *devang,
			int 	*iret );

void	cgr_linelen(	float	*lat,
			float	*lon,
			int	npts,
			float	*length,
			int	*iret );

void 	cgr_polydiff ( 	int 	*npin0, 
			float 	*xin0, 
			float 	*yin0,
			int 	*npin1, 
			float 	*xin1, 
			float 	*yin1, 
			int 	*maxnpo,
			int 	*npo, 
			float 	*xo, 
			float 	*yo, 
			int 	*iret );

void	cgr_dist ( 	int	np,
			float	*xx, 
			float	*yy,
			float	fx,
			float	fy,
			float	*distance,
			int	*nearest_vrt,
			int	*iret );
 
void    cgr_done (    	int 	*ier);
 
void    cgr_init (    	int 	*ier);
 
void    cgr_inpoly (    char 	*syspts, 
			int 	*npts,
			float 	*x,
			float 	*y,
			char 	*syspoly,
			int 	*npoly,
			float 	*px,
			float 	*py,
			int 	*inout,
			int 	*iret );

void    cgr_insert (    float 	*px,
                        float 	*py,
                        int 	np,
                        float 	*qx,
                        float 	*qy,
                        int 	nq,
			float 	dens,
			float 	crvscl,
                        float 	*tx,
                        float 	*ty,
                        int 	*nt,
                        int 	*widx,
                        int 	*iret );

void	cgr_intersect (	char 	*sysp1, 
			int 	*np1, 
			float 	*xp1, 
			float 	*yp1,
                	char 	*sysp2, 	
			int 	*np2, 
			float 	*xp2, 
			float 	*yp2, 
			int 	*intrsct1,
                	char 	*sys3, 
			int 	*intrsct2, 
			float 	*xout, 
			float 	*yout, 
			int	*bpnt1,
			int	*apnt1,
			int	*bpnt2,
			int	*apnt2,
			int 	*iret );

void	cgr_lindist (	float	x1, 
			float	fy1, 
			float	x2,
			float	y2,
			float	fx,
			float	fy,
			float	*px,
			float	*py,
			float	*distance,
			int	*iret );

void 	cgr_linepoly ( 	int 	nptsLine, 
			float 	*latLine,  
			float 	*lonLine, 
			int 	nptsPoly, 
			float 	*latPoly,  
			float 	*lonPoly, 
		    	int 	*nout, 
			float 	**xout, 
			float 	**yout, 
			G_Boolean **inout, 
		    	int 	*iret );

void	cgr_linepolyint ( 	const char 	*sysLine, 
			const int 	nptsLine, 
			const float 	*xLine, 
                      	const float 	*yLine, 
			const char 	*sysPoly, 
			const int 	nptsPoly,
		      	const float 	*xPoly, 
			const float 	*yPoly, 
			G_Boolean 	*intersct, 
		      	int 		*iret );

int	cgr_ntrsct ( 	float 	allx,
			float 	ally,
			float 	aurx,
			float 	aury,
			float 	bllx,
			float 	blly,
			float 	burx,
			float 	bury,
			int  	*iret );

void	cgr_ordrccw (   int     npts, 
			float   *xpt, 
			float   *ypt, 
			int     *iret );

void	cgr_ptonln ( 	float 	px, 
     			float 	py, 
			float 	qx, 
			float 	qy, 
			float 	tx,
                        float 	ty, 
			int   	*iside, 
			int 	*iret );

void	cgr_polyint (	char 	*sysp1, 
			int 	*np1, 
			float 	*xp1, 
			float 	*yp1, 
			char 	*sysp2,
			int 	*np2, 
			float 	*xp2, 
			float 	*yp2, 
			int 	*intrsct, 
			int 	*iret );

void	cgr_lineinterp( int 	*npin0, 
			float 	*xin0, 
			float 	*yin0,
			int 	*npin1, 
			float 	*xin1, 
			float 	*yin1, 
			float	*pct,
			int 	*maxnpo,
			int 	*npo, 
			float 	*xo, 
			float 	*yo, 
			int 	*iret );

void	cgr_polyinterp( int 	*npin0, 
			float 	*xin0, 
			float 	*yin0,
			int 	*npin1, 
			float 	*xin1, 
			float 	*yin1, 
			float	*pct,
			int 	*nmap, 
			float 	*xmap1, 
			float 	*ymap1, 
			float 	*xmap2, 
			float 	*ymap2,
			int 	*maxnpo,
			int 	*npo, 
			float 	*xo, 
			float 	*yo, 
			int 	*iret );

void 	cgr_polylink ( 	int 	*npin0, 
			float 	*xin0, 
			float 	*yin0,
			int 	*npin1, 
			float 	*xin1, 
			float 	*yin1, 
			int 	*maxnpo,
			int 	*npo, 
			float 	*xo, 
			float 	*yo, 
			int 	*iret );

void 	cgr_polysmear ( char    *smear_type,
			int 	*npin0, 
			float 	*xin0, 
			float 	*yin0,
			int 	*npin1, 
			float 	*xin1, 
			float 	*yin1, 
			int 	*nmap, 
			float 	*xmap1, 
			float 	*ymap1, 
			float 	*xmap2, 
			float 	*ymap2,
			int 	*maxnpo,
			int 	*npo, 
			float 	*xo, 
			float 	*yo, 
			int 	*iret );

void 	cgr_polyunion ( int 	*npin0, 
			float 	*xin0, 
			float 	*yin0,
			int 	*npin1, 
			float 	*xin1, 
			float 	*yin1, 
			int 	*maxnpo,
			int 	*npo, 
			float 	*xo, 
			float 	*yo, 
			int 	*iret );

void	cgr_qsol ( 	int	np,
			float	*xx, 
			float	*yy,
			float	fx,
			float	fy,
			int	*lor,
			int	*aob,
			int	*iret );
 
void    cgr_range (     char 	*syspts, 
		        int 	*npts,
		        float 	*x,
		        float 	*y,
			int	*qpoly,
		        char 	*sysout,
		        int 	*rollout,
			int	*nout,
		        float 	*xout,
		        float 	*yout,
		        float 	*xll,
		        float 	*yll,
		        float 	*xur,
		        float 	*yur,
		        int 	*iret );

void    cgr_reducePts (	char	*opts,
		        int 	nin,
		        float 	*xin,
		        float 	*yin,
		        int 	*reduceFlg,
		        int 	*nout,
		        float 	*xout,
		        float 	*yPts,
		        int 	*isOrig,
		        int 	*iret );

void	cgr_reduceLinePoly(int 	nin, 
			float 	*xin,
                	float 	*yin, 
			float 	tolerance, 
			int 	*nout, 
			float 	*xout,
		        float 	*yout, 
			int 	*iret );

void    cgr_reorder (   int 	*npts, 
		        float 	*xin,
		        float 	*yin,
		        int 	*indx,
		        int 	*iret );

void    cgr_rolseg (    float   *x,
	                float   *y,
			float   *qx,
			float   *qy,
			int     *rol,
			int     *iret );
	
void	cgr_segdist ( 	int	*np,
		     	float	*xx,
		     	float	*yy,
		     	float	*fx,
		     	float	*fy,
		     	float	*distance,
		     	int	*nearest_vrt,
		     	int	*next_vrt,
		     	float   *nx, 
		     	float   *ny,
		     	int	*iret );

void    cgr_segint ( 	char	*sys1,
		 	float 	*xin1, 
		 	float 	*yin1,
		 	char	*sys2,
		 	float	*xin2,
		 	float	*yin2,
		 	char	*sys3,
		 	float	*xint,
		 	float	*yint, 
		 	int 	*intrsct,
		 	int 	*iret );

void    cgr_segintwn ( 	float 	*xin1, 
		 	float 	*yin1,
		 	float	*xin2,
		 	float	*yin2,
		 	float	*xint,
		 	float	*yint, 
		 	int 	*intrsct,
		 	int 	*iret );

void 	cgr_sphpolyarea ( int 	*npts, 
			float 	*lat, 
			float 	*lon,
			float 	*radius, 
			float 	*area, 
			int 	*iret );

void    cgr_to_rad (    double  deg,
	                double  *rd );

void	cgr_vectxprod (	double *va,
			double *vb,
			double *vc,
			int *iret );

void	cgr_qrol (	int *np,
			float *xl,
			float *yl,
			int *closed,
             		float *xp,
			float *yp,
			float *tol,
			int *rol,
			int *iret );

void	cgr_bisectpt (     float *x_line,
                        float *y_line,
                        float distance,
                        float *x, 
                        float *y,
                        int *iret);

void	cgr_csegint  (     float *xin1,
                        float *yin1,
                        float *xin2,
                        float *yin2, 
                        float *xint,
                        float *yint,
                        int *intrsct,
                        int *iret);

void	cgr_inpolywn (     const int points,
                        const float *pt_x,
                        const float *pt_y, 
                        const int polypoints,
                        const float *poly_x,
                        const float *poly_y, 
                        const int check_all,
                        int *inside,
                        int *iret);

void	cgr_objinpoly (    const int objpoints,
                        const float *obj_x,
                        const float *obj_y, 
                        const float *obj_bb, 
                        const int polypoints,
                        const float *poly_x,
                        const float *poly_y, 
                        const float *poly_bb, 
                        int *inside,
                        int *iret);

void	cgr_objint (       const int obj1_pts,
                        const float *obj1_x,
                        const float *obj1_y, 
                        const float *obj1_bb, 
                        const int obj2_pts,
                        const float *obj2_x,
                        const float *obj2_y,
                        const float *obj2_bb,
                        int *intersects, 
                        int *iret);

void	cgr_concave(       int points, 
                        const float *x, 
                        const float *y, 
                        int *concave, 
                        int *iret);

void    smpoly_rubberband ( int *npin0, float *xin0, float *yin0, int *npin1,
                            float *xin1, float *yin1, int *npo, float *xo,
                            float *yo, int *iret );

/*
 *  clo prototypes
 */

void    clo_bgall ( 	char	*name,
		 	int	maxbnd,
		 	int	*nbnd,
		 	char	*bndname,
		 	float	*clat,
		 	float	*clon,
		 	int	*nparts,
		 	char	*info,
		 	int	*iret );

void    clo_bgcent (    float	*clat,
		 	float	*clon,
		 	int	*iret );

void    clo_bginfo ( 	char	*name,
		 	int	ihot,
		 	char	*info,
		 	int	*iret );

void    clo_bgnext ( 	int	*minpts,
		 	int	*maxpts,
		 	float	*filt,
		 	int	*npts,
		 	float	*lat,
		 	float	*lon,
		 	int	*iret );

void    clo_bgrange (   float	*latll,
		 	float	*lonll,
		 	float	*latur,
		 	float	*lonur,
		 	int	*iret );

void    clo_binpoly ( 	char	*name,
			int	np,
			float	*x,
			float	*y,
			int	*iret );

void    clo_bofile ( 	char	*filename,
			int	*iret );

void    clo_bqinfo (    char    *info,
                        char    *keyword,
                        char    *value,
                        int     *iret );

void    clo_brdrec ( 	long	strec,
			int	*iret );

void    clo_bsarea ( 	float	*latll,
			float	*lonll,
			float	*latur,
			float	*lonur,
			int	*iret );

void    clo_bstag  (    char    *tag,
                        int     *iret );

void    clo_bstype ( 	char	*name,
			int	*iret );

void 	clo_cleanFmLine( const char	*fromLine, 
			int	lineType,
			char	*cleanFromLine, 
			int	*iret );

void 	clo_clip ( 	int 	*np, 
			float 	*xp, 
			float 	*yp, 
			char 	*sysp, 
			char 	*bounds,
                	char 	*name, 
			int 	*nclip, 
			int 	*maxpts, 
			int 	*iret );

void 	clo_clipdone ( 	int 	*iret );

void 	clo_clipget ( 	int 	*which, 
			int 	*npts, 
			float 	*xo, 
			float 	*yo, 
			int 	*iret );

void    clo_closest ( 	float	*lat, 
			float	*lon,
			int	npts,
			float	plat,
			float	plon,
			int	nclose,
			int	*order,
			int	*iret );

void    clo_cmpdir ( 	char	*cmpdir,
			float	*dir,
			int	*iret );

void    clo_cmpwds ( 	char 	*dir,
			int  	*idir,
			char 	*cmpdir,
			int  	*icmp,
			int  	*iret );

void    clo_compass ( 	float	*dir,
			char	*cmpdir,
			int	*icmp,
			int	*iret );

void    clo_dddec ( 	char    *locnam,
			int	*format,
			char    *string,
			int	*nexp,
			float   flat[],
			float   flon[],
			int	*nstn,
			int	*iret );

void    clo_ddenc ( 	char	*type,
			int	format,
			float   lat,
			float   lon,
			char    *str,
			int     *iret );

void    clo_direct ( 	float	*lat1,
			float	*lon1,
			float	*lat2,
			float	*lon2,
			float	*dir,
			int	*iret );

void    clo_dist ( 	float	*lat1,
			float	*lon1,
			int 	*np,
			float	lat2[],
			float	lon2[],
			float	dist[],
			int	*iret );

void    clo_dltln ( 	float	*alat,
			float	*alon,
			float	*dist,
			float	*dir,
			float	*blat,
			float	*blon,
			int	*iret );

void    clo_fastates (  char    *faarea,
                        char    *strin,
                        char    sep,
                        char    *ptype,
                        char    *strout,
                        int     *iret );

void	clo_findcwa  (	char	*locnam, 
			char	*cwa, 
			int	maxnum, 
			int	*nfips, 
			int	*fips, 
			int	*iret );

void    clo_finddesc ( 	char	*locnam,
			char	*xdesc,
			char	*xstate,
			int	srchtyp,
			int	maxlen,
			int	*nret,
			char	*info,
			int	*iret );

void    clo_findmatch ( char	*locnam,
			char	*name,
			char	*state,
			int	itype,
			int	srchtyp,
			int	maxlen,
			int	*nret,
			char	*info,
			int	*iret );

void    clo_findnum (   char    *locnam,
                        int     num,
                        int     maxlen,
                        int     *nret,
                        char    *info,
                        int     *iret );

void	clo_findstn ( 	char	*locnam,
			char	*xid,
			char	*xstate,
			int	srchtyp,
			int	maxlen,
			int	*nret,
			char	*info,
			int	*iret );

void    clo_from ( 	int	vgtype,
			int	reorder,
			int	npin,
			int	flag,
			float	*lat,
			float	*lon,
			int	maxchar,
			char	*str,
			int	*iret );

void    clo_init ( 	int	*iret );

G_Boolean clo_isCluster ( float	*lat1,
			float	*lon1,
			float	*lat2, 
			float	*lon2, 
			float	cdist );

void    clo_lonin ( 	float	*lon,
			int	nlon,
			float	lon1,
			float	lon2,
			int	maxhot,
			int	*hotlist,
			int	*nhot,
			int	*iret );

int     clo_qformat (   char	*name );


void    clo_qmxpts (	char	*which,
			int	*mxpts,
			int	*iret );

int     clo_qnhot ( 	void );

void	clo_reorder (	int     npin,
			float   *lat,
			float   *lon,
			int     *indx,
			int     *iret);

void 	clo_snapPoly (	G_Boolean expandOnly, 
			float	tolerance,
			G_Boolean reorder,
			G_Boolean closed,
			int	nin,
			float	*inLat,
			float	*inLon,
			int	*nout,
			float	*outLat,
			float	*outLon, 
			int	*iret );

void	clo_snapPt (	int	indx,
			float	lat,
			float	lon,
			int	indx2,
			int	npts,
			float	smearLat[],
			float	smearLon[],
			G_Boolean expandOnly,
			float	tolerance,
			float	*snapLat,
			float	*snapLon,
			int	*iret );

void	clo_snapPtGFA ( int	indx,
			float	lat,
			float	lon,
			int	indx2,
			int	ncheck,
			float	*checkLat,
			float	*checkLon,			
			int	npts,
			float	smearLat[],
			float	smearLon[],
			G_Boolean checkDist,
			G_Boolean expandOnly,
			float	tolerance,
			float	*snapLat,
			float	*snapLon,
			int	*iret );

void 	clo_snapOnePt ( float 	inLat, 
			float 	inLon, 
			float 	*snapLat,
		     	float 	*snapLon, 
			int 	*iret );

void    clo_sortbnd (   char	*locnam,
			int	icol,
			int	*iret );

void    clo_sortstn ( 	char	*locnam,
			int	icol,
			int	*iret );

void    clo_stngall ( 	char	*name,
			int	maxstn,
			int	*nstn,
			float	*lat,
			float	*lon,
			char	desc[][32],
			char	state[][3],
			char	stnid[][9],
			int	*stnnm,
			char	country[][3],
			int 	*elv,
			int	*pri,
			char	c10[][20],
			int	*iret );

void    clo_tclosest ( 	char	*name,
			float	platx,
			float	plonx,
			int	nclose,
			int	*iret );

void    clo_tmatch ( 	char	*name,
			float	platx,
			float	plonx,
			float	tol,
			int	*iret );

void    clo_tdirect ( 	char	*name,
			float	lat,
			float	lon,
			char	*stn,
			float	*dist,
			float	*dir,
			int	*iret );

void 	clo_tgid ( 	char	*name,
			int	maxids,
			int	maxchar,
			int	*nids,
			char	*id,
			int	*iret );

void    clo_tgltln (    char	*name,
			int	maxltln,
			int	*nltln,
			float	*lat, 
			float	*lon,
			int	*iret );

void    clo_tgnm ( 	char	*name,
			int	maxnms,
			int	maxchar,
			int	*n_nms,
			char	*nm,
			int	*iret );

void    clo_tgparm ( 	char	*name,
			int	icol,
			int	maxret,
			char	*sep,
			int	*nret,
			char	*strout,
			int	*iret );

void    clo_tgst ( 	char	*name,
			int	maxlen,
			int	*nst,
			char	*states,
			int	*iret );

void    clo_times ( 	fdttms_t  start,
			int	inc,
			int	ntimes,
			fdttms_t  *fdttms,
			int	*iret );

void    clo_tinltln ( 	char	*name,
			float	lat1, 
			float	lat2, 
			float	lon1,
			float	lon2,
			int	*iret );

void    clo_tinpoly (   char	*name,
			char	*sysin,
			int	npoly,
			float	*x, 
			float	*y,
			int	*iret );

void    clo_tqbnd ( 	char	*name,
			float	plat,
			float	plon,
			char	*strout,
			int	*iret );

void    clo_track (	float	lat1,
			float	lon1,
			fdttms_t  time1,
			float	lat2,
			float	lon2,
			fdttms_t  time2,
			int	inc,
			int	ntimes,
			float	*spd,
			float	*dir,
			float	*lat,
			float	*lon,
			fdttms_t  *fdttms,
			int	*iret );

int     clo_which ( 	char	*type );

/*
 *  cpcg prototypes
 */

void    cpcg_newpoly (  int     *np1,
                        float   xp1[],
                        float   yp1[],
                        int     *np2,
                        float   xp2[],
                        float   yp2[],
                        int     *np3,
                        float   xp3[],
                        float   yp3[],
                        int     *iret);

void	cpcg_rdbnd (	char    *bndtyp,
			int     *nbnd,
			int     npts[],
			float   blat[][LLMXPT],
			float   blon[][LLMXPT],
			int     *iret);

void	cpcg_rdln (	char    *fname,
			int     *nlin,
			int     npts[],
			int     lcolr[],
			int     clos[],
			float   llat[][MAXPTS],
			float   llon[][MAXPTS],
			int     *iret);

void	cpcg_srch (	char    *tblnam,
			char    *fname,
			int     *nstn,
			int     *istnm,
			int     *iflag,
			int     *iclr,
			int     *iret);

/*
 *  crg prototypes
 */

void    crg_build (     char *ifname, 
                        int *layer, 
                        int *iret );

void    crg_clear( 	int	elnum,
			int     *iret );

void    crg_clearLayer( int	layer,
			int	*iret );

void    crg_clroffst ( 	int	joffset,
			int	*iret );

void    crg_deselect (  float	*llx,
			float	*lly,
			float	*urx,
			float	*ury );

void	crg_gassocrec ( int	elnum,
			int	*assocRec,
			int	*iret );

void    crg_gbnd (	char	sys_in[],
			char	sys_out[],
			int	nsp,
			float	sx[],
			float	sy[],
			float	*llx,
			float	*lly,
			float	*urx,
			float	*ury,
			float	*ccx,
			float	*ccy );

void    crg_get ( 	int		elnum,
			int		*layer,
			filter_t	dsplyFilter,
			float		*llx,
			float		*lly,
			float		*urx,
			float		*ury,
			int     	*iret );

void    crg_getfilter ( int		elnum,
			filter_t	filter,     
			int     	*iret );

void    crg_getinx ( 	int	joffset,
			int	*elnum,
			int     *iret );

int     crg_getLayer ( 	int	joffset );

void    crg_getsecrange ( int           elnum,
                        float           *llx,
                        float           *lly,
                        float           *urx,
                        float           *ury,
                        int             *iret );

void    crg_ggbnd ( 	char   	grptyp, 
			int    	grpnum,
			float  	*rleft, 
			float  	*rright, 
			float  	*rtop, 
			float  	*rbottom, 
			int    	*iret );

void    crg_gginx ( 	char 	grptyp, 
			int  	grpnum,
			int  	nexp, 
			int  	*inxarry, 
			int  	*nelm, 
			int  	*iret );

void    crg_ggnel ( 	char 	grptyp, 
			int  	grpnum,
			int  	*nelm, 
			int  	*iret );

void crg_ggnhl ( 	char 	grptyp, 
			int 	*high_grpnum, 
			int 	*low_grpnum, 
			int  	*iret );

void    crg_ggnxt ( 	char 	grptyp,
			int  	*grpnum,
			int 	*iret );

void    crg_ggrp ( 	int  	elnum,
			char 	*grptyp,
			int  	*grpnum,
			int  	*iret );

void    crg_goffset ( 	int	elnum,
			int	*joffset,
			int     *iret );

void    crg_goffsets ( 	int 	vg_class,
			int 	vg_type,
			int	layer,
			int	offsets[],
			int	*num_offsets );

void    crg_grfrsh (	float	in_llx,
			float	in_lly,
			float	in_urx,
			float	in_ury,
			float	*llx,
			float	*lly,
			float	*urx,
			float	*ury,
			int	*iret );

void    crg_gsel ( 	int	elnum,
			char	*selected,
			int     *iret );

void    crg_gtyp ( 	int  	elnum,
			char 	*vg_class,
			char 	*vg_type,
			int  	*iret );

void    crg_init ( 	int	*iret );

G_Boolean crg_isauxrec (  int	elnum,
			int     *iret );

void    crg_lkattr (  	int	elnum,
			int     *iret );

void	crg_mvallayer ( int	layer,
			G_Boolean *moved,
			int	*iret );

void    crg_newinx (  	int	*elnum,
			int     *iret );

void    crg_rebuild ( 	void );

void    crg_rngpt ( 	float	mx,
			float	my,
			float	*llx, 
			float	*lly,
			float	*urx,
			float	*ury,
			int     *iret );

void    crg_sarec ( 	int	index,
			int	assocRec,
			int     *iret );

void    crg_save ( 	int	elnum,
			int	joffset,
			float	llx,
			float	lly,
			float	urx,
			float	ury,
			int     *iret );

void    crg_setauxrec ( int	elnum,
			int     *iret );

void    crg_setLayer ( 	int elnum, 
			int layer,
			int *iret );

void    crg_setltln ( 	int	elnum,
			int	joffset,
			int	np,
			float	lat[],
			float	lon[],
			int     *iret );

void 	crg_sgrp ( 	int  	elnum,
			char 	grptyp, 
			int  	grpnum,
			int  	*iret );

void 	crg_soffset ( 	int	elnum,
			int	joffset,
			int     *iret );

void 	crg_ssel ( 	int	elnum,
			char	selected,
			int     *iret );

void 	crg_styp ( 	int  	elnum,
			char 	vg_class,
			char 	vg_type,
			int  	*iret );

/*
 *  css prototypes
 */

void  	css_date ( 	int	*itype,
			int	*iyear,
			int	*imon,
			int	*iday,
			int	*ihour,
			int	*imin,
			int	*isec,
			int	*julian,
			char    *zone,
			int	*iret );

void 	css_envr ( 	const char	*filnam, 
			char 		*file, 
			int 		*iret );

void 	css_evtadvtime ( 	time_t 	*evtelapse, 
				int 	*iret );

void 	css_evtclear( 		int 	*iret );

void 	css_evtdumpvars( 	void  );

void 	css_evtgetinittm(	char 	*envtime, 
			  	int 	*iret );

void 	css_evtgetspeed( 	float 	*evtrate, 
				float 	*sysrate, 
				int 	*iret );

void 	css_evtison( 		G_Boolean *clockon, 
				int 	* iret );

void 	css_evtpause( 		int 	*iret );

void 	css_evtresume( 		int 	*iret );

void 	css_evtsetcurtm( 	char 	*ecurtm, 
				int 	*iret );

void 	css_evtsetinittm( 	char 	*envtime, 
				int 	*iret );

void 	css_evtsetspeed( 	float 	*evtrate, 
				float 	*sysrate, 
				int 	*iret );

void    css_gtim ( 	int	*itype,
			char	*dattim,
			int	*iret );

void 	css_mtyp ( 	int	*mchtyp,
			int	*iret );
/*
 *  cst prototypes
 */

void 	cst_abbr ( 	char	*str,
			char	*abbr,
			int	*flag,
			int	*iret );

void 	cst_alnm ( 	char	ch,
			int	*type,
			int	*iret );

void 	cst_cins ( 	char    *insptr,
			char     ch,
			int	*iret );

void	cst_clst ( 	char	*instr,
			char	sepr,
			char	*def,
			int	nexpv,
			int	maxchr,
			char	**aryptr,
			int	*numstr,
			int	*iret );

void 	cst_crnm ( 	char	*str,
			float	*value,
			int	*iret );

void	cst_ctod (	char	*str,
			double	*dblptr,
			int	*iret);

void 	cst_ctoi ( 	char	**str,
			int	nstr,
			int	*intptr,
			int	*iret );

void 	cst_ctrl ( 	char	*str,
			char	*outstr,
			int	*outlen,
			int	*iret );

void 	cst_find ( 	const char *str,
			const char **slist,
			int	   nstr,
			int	   *pos,
			int	   *iret );

void 	cst_flst ( 	char	*str,
			char	sep,
			char	*defstr,
			int	nexp,
			int	maxchr,
			char	**namptr,
			int	*num,
			int	*iret );

void 	cst_gtag ( 	const char 	*tag,
			const char 	*str,
			const char 	*def,
			char	*data,
			int	*iret );

void 	cst_ilst ( 	char	*str,
			char	sep,
			int	def,
			int	nexp,
			int	*intptr,
			int	*num,
			int	*iret );

void 	cst_inch ( 	int	intg,
			char	*str,
			int	*iret );

void 	cst_itoc ( 	int	*intary,
			int	nval,
			char	**strptr,
			int	*iret );

void 	cst_itos ( 	int	*intptr,
			int	nval,
			int	*nchar,
			char	*str,
			int	*iret );

void 	cst_lcuc ( 	char	*str,
			char	*outstr,
			int	*iret );

void 	cst_ldsp ( 	char	*str,
			char	*outstr,
			int	*nc,
			int	*iret );

void 	cst_lstr ( 	char	*str,
			int	*nc,
			int	*iret );

void 	cst_ncat ( 	char	*str1,
			char	*str2,
			int	*len,
			int	*iret );

void 	cst_ncpy ( 	char		*str1,
			const char	*str2,
			int		len,
			int		*iret );

void    cst_nocc ( 	const char	*str,
			char		srchc,
			int		fndocc,
			int		sstate,
			int		*nocc,
			int		*iret );

void 	cst_numb ( 	char	*str,
			int	*intg,
			int	*iret );

int 	IsMetaChar ( 	char	ch );

void	cst_padString ( const char	*inString, 
			char		fillChar,
			int		fillDir, 
			int		outLength,
			char		*outString );

void	cst_ptmt ( 	const char	*str, 
			const char	*pattern,
			int		*match, 
			int		*iret );

void 	cst_rang ( 	char	*str,
			char	*first,
			char	*last,
			char	*inc,
			int	*type,
			int	*iret );

void 	cst_rlch ( 	float	flnum,
			int	np,
			char	*str,
			int	*iret );

void 	cst_rlst ( 	char	*str,
			char	sep,
			float	def,
			int	nexp,
			float	*fltptr,
			int	*num,
			int	*iret );

void 	cst_rmbl ( 	char	*str,
			char	*outstr,
			int	*length,
			int	*iret );

void 	cst_rmst ( 	char	*str,
			char	*substr,
			int	*pos,
			char	*outstr,
			int	*iret );

void	cst_rmtag(	const char	*tag,
			char		*str,
			int		*iret );

void 	cst_rnan ( 	char    *string,
			char    *outstr,
			int     *iret );

void 	cst_rpst ( 	const char	*string,
			const char	*substr,
			const char	*repstr,
			char		*outstr,
			int		*iret );

void 	cst_rxbl ( 	char	*str,
			char	*outstr,
			int	*length,
			int	*iret );

void	cst_sort (	int 	itype, 
			int 	*nstr, 
			char 	**inpstr, 
			int 	*nout,
	        	char	**outstr, 
			int 	*iret );

char 	*cst_split ( 	char	*pstart,
			char	delim,
			int	maxchar,
			char	*result,
			int	*iret );

void 	cst_srch ( 	int	ibeg,
			int	iend,
			char	*string,
			char	*text,
			int	*ipos,
			int	*iret );

void 	cst_stag ( 	const char 	*tag,
			const char 	*tagvalue,
			int	*slen,
			char	*str,
			int	*iret );

void 	cst_stoi ( 	char	*str,
			int	nchar,
			int	*nval,
			int	*intptr,
			int	*iret );

void 	cst_tims ( 	int     ntimes,
			char	*timestr,
			dttms_t	tmarry[],
			int	*iret );

void 	cst_uclc ( 	char	*str,
			char	*outstr,
			int	*iret );

void 	cst_unpr ( 	char 	*string,
    			char 	*outstr,
    			int	*iret );

void 	cst_wrap ( 	char	*str,
			const char *sep,
			const int  *ilen,
			const char *eol,
			const char *newLineStr,
			char	*outstr,
			int	*iret );

void	cst_rspc (	char	*string,
			int	*iret );

void	cst_opcl (	const char *instr,
			const char *locopn,
			char **loccls,
			int *iret );

void	cst_zpad (	char *strin,
       			int *nchar,
		       	char *strout,
		       	int *iret );

/*
 *  ctb prototypes
 */


void 	ctb_afos ( 	char	*pil,
			char	*descr,
			int	*kx,
			int	*ky,
			int	*kmap,
			int	*ktime,
			int	*kgscl,
			int	*iret );

void 	ctb_airmetGetIssueTm( char      *cycleTm,
                        char            *issueTm,
			int             *iret );

void 	ctb_airmetGetCycleTms( G_Boolean  isDST,
                        int             *ntimes,
                        char            ***times,
                        int             *iret );

void 	ctb_astn ( 	char	*tblnam,
			char	*dirsym,
			int	*maxstn,
			int	*nstn,
			char	stid[][9],
			char	stnnam[][33],
			int	*istnm,
			char	cstat[][3],
			char	coun[][3],
			float	*slat,
			float	*slon,
			float	*selv,
			int	*ispri,
			char	tbchrs[][21],
			int	*iret );

void 	ctb_awdef ( 	char	*pil,
			char    *awpid,
			char    *kloc,
			char    *flcls,
			char    *prodid,
			char    *extnd,
			char    *wmo,
			char    *bckgnd,
        		int     *rettim,
			int	*kmap,
			int     *crdflg,
			int     *sclint,  
			int     *sclfac,
			int     *areacd,
			int     *label,
			int     *cordfg,  
			int     *nrefpt,
			int     *ktime,
			int     *pick,
        		int     *iret );

void 	ctb_awmap ( 	int     pick,
			int     *kxsize,
			int     *kysize,
			int     ilat[ ],
			int     ilon[ ],  
			int     iangles[ ],
			int	*iret );

void    ctb_ccfind (    int     fips,
                        char    *ccwfo,
                        char    *ccname,
                        int     *ncfips,
                        int     *cfips,
                        int     *iret );

void	ctb_dcatitos (	int	*catnum,
			char	*catgry,
			int	*iret );

void	ctb_dcatstoi (	char	*catgry,
			int	*catnum,
			int	*iret );

void	ctb_dhrsitos  ( int     *ionoff,
			int	*hrsbfr,
			int	*mnsbfr,
			int	*hraftr,
			int	*mnaftr,
			int	*mstrct,
			char	*binhrs,
			int	*iret );

void	ctb_dhrsstoi  ( char    *binhrs,
			int	*ionoff,
			int	*hrsbfr,
			int	*mnsbfr,
			int	*hraftr,
			int	*mnaftr,
			int	*mstrct,
			int	*iret );

void	ctb_dscatitos (	int	*scatnum,
			char	*scatgry,
			int	*iret );

void	ctb_dscatstoi ( char	*scatgry,
			int	*scatnum,
			int	*iret );

void 	ctb_dtcat ( 	char	*alias_i,
			int	*catgry,
			int	*subcat,
			int	*iret );

void 	ctb_dtdump ( 	int	*iret );

void 	ctb_dtget ( 	char	*alias_i,
			char	*path,
			char	*template,
			int	*catgry,
			int	*subcat,
			int	*nframe,
			int	*range,
			int	*intrvl,
			int	*ionoff,
			int	*hrsbfr,
			int	*mnsbfr,
			int	*hraftr,
			int	*mnaftr,
			int     *mstrct,
			int     *dtmmtch,
			int	*iret );

void 	ctb_dtlist (	int	*catgry,
			char	*list,
			int	*count );

void 	ctb_dtpath ( 	char	*alias_i,
			char	*path,
			int	*iret );

void 	ctb_dtrd ( 	int	*iret );

void 	ctb_dttime ( 	char	*alias_i,
			int	*nframe,
			int	*range,
			int	*intrvl,
			int	*iret );

void 	ctb_dttmpl ( 	char	*alias_i,
			char	*template,
			int	*iret );

void 	ctb_fszfnd ( 	float   size,
			int 	*which,
			int	*iret );
			
void 	ctb_fsznam ( 	int     indx,
			char    *name,
			int	*iret );

void 	ctb_fszqn ( 	int     *ntotal,
			int	*iret );

void 	ctb_fszrd ( 	int	*iret );

void 	ctb_fszval ( 	int     indx,
			float   *value,
			int	*iret );

void 	ctb_fszxvl ( 	int     indx,
			int     *xfont,
			int     *mult,
			int	*iret );

void 	ctb_g2gnam ( 	int 	*discpln, 
			int 	*categry, 
			int 	*number,
                        int 	*pdnumber,
                  	char 	*g2name,  
                        int	*ihzrmp,
                        int	*idrct,
			int 	*iret );

void 	ctb_g2gnum ( 	char 	*g2name,
                  	int 	*discpln, 
			int 	*categry, 
			int 	*number, 
                        int     *pdnumber,
			int 	*iret );

void 	ctb_g2read ( 	int 	*iret );

void 	ctb_gfaCmpSeverity (	char *hazard, 
				char *descriptor,
				char *value1,
			  	char *value2, 
				char *sevValue, 
				int *iret );

void ctb_gfaCombineIFRTypes(    const char *inType0, 
				const char *inType1, 
				char *outType, 
				int *iret );

void ctb_gfapCombineIFRTypes(    const char *inType0, 
				const char *inType1, 
				char *outType, 
				int *iret );

G_Boolean ctb_gfaHasCV (		void );

void ctb_gfaWorstFL (		int		topBottom,
				const char	*level0,
				const char	*fzlBottom0,
				const char	*fzlTop0,
				const char	*level1,
				const char	*fzlBottom1,
				const char	*fzlTop1,
				char		*worstFL,
				char		*worstFzlBottom,
				char		*worstFzlTop,
				int		*ier );

void ctb_gfaWorstIssue ( 	int 		nEl,
				char	 	**elIssue, 
                                char 		*worstIssue, 
				int		*ier );

void 	ctb_gfagcat ( 	char 	*haz, 
			char 	*cat, 
			int 	*iret );

void    ctb_gfagdc (    char    *haz,
                        int     *which,
                        int     *choice,
                        char    *str,
                        int     *iret );

void    ctb_gfagdesc (  char    *haz,
                        int     *which,
                        char    *type,
                        char    *desc,
                        int     *nchoices,
                        int     *iret );

void    ctb_gfagfhr (   char    *sep,
                        int     *nfhr,
                        char    *fhrlist,
                        int     *iret );

void    ctb_gfaghaz (   char    *sep,
                        int     *nhaz,
                        char    *hazlist,
                        int     *iret );

void    ctb_gfagid (    char    haz[],
                        char    hazId[],
                        int     *iret );

void    ctb_gfagiss (	char	*sep,
			int	*nissOpt,
			char	*issOptlist,
			int	*iret );

void    ctb_gfagndesc ( char    *haz,
                        int     *ndesc,
                        int     *iret );

void    ctb_gfagdesk (  char    *sep,
                        int     *ndesk,
                        char    *desklist,
                        int     *iret );

void 	ctb_gfagtag ( 	char 	*sep, 
			int 	*ntag, 
			char 	*taglist, 
			int 	*iret );

void 	ctb_gfagtemp ( 	char 	*fileName, 
			int 	*iret );
			
void    ctb_gfard (     int     *iret );

void    ctb_lygetcmode (int     ly,
                        char    *cmod,
                        int     *iret );
        
void    ctb_lygetcolor (int	ly,
			int	*icolr,
			int	*iret );

void	ctb_lygetdsply (int 	ly,
			char	*fmode,
			int	*iret );

void    ctb_lygetfile ( int     ly,
                        char    *file,
                        int     *iret );

void    ctb_lygetoutfile( int   ly,
	                  char  *outfile,
			  int   *iret );

void    ctb_lygetfmode ( int    ly,
                         char   *fmode,
                         int    *iret );

void    ctb_lygetgrptyp ( int    ly,
                         char   *grptyp,
                         int    *iret );

void    ctb_lygetname ( int     ly,
			int	namlen,
                        char    *name,
                        int     *iret );

void    ctb_lyrd (      char    *tblnam,
                        int     *ntoply,
                        int     *iret );

void 	ctb_mzgnm ( 	char 	*stid, 
			char 	*fulnam, 
			int 	*iret );

void    ctb_permccfind (int     fips,
                        char    *pcwfo,
                        char    *pcname,
                        int     *ncfips,
                        int     *cfips,
                        int     *iret );

void 	ctb_pldump ( 	int	*iret );

void 	ctb_plgcc ( 	char	*dtype_i,
			char	*alias_i,
			char	*colcod,
			int	*iret );

void 	ctb_plget ( 	char	*dtype_i,
			char	*alias_i,
			char	*parm_i,
			char	*str,
			int	*iret );

void 	ctb_pllist ( 	char	*dtype_i,
			int	maxals,
			char	list[][16],
			int	*nele,
			int	*iret );

void 	ctb_plrd ( 	int	*iret );

void 	ctb_plsdef ( 	char    *dtype_i,
			char    *alias_i,
			char    *parm_i,
			int	*iret );

void 	ctb_plset ( 	char	*dtype_i,
			char	*alias_i,
			char	*parm_i,
			char	*str,
			int	*iret );

void 	ctb_prmt (  	char	*type,
			int	*num,
			char	alias[][73],
			char	vcord[][5],
			int	level[],
			int	*iret );

void 	ctb_prod (	char	*wheel,
			char	*subset,
			int	maxsub,
			int	*nsub,
			char	subout[][5],
			char	descr[][41],
			int	kbit[],
			int	klin[],
			int	krot[],
			int	kind[],
			int	krsv[],
			int	*iret );

void 	ctb_tiff ( 	char	wmoid[],
			char	descr[],
			int	*kbit,
			int	*klin,
			int	*krot,
			int	*iret );

void 	ctb_trkfnd ( 	int     *intv,
			int	*indx,
			int     *iret );

void 	ctb_trkitv ( 	int	max_itvs,
			char    *intv[],
			int     *iret );

void 	ctb_trkqn ( 	int     *nintv,
			int     *iret );

void    ctb_pfread (    int     *iret );

void    ctb_pfstr  (    char    *tag, 
                        char    *cval, 
                        int     *iret );

void    ctb_pfbool (    char    *tag, 
                        G_Boolean *bval, 
                        int     *iret );

void	ctb_rdcpf (	char	*cpfname,
			int	*np, 
			float	*cplat,
			float	*cplon,
			int	*iret );

void	ctb_rdprf (	char	*tblnam,
			char	*dirsym,
 			char 	*tag, 
			char 	*value, 
			int 	*iret );

void	ctb_wrcpf (	char    *cpfname,
			int	np,
			float	*cplat,
			float	*cplon,
			int	*iret );

void    ctb_hfdump (    int     *iret );
void    ctb_hfread (    int     *iret );
void	ctb_hfgetfont ( int	*fn,
       			int	*mxpt,
		       	int	*nc,
		       	int	*ascv,
	                int	*ixmin,
		       	int	*ixmax,
			int	*npnts,
		       	int	*ixc,
		       	int	*iyc,
		       	int	*iret );

/*
 *  cvg prototypes
 */

void    cvg_c3e3f (     char            *fname,
                        int             *iret );

void    cvg_c3i3h (     char            *ifname,
                        char            *ofname,
                        int             *iret );

void    cvg_c3k3j (     char            *ifname,
                        char            *ofname,
                        int             *iret );

void    cvg_clearplace (int             *iret );

void    cvg_clos (      FILE            *fp,
                        int             *iret );

int     cvg_cp (        char            *ifname,
                        char            *ofname,
                        int             initflag,
                        int             *iret );

void    cvg_crelm (     char            *fname,
                        int             *iret );

void    cvg_crvgf (     char            *fname,
                        int             *iret );

void    cvg_deall (     char            *fname,
                        int             layer,
                        G_Boolean       inc_place,
                        int             *iret );

void    cvg_delet (     char            *fname,
                        int             offset,
                        G_Boolean       inc_place,
                        int             *iret );

void    cvg_drawLayer ( FILE		*fp,
			char            *fname,
                        int             layer,
                        int             fsize,
                        int             *iret );

void    cvg_fscan (     char            *fname,
                        int             layer,
                        char            class,
                        char            sel_grpd,
                        int             fpts,
                        float           fx[],
                        float           fy[],
                        int             *num_sel,
                        int             offsets[],
                        int             *iret );

void    cvg_getfilter ( int             *filnum,
			filter_t        filter[],
                        int             *iret );

void    cvg_getfname (  char            *fname,
                        int             *iret );

char	*cvg_getoutdir( char		*prefsTag,
			char		*filename );

void    cvg_gettblfilter( int           *filnum,
			filter_t        filter[],
                        int             *iret );

char	*cvg_getworkfile ( void );

void	cvg_gtgnum (	char		*fname,
			FILE		*fp,
			char		grptyp,
			long		size,
			int		*grpnum,
			int		*iret );

void    cvg_initplace ( int             *iret );

void	cvg_load (      char            *fname,
			G_Boolean      	selflag,
			G_Boolean     	wrtflg,
			G_Boolean      	plotflg,
			int             layer,
			int             icol,
			int             *iret );

void    cvg_load2 (     char            *fname,
                        int             icol,
                        int             *iret );

void    cvg_matchfilter( filter_t	el_filter,                        
			G_Boolean	matchAny,
                        G_Boolean      	*match,                        
			filter_t	timeMatched,
			int		*iret );

void	cvg_open (      char            *filnam,
                        int             wrtflg,
                        FILE            **fptr,
                        int             *iret );

int 	cvg_plenabled (void);

void	cvg_plrtbl (	int		*iret);

void    cvg_qkopen (    char            *filnam,
                        FILE            **fptr,
                        int             *bytes_inFile,
                        int             *iret );

void    cvg_rdfilter (  int             *iret );

void	cvg_rdgtn (	char		*fname,
			FILE		*fp,
			long		*size,
			int		istoff,
			char		gptyp,
			char		vgclss,
			char		vgnum,
			int		sbtyp,
			int		*ieloff,
			int		*gpnum,
                        int             *iret );

void    cvg_rebuildplace (	char		*fname,
				int             *iret );

void 	cvg_rebun ( 	int 		*nfips, 
			int 		fipsin[], 
			int 		*expand, 
			int 		*debug,
        		char 		*bndnam, 
			int 		*npout, 
			float 		*latout, 
			float 		*lonout,
        		int 		*iret );

void    cvg_redraw (    char            *fname,
                        int             *iret );

void    cvg_rest ( 	void );

void    cvg_rfrsh (     char		*fname,
                        float		dllx,
                        float		dlly,
                        float		durx,
                        float		dury,
                        int		*iret );

void    cvg_rfrshLayer ( FILE		*fp,
			char            *fname,
                        int             layer,
                        int             fsize,
                        float		llx,
                        float		lly,
                        float		urx,
                        float		ury,
                        int		*iret );

void    cvg_rndef ( 	void );

void    cvg_setfilter ( char            *filter,
                        int             *iret );

void    cvg_setginf (   char            *fname,
                        int             fpos,
                        char            grptyp,
                        int             grpnum,
                        int             *iret );

void	cvg_srchgrp (	char		*vfname,
			FILE		*fp,
			long		*size,
			char		gptyp,
			int		gpnum,
			int		maxoff,
			int		members[],
			int		*numingrp,
			int		*iret );

void    cvg_svfhed (    char            *fname,
                        int             *iret );
 
void    cvg_updateplace (	G_Boolean    	place_all,
				int     	*iret );
 
void    cvg_undel (	char    	*fname,
			int     	offset,
			G_Boolean 	inc_place,
			int     	*iret );

void	cvg_valid (	char		*filnam,
			int             *iret );

int	cvg_getFlghtLvl(const char	*flightLvl );


/*
 *  cvq prototypes
 */

void	cvq_getginf ( 	char	*fname,
			int	fpos,
			char	*grptyp,
			int	*grpnum,
			int	*iret );

void 	cvq_higp ( 	char	*fname,
			int	*grpnum,
			int	*iret );

void 	cvq_nxtgnm ( 	char	*fname,
			char	 grptyp,
			int	*grpnum,
			int	*iret );

void 	cvq_scangrp (	char 	*vfname,
			char	gptyp,
			int 	gpnum,
			int     maxoff,
			int	members[],
			int	*numingrp,
			int 	*iret );

/*
 *  DM library prototypes
 */
void    dm_rpkgc (      int     *iflno,
                        int     *iiword,
                        int     *lendat,
                        int     *ipktyp,
                        int     *kxky,
                        int     *nbits,
                        float   *ref,
                        float   *scale,
                        int     *miss,
                        float   *difmin,
                        int     *kx,
                        float   *rdata,
                        int     *iret );

void    dm_pkgdc (      float   *rdata,
                        int     *nword,
                        int     *kword,
                        int     *ipktyp,
                        int     *nbits,
                        int     *miss,
                        int     *kxky,
                        int     *kx,
                        float   *difmin,
                        float   *ref,
                        float   *scale,
                        int     *iret );

void    dm_wpkgc (      int     *iflno,
                        float   *rdata,
                        int     *isword,
                        int     *kword,
                        int     *ipktyp,
                        int     *nbits,
                        int     *miss,
                        int     *kxky,
                        int     *kx,
                        float   *ref,
                        float   *scale,
                        float   *difmin,
                        int     *iret );

/*
 *  spf prototypes
 */
void	spf_clnup (	int	*iret );

void	spf_close (	FILE	*fptr, 
			int	*iret );
			
FILE	*spf_create (	char	*filnam,
			int	*hlen, 
			int	*iret );

void	spf_gtfld (	char	*tag,
			char	*data,
			int	*iret );
			
void	spf_init (	int	*iret );

void	spf_load (	char	*filnam,
			int	*iret );
			
void	spf_open (	char	*filnam,
			G_Boolean crt,
			FILE	**fptr,
			int	*flen,
			int	*iret );
			
void	spf_read (	FILE	*fptr,
			char	*filnam,
			int	flen,
			int	*iret );
			
void	spf_write (	FILE	*fptr,
			char	*tag,
			char	*data,
			int	*iret );

/*
 *  utf prototypes
 */

void 	utf_clos ( 	FILE 	*fp, 
			int 	*iret );

void 	utf_dtext ( 	int 	offflg, 
			int 	shift_x, 
			int 	shift_y, 
			int	 zm,
		 	unsigned char *endbuf, 
			unsigned char *ptr, 
			int 	*g,
		 	int 	*b, 
			int 	*rb, 
			int 	*zt, 
			int 	*pltfil, 
			int 	*zf,
		 	int 	*chrsiz, 
			int 	*ipnt, 
			int 	*jpnt, 
			int 	*len, 
			int 	*add,
		 	int 	*iret );

void 	utf_dump ( 	unsigned char *buffer, 
			int 	size, 
			int 	sbyte, 
			int 	nout,
			char 	*ans, 
			int 	zm, 
			int 	shift_x, 
			int 	shift_y,
			int 	gphflg, 
			int 	*iret );

void 	utf_dvctr ( 	int 	shift_x, 
			int 	shift_y, 
			unsigned char *ptr, 
			int 	*zd,
		 	int 	*zt, 
			int 	*zf, 
			int 	*ipnt, 
			int 	*jpnt, 
			int 	*len,
		 	int 	*add, 
			int 	*iret );

void 	utf_dvev ( 	unsigned char *ptr, 
			int 	*vdir, 
			int 	*zt, 
			int 	*zf,
			int 	*vlen, 
			int 	*ipnt, 
			int 	*jpnt, 
			unsigned int *bits,
			int 	*add, 
			int 	*iret );

void 	utf_gphgd ( 	unsigned char *ptr, 
			int 	siz, 
			int 	*shift_x, 
			int 	*shift_y,
		 	int 	*pi, 
			int 	*gs, 
			unsigned int *imax, 
			unsigned int *jmax,
		 	unsigned int *imaxad, 
			unsigned int *jmaxad, 
			int 	*day,
		 	int 	*month, 
			int 	*year, 
			int 	*itime,
			int 	*pdc, 
			int 	*add,
		 	int 	*iret );

void 	utf_open ( 	char 	*filnam, 
			int 	*fptr, 
			int 	*iret );

void 	utf_plot ( 	char 	*filnam, 
			int 	*zm, 
			int 	*iret );

void 	utf_ptext ( 	int 	offflg, 
			int 	shift_x, 
			int 	shift_y,
		 	unsigned char *endof, 
			int 	zm, 
			unsigned char *ptr,
		 	int 	*add, 
			int 	*iret );

void 	utf_pvctr ( 	int 	shift_x, 
			int 	shift_y, 
			unsigned char *ptr, 
			int 	*add,
		 	int 	*iret );

void 	utf_pvev ( 	unsigned char *ptr, 
			int 	*add, 
			int 	*iret );

void 	utf_read ( 	FILE	*fp, 
			long 	nbytes, 
			unsigned char *buffer,
			long 	*nbread, 
			int 	*iret );

void 	utf_size ( 	char 	*filnam, 
			long 	*nbytes, 
			int 	*iret );

void 	utf_strip ( 	unsigned char *buf, 
			long 	byts, 
			long 	*tot, 
			int 	*iret );

/*
 *  xml prototypes
 */

int xml_transform( 	const char	*xmlBuf,
                   	int		bufSize,
                   	char		*xsltFile,
			unsigned char 	**outDoc,
		        int 		*iret );

int xml_count( 		const char	*xmlBuf,
       			const char	*elementName, 
			int		elementNumber,
			const char	*childName,
			const char	*childValue, 
			int		*iret );

void xml_value(		const char	*xmlBuf,
       			const char	*elementName, 
		       	int 		elementNumber, 
			const char	*childName,
			int		childNumber,
			char		**value,
			int		*iret );

void xml_getSubDoc( 	const char	*xmlBuf,
       		    	const char	*elementName, 
		    	int		elementNumber,
			const char	*childName,
			unsigned char 	**xmlOut, 
			int		*iret );

void xml_readTable( 	const char 	*xmlTableFile,
		    	const char  	*xmlTableDir,
    		    	const char  	*parentElPath,
    		    	int		nfields,
    		    	const char	*delim,
    		    	char		***output,
       		    	int		*iret,
    		    	char		*fields,... );

/*
 * Scandir and alphasort not defined on Sun and Solaris
 */
#ifdef SunOS
extern int scandir (	const char *dirname, struct dirent ***,
			int (*)( const struct dirent *),
			int (*)( const struct dirent **,
			const struct dirent **));
extern int alphasort (	const struct dirent **, const struct dirent ** );
#endif
#ifdef Solaris
extern int scandir (	const char *dirname, struct dirent ***,
			int (*)( const struct dirent *),
			int (*)( const struct dirent **,
			const struct dirent **));
extern int alphasort (	const struct dirent **, const struct dirent ** );
#endif
#endif /* PROTO_CGEMLIB */
