/************************************************************************
 * proto_gemlib.h							*
 *									*
 * NOTE:  When adding entries, please maintain alpabetical order.	*
 *									*
 **									*
 * Log:									*
 * T. Piper/GSC		12/00	Created					*
 * S. Jacobs/NCEP	12/00	Added GB library public prototypes	*
 * A. Hardy/GSC          2/01   Added GETMAP				*
 * J. Wu/GSC            03/01   Added parameter 'ilogo' into 'glogo'	*
 * T. Piper/GSC		04/01	Added cv_rduc				*
 * A. Hardy/GSC          6/01   Added gg_wcur,fixed gg_wtch call. seq.	*
 * A. Hardy/GSC		 7/01	Added im_chtb				*
 * S. Jacobs/NCEP	 7/01	Added prnlon and pr_mskn		*
 * T. Lee/SAIC		10/01	Added fill types to gcfill calling seq.	*
 * D. Kidwell/NCEP	 6/02	Added gsgtgn                            *
 * A. Hardy/NCEP	 8/02	Added gg_csig				*
 * S. Jacobs/NCEP	 9/02	Added mv_itob				*
 * J. Wu/SAIC	 	 9/02	Added gplbnd				*
 * D.W.Plummer/NCEP	 3/03	Chgs for IM_BTOT, IM_TTOB, IM_GVTOTA	*
 * D.W.Plummer/NCEP	 3/03	Added im_gtmp				*
 * K. Brill/HPC		 5/03	Added dg_intl				*
 * M. Li/SAIC		 6/03	Added gg_wwfo				*
 * T. Piper/SAIC	 7/03	Added ip_svar				*
 * A. Hardy/NCEP	 8/03	Added ti_elcl				*
 * A. Hardy/NCEP	 8/03	Added gg_wcvf and fl_fpth		*
 * D.W.Plummer/NCEP	 9/03	Added gqgprj				*
 * M. Li/SAIC		10/03	Added ip_idnt, ip_dynm, ip_exit		*
 * M. Li/SAIC		11/03	Added im_cbar				*
 * J. Wu/SAIC	 	01/04	Added er_gnumerr & er_gerrmsg		*
 * R. Tian/SAIC		01/04	Changed dg_intl calling sequence	*
 * R. Tian/SAIC		02/04	Changed dg_intl calling sequence back	*
 * J. Wu/SAIC	 	02/04	Changed er_gerrmsg			*
 * B. Yin/SAIC		02/04	Added 4 gh functions			*
 * T. Piper/SAIC	03/04	Added various functions			*
 * T. Piper/SAIC	03/04	Removed cfdate and ss_gtim		*
 * B. Yin/SAIC		03/04	Changed gh_bksq calling sequence	*
 * m.gamazaychikov/SAIC	04/04	Added gh_bkpl, gh_bktb			*
 * B. Yin/SAIC		05/04	Added gh_ftbl				*
 * F. J. Yen/NCEP	06/04	Changed gg_qsrd calling sequence	*
 * D.W.Plummer/NCEP	06/04	Chg call seq of im_gtmp			*
 * m.gamazaychikov/SAIC 08/04   Changed call to gh_bkpl                 *
 * B. Yin/SAIC		08/04	Added gh_tctx, gh_wwbk, gh_wwtx		*
 * H. Zeng/SAIC		09/04	Added gg_wcck and gg_wwtp		*
 * T. Piper/SAIC	10/04	Changed calling sequence for csleep.c	*
 * m.gamazaychikov/SAIC	11/04	Added gg_wfps				*
 * T. Piper/SAIC	11/04	Changed Cardinal to size_t for 64-bit	*
 * D. Kidwell/NCEP	 2/05	Added gh_wwbl, changed gh_wwtx call seq *
 * S. Jacobs/NCEP	 2/05	Added gh_fstn				*
 * H. Zeng/SAIC		03/05	Added in_catminp&in_catmmap		*
 * D. Plummer/NCEP	03/05	Changed ggdriv arguments list		*
 * A. Hardy/NCEP	03/05   Added int* to gg_wcck			*
 * C. Bailey/HPC        03/05   Added ggsave                            *
 * M. Li/SAIC		04/05   Added 3 char* to gh_tctx		*
 * A. Hardy/NCEP	04/05   Added gg_cwcp and gg_wcpb		*
 * T. Piper/SAIC	04/05	Added sn_clos, sn_rstr, sn_sstn, sn_stim*
 * B. Yin/SAIC		04/05 	Added issuing status in gh_wwtx		*
 * T. Piper/SAIC	04/05	Added im_dump and im_icmn 		*
 * R. Tian/SAIC		04/05	Added oabsdr				*
 * F. J. Yen/NCEP        4/05   Added gg_wlso                           *
 * D. Kidwell/NCEP	 4/05	Changed gh_wwbk call seq, 275 -> 605    *
 * H. Zeng/SAIC		05/05	Added 5 in_xxxx functions		*
 * M. Li/SAIC		05/05	Changed ggdriv argument list		*
 * G. Grosshans/SPC	06/06	Added int* to gg_wwtp			*
 * F. J. Yen/NCEP        8/05   Changed gg_wlso                         *
 * M. Li/SAIC		 9/05	Removed gh_bkrd  and gh_bkus		*
 * D.W.Plummer/NCEP	 9/05	Added dg_nend and modified ggapsm	*
 * M. Li/SAIC		 9/05	Added gg_update				*
 * m.gamazaychikov/SAIC 09/05   Added ip_log                            *
 * D. Kidwell/NCEP      10/05   Added int* to gh_wwtx for adv num       *
 * S. Gilbert/NCEP      11/05   Added gh_0bkp                           *
 * m.gamazaychikov/SAIC 12/05   Added gg_rdvgf, changed gg_update       *
 * S. Gilbert/NCEP      01/06   Added gh_advn; changes gh_wwtx, gh_0bkp *
 * H. Zeng/SAIC		03/06	added conditional compiling in PR lib.  *
 * M. Li/SAIC		04/06	added in_discqleft			*
 * R. Tian/SAIC          4/06   Removed DG library prototypes           *
 * T. Piper/SAIC	07/06	Added gg_wsrd.c				*
 * R. Tian/SAIC		 8/06	Added inc_outt				*
 * R. Tian/SAIC		 8/06	Added grc_geni and grc_wnmc		*
 * D.W.Plummer/NCEP	 9/06	Added some DM library functions		*
 * R. Tian/SAIC		 9/06	Added gr_sscl,in_scal,ip_resp,tm_accp	*
 * R. Tian/SAIC		 9/06	Added tm_str				*
 * T. Piper/SAIC        09/06   Added im_qimg, im_rtbl, im_smgd         *
 * R. Tian/SAIC		10/06	Removed na_*, added gd_cref, gg_proj,	*
 *                              gr_algn, gr_mbn2, gr_suba		*
 * S. Gilbert/NCEP    10/06     Added gr_vnav                           *
 * S. Gilbert/NCEP    07/07     Set up GU library section		*
 * S. Gilbert/NCEP    10/07     Added gh_bkcal and gh_bkloc		*
 * E. Safford/SAIC	12/07	use G_Cardinal to rm X/Motif dependency *
 * E. Safford/SAIC	01/08	add prototypes for gmesg, gsatim        *
 * T. Piper/SAIC	01/08	Added gd_init				*
 * T. Piper/SAIC	03/08	Added new UNISYS navigation routines	*
 * m.gamazaychikov/SAIC 04/08   Changed gh_bkpl prototype adding size_t	*
 * J. Wu/SAIC           06/08   add "intensity" in inc_pgfatxt          *
 * J. Wu/SAIC           06/08   add "coverage" in inc_pgfatxt           *
 * J. Wu/SAIC           06/08   add "category/frequency" in inc_pgfatxt *
 * L. Hinson/AWC        06/08   Add cycle parameter to inc_pgfatxt      *
 * S. Gilbert/NCEP      06/09   Added gh_bkme				*	
 * L. Hinson/AWC        07/09   Add inc_pccftxt                         *
 * m.gamazaychikov/CWS  01/10   Add DB library prototypes, tb_nidsdb	*
 * X. Guo/CWS		04/10   Add im_nexbz function prototype         *
 * m.gamazaychikov/CWS  04/10   Add db_getduri, db_rdtr, db_getrdata,   *
 *                              db_getgnav, db_gtim                     *
 ***********************************************************************/

/* AW library */
void awrfil ( char fname[], int *nbytes, unsigned char barr[], int *iret );

/* CV library */
void cv_mdfy (	int     *npo,
		float   *xo,
		float   *yo,
		int     *npc,
		float   *xc,
		float   *yc,
		int     *sm_lvl,
		int     *oline,
		int     *maxpts,
		int     *np,
		float   *x,
		float   *y,
		int     *is,
		int     *ie,
		int     *iret );

void cv_prmt (	int*, float*, float*, float*, int*, float*, int*, 
		int*, int*, float*, float*, int *iret );

void cv_rduc (	int	*npts,
		float	*xpts,
		float	*ypts,
		float	*filt,
		int	*nout,
		float	*xout,
		float	*yout,
		int	*iret );

/* DG library */
void dgc_grid ( const char *gdattm, const char *glevel, const char *gvcord,
                const char *gfunc, char *pfunc, float *grid, int *igx,
                int *igy, char *time1, char *time2, int * level1,
                int *level2, int *ivcord, char *parm, int *iret );
void dgc_ndtm ( const char *gdatm, int *iret );
void dgc_nfil ( const char *gdfile, const char *gdoutf, int *iret );
void dgc_ntim ( const int *chngnv, const int *coladd, char *time1,
                char *time2, int *nxtok, int *iret );
void dg_intl ( int *iret );
void dg_nend ( int *iret );
void dg_onav ( const float *rnvblk, int *iret );

/* ER library */
void er_gerrmsg ( int *indx, char *msg, int *iret );
void er_gnumerr ( int *errnum, int *iret );
void er_init ( int *iret );
void er_lmsg ( int*, char*, int*, char*, int*, size_t, ... );
void er_smsg ( char *string, int *iret );
void er_stat ( int*, int*, int*, int *iret );
void er_wbuf ( int *iret );
void er_wmsg ( char *errgrp, int *numerr, char *errstr, int *iret, size_t, size_t );

/* DB library */
void db_init ( int *iret );
void db_a2dtget 	( char *alias, char *path, char *template, int *iret );
void db_isdbfile        ( char *afile, int *iret,  size_t );
void db_scandb 		( const char *modelName, const char *dbTag, const char *ensTag,
                          const char *timeTmplt, char *fileNames, int *iret);
void db_seta2dtinfo 	( const char *alias, const char *path, const char *template,
                          int *iret );
int db_dataCallback     ( void (*callback)(char*) );
int db_navCallback      ( void (*callback)(char*) );
int db_fhrsCallback      ( void (*callback)(void) );
int db_duriCallback      ( void (*callback)(char*) );
int db_diagCallback      ( void (*callback)(char*) );
int db_flnmCallback      ( void (*callback)(char*) );
int db_subgCrsCallback   (void  (*callback)(char*));
void db_getparm         ( char *plgn, int *iret,  size_t );
void db_getnavtime      ( char *timeNav, int *iret,  size_t );
void db_getevtname      ( char *evtname, int *iret,  size_t );
void db_getcycle        ( char *cycle, int *iret,  size_t );
void db_getensmbrs      ( char *ensMbrsNames, int *iret, size_t );

void db_setnavtime      ( char *timeNav, int *iret, size_t );
void db_setevtname      ( char *evtname, int *iret, size_t );
void db_setensmbrs      ( char *ensMbrsNames, int *iret, size_t );
void db_wsetnavtime     ( const char *timeNav, int *iret );
void db_wsetevtname     ( const char *evtname, int *iret );
void db_getQueryText    ( char *queryType, char *queryText, int *iret );
void db_getduri         ( char *queryType, char *source, char *model, char *dattim,
                          char *vcord, char *parm, char *evtname, int *level, int *level2, char *dataURI,
                          int *lDataURI, int *iret );
void db_retsubgcrs ( char *prj, int kx, int ky,
                   float lllat, float lllon, float urlat, float urlon,
                   float angl1, float angl2, float angl3, int * iret );
void db_getgnav         ( char *model, char *eventName, char *navTime,char *gridNav, int *lGridNav, int *iret );
void db_gtimgrid        ( char *gridTimes, int *lGridTimes, int *iret );
void db_getRData        ( char *dbHost, char *fileName, float *rdata, int *nword,
                          int *iret );
int  db_gFileNames      ( const char *dir, int isrch, struct dirent ***ret_namelist);
int  db_gNextColumn     ( char *dir, struct dirent ***ret_namelist);
void db_gradhdr         ( char *dbHost, char *queryType, char *source,
                          char *icao, char *product, char *resolution, char *elAngle,
                          char *dbTime, char *satHdr, int *lSatHdr, int *iret );
void db_gradimg         ( char *dbHost, char *queryType, char *source, char *icao,
                          char *product, char *resolution, char *elAngle, char *dbTime,
                          int *iret );
void db_gsathdr         ( char *dbHost, char *queryType, char *source,
                          char *physEl, char *sectorID, char *resolution, char *dbTime,
                          char *satHdr, int *lSatHdr, int *iret );
void db_gsatimg         ( char *dbHost, char *queryType, char *source,
                          char *physEl, char *sectorID, char *resolution, char *dbTime,
                          int *iret );
void db_gtim            ( char *queryType, char *source, char *qparms,
                          char *times, int *ltimes, int *iret );
void db_rdtr            ( char *queryType, char *source, char *part, char *dattim,
                          char *stid, char *dataUri, int *icnt, float *rdata, int *nword,
                          int *iret );
void db_rdtrgrid        ( char *dimx, char *dimy, char *dataUri,
                          float *rdata, int *nword, int *iret );
void db_runQuery        ( char *queryText, char *queryResult,
                          int *iret );
void db_returndata	( float *result, int *nrez );
void db_returnnav	( char *result );
void db_returnfhrs	( char *result );
void db_returnduri	( char *result );
void db_returnflnm	( char *result );
void db_msgcave		( char *functionName, char *logLevel, char *message, int *iret );
void db_getstinfo       ( char *qry, char *gemarea, char *stinfo,
                          int *lstinfo, int *iret );
void db_setsubgnav ( float lllat, float lllon, float urlat, float urlon, int *irer );
/* DM library */
void dm_rint ( int*, int*, int*, int*, int *iret );
void dm_pkgd ( float*, int*, int*, int*, int*, int *, int*, int*, float*, float*, float*, int *iret );
void dm_wpkg ( int*, float*, int*, int*, int*, int*, int *, int*, int*, int*, float*, float*, float*, int *iret );
void dp_ugrb ( int*, int*, int*, float*, float*, int*, float*, int *iret );
void dp_unmc ( int*, int*, int*, float*, float*, float*, int *iret );
void dp_udif ( int*, int*, int*, float*, float*, float*, int*, int*, float*, int *iret );

/* FL library */
void fl_fpth ( char*, char*, char*, int *iret, size_t, size_t, size_t );

/* GB library */
void gb_clos (	int	*iret );

void gb_diag (	char	*gbdiag,
		int	*iret );

void gb_gbdh (	int	*length,
		int	*bdflag,
		int	*scale,
		float	*ref,
		int	*nbits,
		int	*iret );

void gb_ggds (	float	*gdsarr,
		int	*irltv,
		int	*nrmflg,
		int	*iret );

void gb_gpbd (	int	*kx,
		int	*ky,
		int	*iuscal,
		float	*ref,
		float	*scale,
		int	*nbits,
		int	*igrid,
		int	*lengrd,
		int	*iret );

void gb_gpds (	int	*itime,
		int	*iaccm,
		int	*ilevel,
		int	*ivcord,
		int	*iparm,
		int	*igrdnm,
		int	*icodtbl,
		int	*icenter,
		int	*isgds,
		int	*isbms,
		int	*ispdse,
		char	*cpds,
		int	*iclen,
		int	*iret );

void gb_gubd (	int	*kx,
		int	*ky,
		int	*iuscal,
		int	*misflg,
		float	*rmsval,
		int	*nrmflg,
		int	*irltv,
		int	*nbits,
		float	*fgrid,
		int	*iret );

void gb_next (	int	*ivers,
		int	*iret );

void gb_open (	char	*filnam,
		int	*lenfil,
		char	*idxnam,
		int	*lenidx,
		int	*iret );

void gb_read ( int *mxgrib, int *ipack, int *iret );

/* GD library */
void gdc_gcyc (char *gdfile, char *cycles, int *iret );
void gdc_gtmf ( char *gdfile, char  *cycle, char *availableTimes, int *iret );
void gdc_open ( char *gdfile, int *wrtflg, int *mxanl, int *mxnav,
                int  *iacss,  float *anl, float *rnav,
                int  *msxgrd, int *iret );
void gd_clos ( int *iacss, int *iret );
void gd_cref ( const char*, const int*, const float*, const int*, const float*,
               const int*, const int*, int*, int*, size_t );
void gd_geni ( char *gdfile, int *nlun, int *luns, int *iret, size_t );
void gd_gcyc ( char*, char*, int*, char*, int *iret, ... );
void gd_gtmf ( char *gdfile, char *gdatim, char *cycle, int *maxt, int *ngdftm,
                char gdtlst[][20], int *iret, ... );
void gd_init ( int *iret );
void gd_open ( char *, int *, int *, int *, int *, float *, float *, int *,
						int *iret, size_t);
void gd_wpgd ( int*, float*, int*, int*, int*, char*, int*, int*, char*, 
				int*, int*, int*, int *iret, size_t, size_t );

/* GG library */
void gg_asrd ( char *filtyp, char *filnam, char *stime, char *etime,
     char *dtime, float fwninc[], int icolrs[], int icolrs2[],
     int *numclr, int *iskip, int *interv, int *itmclr, int iflags[],
     int *ityp, int *iret );
void gg_cbar ( char*, int*, float*, int*, int *iret, size_t );
void gg_csig ( char*, int*, int*, int*, int *iret, size_t );
void gg_cwcp ( int*,  int*, int*, float*, float*, int* iret);
void gg_dltn ( int *, int *, int *, int *, float *, float *, int *, int * );
void gg_dvgf ( char *vgfile, int *icol, int *iret );
void gg_init ( int *mode, int *iret );
void gg_isig ( char*, int*, float*, int*, int*, int*, int *iret, size_t );
void gg_ltln ( char *latlon, int *iret, size_t );
void gg_ltng ( char*, int*, int*, int*, float*, float*, int*, int *iret, size_t );
void gg_map  ( char *map, int *iret, size_t );
void gg_maps ( char *proj, char *garea, char *imgfil, int *idrpfl, int *iret, 
		size_t, size_t, size_t );
void gg_motf ( char *window, int *iret, size_t );
void gg_panl ( char *panel, int *iret, size_t );
void gg_proj ( const char*, char*, float*, float*, int*, int*, size_t, size_t );
void gg_qsrd ( char *filtyp, char *filnam, char *cstime, char *etime,
     char *dtime, float fwninc[], int icolrs[], int icolrs2[], int *numclr,
     int *iskip, int *interv, int *itmclr, int iflags[], int *ityp,
     int *iret );
void gg_sdev ( char*, int *iret, size_t );
void gg_splt ( char *stnplt, int *iret, size_t );
void gg_sttl ( char *ttlstr, int *iret, size_t );
void gg_warn ( char *filtyp, char*, int*, int*, int*, float*, int*, int*, 
					int *iret, size_t, size_t );
void gg_wcck ( int*, char*, char*, int*, int*, int*, char*, char*, int*, size_t,
               size_t, size_t, size_t );
void gg_wcpb ( int*, char*, char*, char*, char*, char*, char*, int*);
void gg_wcur ( char *date, int*, int*, int*, size_t );
void gg_wcvf ( int*, int *iret );
void gg_wfps ( int*, char*, char*, int*, int wcnty[], int*,
               size_t, size_t );
void gg_wlbl ( int*, float*, float*, float*, float*, int *iret);
void gg_wlso ( int *npts, int nfips[], int *mxp, int *nunion, int *nv,
               float xp[], float yp[], float *xc, float* yc, float* area,
               int *iret );
void gg_wsrd ( char *filtyp, char *filnam, char *stime, char *etime, 
     char *dtime, float fwninc[], int icolrs[], int icolrs2[],
     int *numclr, int *iskip, int *interv, int *itmclr, int iflags[],
     int *ityp, int *iret );
void gg_wstr ( char *string, int *line, int *iret, size_t );
void gg_wtch ( char*, int*, float*, int*, int*, int *iret, size_t );
void gg_wusc ( char *ugcin, char *stin, char *namin, int *numb,
               char *cday, char *chour, int *vtecln, char *prdcod,
               char *actn, char *offid, char *phen, char *sigcd,
               char *etn, int vtime[], int etime[], char *cntystr,
               int *ilenout, int *iret);
void gg_wwfo ( char*, char*, int *iret, size_t, size_t );
void gg_wwtp ( int*, int*, char*, char*, char*, char*, char*, char*, int*, char*,
	       char*, char*, int*, char*, char*, char*, char*, int*, int*, int*,
	       int*, size_t, size_t, size_t, size_t, size_t, size_t, size_t, 
	       size_t, size_t, size_t, size_t, size_t, size_t            );

/* GH library */
void gh_advn ( char *, int *, int *, int * );
void gh_bkcal ( int*, int* );
void gh_bkloc ( char*, char*, char*, int*, size_t, size_t, size_t );
void gh_bkme ( int*, int* );
void gh_bkpl ( int(*) [ 4 ], int*, char*, int(*) [ 4 ], int(*) [ 4 ], int*, size_t );
void gh_bksq ( char*, int*, int*, int* );
void gh_bktb ( int* );
void gh_fstn ( char*, char*, char*, int*, int*, int*, char*, int* );
void gh_ftbl ( int* );
void gh_rtbl ( int* );
void gh_tctx ( char*, char*, char*, char*, char* );
void gh_wwbk ( int(*) [ 4 ], int[], int(*) [ 4 ], int[], int*, char (*) [ 1000 ],
	       char(*) [ 1000 ], int(*) [ 3 ], char(*) [ 605 ], int*, char*, int*,
	       size_t, size_t, size_t, size_t );
void gh_wwbl ( int(*) [ 4 ], int[], int(*) [ 4 ], int[], int*, char (*) [ 1000 ],
	       char(*) [ 1000 ], int(*) [ 3 ], char(*) [ 605 ], int*, char*, int*, 
	       size_t, size_t, size_t, size_t );
void gh_wwtx ( char*, char*, char*, char*, char*, int*, char*, int*, char*, char*,
	       char(*) [ 1000 ], char(*) [ 1000 ], int(*) [ 3 ], char(*) [ 605 ],
	       int*, size_t, size_t, size_t, size_t, size_t, size_t, size_t,
	       size_t, size_t, size_t, size_t );
void gh_0bkp ( char*, char*, int*, float*, float*, int*, float*, int*,
               int*, size_t, size_t );

/* GP library */
void gp_azdr ( float *azim, float *xm, float *ym, float *dir, int *iret );
void gp_draz ( float *dir, float *xm, float *ym, float *azim, int *iret );

/* GU library */
void gcboxx  ( const int *, const int *, const float *,
               const int *, const int *,
               const int *, const int *, const float *,
               const int *, const int *,
               const int *, const int *, int *);
void gcfill ( int*, int*, float*, int*, int*, int*, int*, float*, int*, int*, 
		int*, int*);
void gclgrn ( int*, int*, float*, int*, int*, int*, int*, float*, char*, 
              int*, int*, int*, int*, int*, int*, int );
void gstrml (const int *, const int *, float *, float *, const int *,
               const int *, const int *, const int *,
               int *, float *, float *, float *,
               float *, float *, int *);

/* IM library */
void im_cbar ( char *clrbar, int *iret, size_t );
void im_btot ( int *nvals, int *brit, float *tmpk, int *iret ) ;
void im_chtb ( char *chanl, int *itype, int *iret, size_t );
void im_drop ( int *iret );
void im_dump ( int *iret );
void im_gpix ( char *imgfil, char *garea, char *dattim, float *rlat, 
		float *rlon, int *maxdst, int *tblflg, int *mstflg, 
		int *iarrea, int *mode, int *ipix, float *tmpk, 
		char *stid, float *dist, float *pres, float *hght, 
		int *npt, int *iret, ... ); 
void im_gtmp ( char *imagefile, char *garea, char *sys, float *cenlat, 
		float *cenlon, int *irad, int *numx, int *numy, float *temps, 
		float *flats, float *flons,
		int *ier, size_t, size_t, size_t );
void im_gvtota ( int *nvals, unsigned int *gv, float *ta, int *iret );
void im_icmn ( int *hv, int *iret );
void im_lutf ( char*, int *iret, size_t );
void im_nexbz ( char filnam[], int *itype, int *ioff, int *iret );
void im_nexz ( char filnam[], int *nline, int *itype, int *ioff, int *iret );
void im_nzhd ( char*, unsigned char *barr, int *ipdoff, int *iret, size_t );
void im_qchn ( char *chntyp, int *iret, size_t ); 
void im_qimg ( int *, int *, int *, int *, int *iret );
void im_qlut ( char*, int *iret, size_t );
void im_qsiz ( int *ncol, int *nrow, int *iret );
void im_qtim ( int *idate, int *itime, int *iret );
void im_rcdf ( char imgfil[], int *kx, int *ky, int *isorc, int *itype,
                int *idate, int *itime, int *iproj, float *clat,
                float *clon, float *xmin, float *xmax, float *ymin,
                float *ymax, int *iret );
void im_rfax ( char *filnam, int *lenfil, int *isize, int *ixlen,
                                                  int *iylen, int *iret );
void im_rgin ( char filnam[], int *lenf, int *offset, int *lendat, int *iret );
void im_rtbl ( int *iret );
void im_sbgn ( int *kx, int *ky, int *ixlef, int *iytop, int *ixrit,
                                        int *iybot, int *ipix, int *iret );
void im_simg ( char *cproj, char *imgfil, int *iret, size_t, size_t );
void im_smgd ( char*, char*, int*, int*, char*, int *iret, size_t, size_t, size_t);
void im_ttob ( int *nvals, float *tmpk, float *brit, int *iret ) ;
void im_wgin ( char filnam[], int *lenf, int *offset, int *lendat, int *iret );
void lc_km_to_latlon( double*, double*, double*, double* );
void lc_latlon_to_km( double*, double*, double*, double* );
void prnlon  ( int *np, float *dlon, int *iret );
void psn_km_to_latlon_alaska( double*, double*, double*, double* );
void psn_km_to_latlon_hawaii( double*, double*, double*, double* );

void inc_pccftxt(char *tops, char *growth, char *prob, char *coverage,
                 char *instr, char *outstrfin, int *iret);
void inc_pgfatxt(char *status, char *hazard, char *tag, char *cycle, 
           char *fcsthr, char *dueto, char *top, char *base, char *fzltop,
           char *fzlbse, char *level, char *intensity, char *coverage,
	   char *category, char *frequency, char *instr, char *outstrfin,
           int *iret);

/* GR library */
void gr_acol ( int *, int *, float *, int *, int *, int *iret );
void gr_algn ( const float*, const float*, const float*, float*, int*, int*,
               int* );
void gr_cnav ( float *, float *, int *, int *, int *iret );
void gr_dorg ( int *, int *, int *, float *, int *iret );
void gr_gtim ( char *, char *, char *, char *, char *, int *,
				size_t, size_t, size_t, size_t, size_t );
void gr_levl ( char *, int *, int *, int *iret, size_t );
void gr_ltln ( int *, int *, float *, float *, int *iret );
void gr_mbn2 ( const float*, const int*, const float*, const float*,
               float*, int* );
void gr_mnav ( char*, int*, int*, float*, float*, float*, float*, float*,
			float*, float*, int*, float*, int *iret, size_t );
void gr_pack ( char *, int *, int *, int *iret, size_t );
void gr_plin ( char *, int *, int *, float *, float *, float *,
					float *, int *iret, size_t );
void gr_ploc ( char *, float *, float *, float *, float *, int *iret, size_t );
void gr_rban ( float *, float *, float *, float *, float *, float *,
						float *, int *, int *iret );
void gr_rnav ( float *, char *, int *, int *, int *, size_t );
void gr_setr ( int *, int *, int *, int *iret );
void gr_snav ( int *, float *, int *iret );
void gr_sscl ( int*, const int*, const int*, const int*, const int*,
               const int*, const int*, float*, float*, float*, int* );
void gr_stat ( float *, int *, int *, int *, int *, int *, int *, float*,
					float*, float*, float*, int *iret );
void gr_suba ( const char*, const int*, float*, float*, int*, int*, int*, int*,
               size_t );
void gr_vnav ( char*, int*, int*, float*, float*, float*, float*, float*,
                         float*, float*, int*, int*, int *iret, size_t );
void gr_wgb2 ( int*, int*, int*, int*, char*, int*, int*, char*, int*, 
			int*, int*, int*, int*, int*, int*, size_t, size_t );

/* GRC library */
void grc_geni ( const char *gdfile, FILE **fps, const int *nfps, int *iret );
void grc_wnmc ( FILE **fps, const int *nfps, const int *title,
                const int *num, const char *gdattm1, const char *gdattm2,
                const int *level1, const int *level2, const int *ivcord,
                const char *parm, const int *nmcprm, const int *nmcvco,
                const int *nmcgdn, int *iret );

/* IN library */
void in_bdta ( int *iret );
void in_cint ( char*, float*, int *npts, float *gmin, float *gmax, float*, 
		int*, float *frint, int *iret, size_t );
void in_line ( char*, float*, int *nexp, int*, int*, int*, int*, float *smooth, 
		float *filter, int *iret, size_t );
void in_outt ( char *output, char *name, int *lun, int *nlun, char *devs,
		int *iret, size_t, size_t, size_t );
void in_text ( char *test, int *iret, size_t );
void in_txtn ( char *text, int *ifont, int *ihwsw, float *siztxt, 
		int *itxwid, int *ibrdr, int *irrotn, int *ijust, int *iret,
		size_t );
void in_scal ( const char*, int*, int*, int*, size_t );
void in_catminp ( char *catmap, int *iret );
void in_catmmap ( char *string, float *value, int *iret );
void in_discrete( char *discrete, int *iret );
void in_discmap ( float *v_line1, float *v_line2, float *v_out, int *iret );
void in_discq   ( int *state, int *iret );
void in_discqleft ( float *value, float *newval, int *iret );
void in_dlines  ( char *dlines, int *iret );
void in_dlinq   ( int *stateR, int *stateL, float *epsilon, int *iret );
void inc_outt ( const char *output, const char *def, int *termflg,
                int *fileflg, char *filnam, int *iret );

/* IP library */
void ip_dynm ( int *done, int *iret );
void ip_exit ( int *iret );
void ip_help ( char *pname, int *pagflg, int *iret, size_t );
void ip_idnt ( char *program, int *iret, size_t );
void ip_init ( int *respnd, int *iret );
void ip_putv ( char*, char*, int *iret, size_t, size_t );
void ip_str ( char *pname, char *parm, int *iret, size_t, size_t );
void ip_strp ( char *progrm, int *iret, size_t );
void ip_svar ( char*, int *iret, size_t);
void ip_log  ( char *pname, int *logprm, int *iret, size_t);
void ip_resp ( int*, int* );
 

/* LC library */
void lc_gare ( char *garea, float *grltln, char *cdproj, float *centrd, int *iret, 
					size_t, size_t );

/* LV library */
void lv_ccrd ( int *ivcord, char *vcoord, int *iret, size_t );
void lv_cord ( char *vcoord, char *vparm, int *ivert, int *iret, size_t, size_t );
void lv_sort ( int *, int *, float *, int *iret );

/* MV library */
void mv_btoi ( unsigned char *barray, int *istart, int *nbytes, int *negflg, 
		int *ivalue, int *iret );
int  mv_ev32 ( int *n, float *valin, float *valout );
void mv_itob ( int *ivalue, int *istart, int *nbytes,
		unsigned char *barray, int *iret );
int  mv_swp2 ( int *n, void *input, void *output );
int  mv_swp4 ( int *n, void *input, void *output );
int  mv_ve32 ( int *n, float *valin, float *valout );

/* PR library */
#ifdef G_64BIT
   double pr_hgmf ( float *hght ); 
   double pr_mskn ( float *sped ); 
   double pr_tmkc ( float *tmpk );
   double pr_hgfm ( float* );
   double pr_knms ( float* );
   double pr_knmh ( float* );
#else
float pr_hgmf ( float *hght ); 
float pr_mskn ( float *sped ); 
float pr_tmkc ( float *tmpk );
   float  pr_hgfm ( float* );
   float  pr_knms ( float* );
   float  pr_knmh ( float* );
#endif

/* SF library */
void sf_clos ( int *isffln, int *iret );
void sf_rspc ( int *isffln, char *string, int *nchar, int *ihhmm, int *nrep, 
		int *iret, size_t );
void sf_rstr ( int *isffln, char *string, int *ihhmm, int *nchar, int *iret, size_t );
void sf_sstn ( int *isffln, char *stn, char *stid, int *istnm, float *slat, 
		float *slon, float *selv, int *ispri, int *iret, size_t, size_t );
void sf_stim ( int *isffln, char *dattim, int *iret, size_t );

/* SN library */
void sn_clos ( int *isnfln, int *iret );
void sn_rstr ( int *isnfln, char *part, char *string, int *ihhmm, int *nchar, 
						int *iret, size_t, size_t );
void sn_sstn ( int *isnfln, char *stn, char *stid, int *istnm, float *slat,
			float *slon, float *selev, int *iret, size_t, size_t );
void sn_stim ( int *isnfln, char *dattim, int *iret, size_t );

/* SS library */
void csleep  ( float *nsec, int *iret );
void ss_vers ( char*, int*, size_t );

/* ST library */
void st_atoe ( char *instr, int *nchar, unsigned char *barry, int *iret, size_t );
void st_gtst ( char*, char*, char*, char*, char*, int*, int *iret, ... );
void st_null ( char *string, char *outstr, int *lens, int *iret, size_t, size_t );
void st_numb ( char *string, int *ival, int *iret, size_t );
void st_opcl ( char *, int *, char *, int *, int *iret, size_t, size_t );

/* TB library */
void tb_idst ( char *stid, char *statnam, int *iret, size_t, size_t );
void tb_fgeo ( char *geog, float*, float*, float*, float*, char *cdproj, float*, 
					float*, int *iret, size_t, size_t );
void tb_nids ( const int *, const int *, char *, int *, char *,
					float *, char *, int *iret );
void tb_nidsdb ( char *prdnam, char *res, int *iprod, int *iret );

/* TG library */
void tg_ctoi ( char *, int *, int *, size_t );
void tg_diff ( char *, char *, int *, int *iret, size_t, size_t );
void tg_dual ( char *, char *, int *iret, size_t, size_t);
void tg_itoc ( int *intdtf, char *gdattm, int *iret, size_t );
void tg_vald ( char *gdattm, char *vdattm, int *iret, size_t, size_t );

/* TI library */
void ti_addd ( int*, int*, int *iret );
void ti_addm ( int*, int*, int*, int *iret );
void ti_c2i  ( char*, int*, int*, int*, int *iret, size_t );
void ti_ccnt ( char*, char*, int *iret, size_t, size_t );
void ti_cdtm ( int*, int*, char *dattim, int *iret, size_t );
void ti_ctoi ( char*, int*, int *iret, size_t );
void ti_daym ( int*, int*, int*, int *iret );
void ti_dayw ( int*, int*, int *iret );
void ti_difd ( char*, char*, float*, int *iret, size_t, size_t );
void ti_diff ( char*, char*, int*, int *iret, size_t, size_t );
void ti_dspl ( int*, char*, char*, int *iret, size_t, size_t );
void ti_dst  ( int*, int*, int *iret );
void ti_dtm4 ( char*, char*, int *iret, size_t, size_t );
void ti_elcl ( char*, char*, char*, int *iret, size_t, size_t, size_t);
void ti_find ( char*, int*, char*, char*, int*, char*, int *iret, 
			size_t, size_t, size_t, size_t );
void ti_form ( char*, char*, int *iret, size_t, size_t );
void ti_i2c  ( int*, int*, int*, char*, int *iret, size_t );
void ti_idtm ( char *dattim, int*, int*, int *iret, size_t );
void ti_itoc ( int*, char*, int *iret, size_t );
void ti_itoj ( int*, int*, int*, int *iret );
void ti_jtoi ( int*, int*, int*, int *iret );
void ti_mdif ( int*, int*, int*, int *iret );
void cti_mtch ( int match, const char *dattim, const char **times,
                        int ntime, int minute, int *ipos, int *iret );
void ti_mtch ( int*, char *dattim, char*, int*, int*, int*, int *iret, 
		size_t, size_t );
void ti_mtchw ( int*, char*, char*, int*, int*, int*, int *iret, size_t, size_t ); 
void cti_rseq ( int, char**, char** );
void ti_rseq ( int*, char*, char*, int *iret, size_t, size_t );
void ti_sort ( int*, char*, char*, int *iret, size_t, size_t );
void ti_stan ( char*, char*, char*, int *iret, size_t, size_t, size_t );
void ti_stnt ( char *dattim, int*, char*, int *iret, size_t, size_t );
void ti_subd ( int*, int*, int *iret );
void ti_subm ( int*, int*, int*, int *iret );
void cti_tmln ( char **timin, int nin, int mrange, int intrvl,
                int idir, int iflag, int iauto, char *basetm,
                char *endtim, int *idelrt, char **timout,
                int *ntime, int *iret );
void ti_tzdf ( int*, char*, char*, int*, float*, int *iret, size_t, size_t );
void ti_yy24 ( int*, int*, int *iret );
void ti_yymd ( int*, int*, int *iret );
void cti_yyyy ( int ntime, char **timin, char **outime, int *iret );
void ti_yyyy ( int*, char*, char*, int *iret, size_t, size_t );

/* TM library */
void tm_accp ( int* );
void tm_str  ( char*, int*, int*, char*, int*, size_t, size_t );

/*=====================================================================*/
/* GPLT functions */

void gcbar ( char *clrbar, int *nflvl, float *flvl, int *ifcolr, int *iret, size_t );
void gclear ( int *iret );
void gclpnl ( float*, float*, float*, float*, int *iret );
void gdpltb ( int *iframe, char *prfxtt, int *iret, size_t );
void gdpstp ( char *pname, char *s, int *iret, size_t, size_t );
void gdpstt ( char *lparm, int *value, int *iret, size_t );
void gdptmc ( char *gdatim, char *gdfile, char *cycle, char *sep, int *maxlen, 
		int *ntimes, char *timstr, int *length, int *iret, size_t, size_t, 
		size_t, size_t, size_t );
void genanm ( int *iret );
void gendp ( int *ieop, int *iret );
void geplot ( int* );
void getmap ( int *maptyp, int *mxelts, int *mxpts, int *nelts, int *npts, 
                 int *ielts, float *xlats, float *ylons, int *iret );

void ggapsm ( float*, float*, int*, int*, int*, int*, int*, int*, float*, float*, int*, 
					int *iret );
void ggdriv ( float *grid, float *grid1, int *kx, int *ky, float *hist, float *work1, float *work2, float *work3, float *buffer, int *rspflg, int *iret );
void ggsave ( char*, int *iframe, int *nframe, int *iret, G_Cardinal );
void ggtpnt ( char *sys, int *ityp, float *x, float *y, int *iret, size_t );
void ginitp ( int *mode, int *istat, int *iret );

void gqbnd ( char *sys, float*, float*, float*, float*, int *iret, size_t );
void gqcvsc ( float *dvcvsc, int *iret );
void gqgprj ( char *proj, float*, float*, float*, int*, int*, 
		float*, float*, float*, float*, int *iret, size_t );
void gqmprj ( char *proj, float*, float*, float*, float*, float*, float*, 
				float*, int *iret, size_t );

void groam ( int *ityp, char *sys, float *x, float *y, int *iret, size_t );

void gsdeva ( char*, int*, char*, int*, float*, float*, int *iret, ... );
void gsatim ( char*, int*, size_t );
void gsgprj ( char *, float *, float *, float *, int *, int *, float *,
				float*, float*, float*, int *iret, size_t );
void gsgraf ( int*, int*, float*, float*, float*, float*, float*, int *iret );
void gsgtgn ( int*, int*, int* );
void gsicmn ( int *iret );
void gslwin ( char *wname, int *iret, size_t );
void gsmfil ( char *mapfil, int *iret, size_t );
void gsmprj ( char *proj, float*, float*, float*, float*, float*, float*, float*, int *iret, size_t );
void gsplot ( int *iret );
void gstanm ( int *iret );
void gsview ( float*, float*, float*, float*, int *iret );

void gqarrw ( float*, float*, int*, int*, int* );
void gsarrw ( float*, float*, int*, int*, int* );
void gqbarb ( float*, int*, int*, int* );
void gsbarb ( float*, int*, int*, int* );
void gsbrgb ( int *icbank, int *ncolr, int *icolrs, int *reds, int *igrns, int *iblus,
		int *iret );
void gqclr2 ( int*, int*, int* );
void gsclr2 ( int*, int*, int* );
void gqcmbo ( float*, int*, int* );
void gscmbo ( float*, int*, int* );
void gqcolr ( int*, int* );
void gscolr ( int*, int* );

void gscrgb ( int*, int*, int*, int*, int* );

void gqctyp ( float*, int*, int* );
void gsctyp ( float*, int*, int* );
void gqdarr ( float*, float*, int*, int*, int* );
void gsdarr ( float*, float*, int*, int*, int* );
void gqdatt ( int*, char*, int*, float*, float*, int*, int *iret, size_t );
void gqfill ( float*, int*, int* );
void gsfill ( float*, int*, int* );
void gqfrnt ( int*, float*, int*, int*, int* );
void gsfrnt ( int*, float*, int*, int*, int* );
void gqhash ( float*, int*, int*, int* );
void gshash ( float*, int*, int*, int* );
void gqicng ( float*, int*, int* );
void gsicng ( float*, int*, int* );
void gqline ( int*, int*, int*, int*, int* );
void gsline ( int*, int*, int*, int*, int* );
void gqmrkr ( int*, int*, float*, int*, int* );
void gsmrkr ( int*, int*, float*, int*, int* );
void gqptnd ( float*, int*, int* );
void gsptnd ( float*, int*, int* );
void gqpwth ( float*, int*, int* );
void gspwth ( float*, int*, int* );
void gqsizd ( float*, float*, float*, float*, float*, float*, int *iret );
void gqsky  ( float*, int*, int*, int* );
void gssky  ( float*, int*, int*, int* );
void gqsmth ( int*, float*, int* );
void gssmth ( int*, float*, int* );
void gqspcl ( float*, int*, int* );
void gsspcl ( float*, int*, int* );
void gqspln ( int*, int*, int*, float*, int*, int* );
void gsspln ( int*, int*, int*, float*, int*, int* );
void gqtext ( int*, int*, float*, int*, int*, int*, int*, int* );
void gstext ( int*, int*, float*, int*, int*, int*, int*, int* );
void gqturb ( float*, int*, int* );
void gsturb ( float*, int*, int* );
void gqwthr ( float*, int*, int* );
void gswthr ( float*, int*, int* );

void garrw  ( char*, int*, float*, float*, float*, float*, int*, size_t );
void gbarb  ( char*, int*, float*, float*, float*, float*, int*, size_t );
void gcircl ( char*, float*, float*, float*, float*, int*, int*, size_t );
void gcmbo  ( char*, int*, float*, float*, float*, int*, int*, int*, size_t );
void gctyp  ( char*, int*, float*, float*, float*, int*, int*, int*, size_t );
void gdarr  ( char*, int*, float*, float*, float*, int*, size_t );
void gfill  ( char*, int*, float*, float*, int*, size_t );
void gfrnt  ( char*, int*, float*, float*, int*, size_t );
void ghash  ( char*, int*, float*, float*, float*, int*, size_t );
void gicng  ( char*, int*, float*, float*, float*, int*, int*, int*, size_t );
void gline  ( char*, int*, float*, float*, int*, size_t );
void glogo  ( char *sys, float *x, float *y, float *size, int *iclmod, int *ilogo, 
						int *iret, size_t );
void gmark  ( char*, int*, float*, float*, int*, size_t );
void gmesg  ( char*, int*, size_t );
void gptnd  ( char*, int*, float*, float*, float*, int*, int*, int*, size_t );
void gpwth  ( char*, int*, float*, float*, float*, int*, int*, int*, size_t );
void gplbnd ( char*, int*, float*, int*, float*, int*, int*, int*, int*,
                     char*, int*, int*, float*, int*, int*, size_t, size_t );
void gsky   ( char*, int*, float*, float*, float*, int*, int*, int*, size_t );
void gspcl  ( char*, int*, float*, float*, float*, int*, int*, int*, size_t );
void gspln  ( char*, int*, float*, float*, int*, size_t );
void gsroam ( int *irmflg , int *ipwdth , int *iphght , int *iret );
void gtext  ( char*, float*, float*, char*, float*, int*, int*, int*, size_t, size_t );
void gtextc ( char *sys, float *x, float *y, char *cchar, float *rotat, 
				int *ixoff, int *iyoff, int *iret, size_t, size_t );
void gtrans ( char*, char*, int*, float*, float*, float*, float*, int*, 
						size_t, size_t );
void gturb  ( char*, int*, float*, float*, float*, int*, int*, int*, size_t );
void gtxsy  ( char*, int*, int*, int*, int*, int*, float*, float*, float*, char*, 
						int*, size_t, size_t );
void gwthr  ( char*, int*, float*, float*, float*, int*, int*, int*, size_t );

void oabsdr ( float *gelat, float *gelon, float *coslat, float *data, int *infoflg, int *iret );

/* G2G library */
void gg_update ( char *vname, char *iname, int *cur_layer, char *catmap, int *plt_ext, int *iret );
void gg_rdvgf  ( char *vname, char *iname, char *catmap, int *iret );
