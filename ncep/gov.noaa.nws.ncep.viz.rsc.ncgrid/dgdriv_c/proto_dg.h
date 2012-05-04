/************************************************************************
 * proto_dg.h                                                          	*
 *                                                                      *
 * This include file contains the public APIs of the DG library.	*
 *                                                                      *
 * Log:                                                                 *
 **                                                                     *
 * R. Tian/SAIC		 2/06	Created					*
 * R. Tian/SAIC		 9.06	Added dg_kxky				*
 ***********************************************************************/

#ifndef PROTO_DG_H_
#define PROTO_DG_H_

/*
 * Interface to applications.
 */
void dgc_cxgp ( const char *uipnts, const char *ijskip, const int *npmx,
                int *np, float *rgx, float *rgy, float *rlat, float *rlon,
	        int *iret );
void dgc_fixa ( const char *area, const char *proj, char *areout,
                char *prjout, int *iret );
void dgc_flno ( const char *gfunc, int *igdfln, int *iret );
void dgc_glev ( const int *intry, const char *gdattm1, const char *gdattm2,
                const int *ivcord, const int *maxlev, int *levarr1,
	        int *levarr2, int *nlev, int *iret );
void dgc_inxt ( const int *chngnv, const int *coladd, const char *time1,
                const char *time2, int *iret );
void dgc_nrdt ( const int *ifpn, const char *time1, const char *time2,
                const int *level1, const int *level2, const int *ivcord,
                const char *parm, float *grid, int *igx, int *igy, int *ighd,
                int *iret );
void dgc_nwdt ( const float *grid, const char *time1, const char *time2,
                const int *level1, const int *level2, const int *ivcord,
	        const char *parm, const int *igdhdr, const char *gpack,
	        const int *rplc, int *iret );
void dgc_qdtm ( const int *intry, char *fstgt, char *lstgt, int *iret );
void dgc_qgrd ( const char *time1, const char *time2, const int *level1,
                const int *level2, const int *ivcord, const char *parm,
	        int *exist, int *iret );
void dgc_qtms ( const int *mxtms, const int *both, char *tmlst, int *ntms,
                char *trange, int *iret );
void dgc_subg ( const char *ijskip, int *maxgrid, int *imll, int *jmll, int *imur,
                int *jmur, int *iret );
void dgc_vecr ( const char *gdattm, const char *glevel, const char *gvcord,
                const char *gvect, char *pfunc, float *ugrid, float *vgrid,
	        int *igx, int *igy, char *time1, char *time2, int *level1,
	        int *level2, int *ivcord, char *parmu, char *parmv,
	        int *iret );
void dgc_vect ( const char *gdattm, const char *glevel, const char *gvcord,
                const char *gvect, char *pfunc, float *ugrid, float *vgrid,
	        int *igx, int *igy, char *time1, char *time2, int *level1,
	        int *level2, int *ivcord, char *parmu, char *parmv,
	        int *iret );
void dg_clos ( const int *idgfln, int *iret );
void dg_fall ( int *iret );
void dg_grel ( const float *unor, const float *vnor, float *urel,
               float *vrel, int *iret );
void dg_hilo ( const float *grid, const int *kx, const int *ky,
               const int *ksrad, const int *intflg, const int *nmax,
               const int *nmin, const float *rlomax, const float *rhimax,
               const float *rlomin, const float *rhimin, int * nummax,
               float *xmax, float *ymax, float *vmax, int *nummin,
               float *xmin, float *ymin, float *vmin, int *iret );
void dg_igrg ( const int *np, const float *ri, const float *rj,
               float *rir, float *rjr, int *iret);
void dg_kxky ( int *kx, int *ky, int *iret );
void dg_nrel ( const float *urel, const float *vrel, float *unor,
               float *vnor, int *iret );
void dg_oang ( const float *orient, int *iret );
void dg_orgn ( const float *olat, const float *olon, int *iret );
void dg_qkxy ( int *kx, int *ky, int *iret );
void dg_qref ( const int *mxanl, const int *mxnav, float *bkanl,
	       float *bknav, int *mxgrd, int *iret );
void dg_rstm ( int *iret );

/*
 * Interface to libraries.
 */
void dg_azst ( const int *ilat, const int *ilon, int *iazst, int *iret );
void dg_cget ( const char *ckey, char *cval, int *iret );
void dg_cset ( const char *ckey, const char *cval, int *iret );
void dg_dmsf ( int *iret );
void dg_driv ( const int *itype, int *iret );
void dg_esub ( const int *n1, const int *n2, const int *n3, const int *n4,
	       int *iret );
void dg_fget ( const char *ckey, const int *nval, float *fval, int *iret );
void dg_frig ( const int *num, int *iret );
void dg_getg ( const int *num, float **grid, int *kx, int *ky,
               int *ksub1, int *ksub2, int *iret );
void dg_getl ( int *ignum1, int *ignum2, int *iret );
void dg_gets ( int *ignum, int *iret );
void dg_gett ( int *ignum1, int *ignum2, int *iret );
void dg_getv ( int *ignumu, int *ignumv, int *iret );
void dg_gnfl ( const int *entry, char *ntmplt, char *gflpth,
               char *crtfnm, char *crtgdt1, char *crtgdt2,
	       int *mbrnum, int *iret );
void dg_gobs ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, int *igu, int *igv,
	       int *iret );
void dg_gtvl ( int *igu1, int *igv1, int *igu2, int *igv2, int *iret );
void dg_iget ( const char *ckey, const int *nval, int *ival, int *iret );
void dg_iset ( const char *ckey, const int *nval, const int *ival, int *iret );
void dg_ltln ( int *iret );
void dg_merr ( const char *name, const char *time1, const char *time2,
               const int *level1, const int *level2, const int *ivcord,
               char *errst, int *iret );
void dg_mnam ( const char *func, const char *parm1, const char *parm2,
               char *dname, int *iret );
void dg_mscl ( int *iret );
void dg_nxts ( int *num, int *iret );
void dg_nxtv ( int *numu, int *numv, int *iret );
void dg_pfun ( const char *func, int *iret );
void dg_puts ( const int *ignum, int *iret );
void dg_putv ( const int *ignumu, const int *ignumv, int *iret );
void dg_qbnd ( int *jgxmin, int *jgxmax, int *jgymin, int *jgymax, int *iret );
void dg_qlyr ( const char *gfunc, int *lflag, int *iret );
void dg_qmsl ( int *ixmscl, int *iymscl, float *gddx, float *gddy, int *iret );
void dg_real ( const float *value, const int *num, int *iret );
void dg_rpls ( const char *gfunc, const int *ignum, int *iret );
void dg_rplv ( const char *gvect, const int *ignumu, const int *ignumv,
               int *iret );
void dg_snfl ( const int *entry, const char *ntmplt, const char *gflpth,
               const char *crtfnm, const char *crtgdt1, const char *crtgdt2,
	       const int *mbrnum, int *iret );
void dg_ssub ( int *iret );
void dg_temp ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret );
void dg_tops ( char *gfunc, int *ignum, char *time1, char *time2,
               int *level1, int *level2, int *ivcord, char *parm, int *iret );
void dg_topv ( char *gvect, int *ignumu, int *ignumv, char *time1,
               char *time2, int *level1, int *level2, int * ivcord,
	       char *parm, int *iret );
void dg_udig ( const char *ftype, const int *ignu, const int *ignv,
               int *idnum, char *stprm, int *iret );
void dg_updh ( const char *func, const int *num, const int *num1,
               const int *num2, int *iret );
void dg_updv ( const char *func, const int *numu, const int *numv,
               const int *num1, const int *num2, int *iret );
void dg_upsg ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const int *ifilen,
               const char *parm, const int * num, int *iret );
void dg_upvg ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const int *ifilen,
	       const char *parm, const int *numu, const int *numv, int *iret );

#endif
