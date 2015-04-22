
/************************************************************************
 * dg.h                                                           	*
 *                                                                      *
 * This include file contains the APIs of the DG library.       	*
 *                                                                      *
 * Log:                                                                 *
 **                                                                     *
 * R. Tian/SAIC          2/06   Created                                 *
 * R. Tian/SAIC		 9/06	Removed grc.h, proto_cmm.h, 		*
 *                              fortran_wrappers.h			*
 ***********************************************************************/

#ifndef _DG_H
#define _DG_H

#include "geminc.h"
#include "gemprm.h"

#include "dggrid.h"
#include "dginpt.h"
#include "dgtabl.h"
#include "dgstck.h"
#include "dgfile.h"
#include "mapscl.h"
#include "dgstrm.h"
#include "dgerr.h"
#include "dgovec.h"
#include "dgorig.h"
#include "dgarea.h"
#include "dgsubg.h"
#include "dgrtwd.h"
#include "dglndc.h"
#include "nfile.h"
#include "hintrp.h"

#include "de.h"
#include "dl.h"
#include "dv.h"
#include "df.h"

/*
 * DG public APIs.
 */
#include "proto_dg.h"

/*
 * DG private APIs.
 */
void dg_adcl ( int *iret );
void dg_cdeg ( const char *prmin, const char *prmout, const int *num,
               int *iret );
void dg_chck ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret );
void dg_clcn ( const int *nclust, const int *idofcl, const float *x,
               const float *y, int *idextr, int *iret );
void dg_clmp ( const int *krad, int *mcnt, float *x, float *y, float *v,
               int *idofcl, int *keep, int *iret );
void dg_cndg ( const char *cname, int *igrd, int *cmptd, int *iret );
void dg_cola ( int *mcnt, float *x, float *y, float *v, int *keep, int *iret );
void dg_cone ( const char *gprj, const float *angr1, const float *angr3,
	       float *ccone, int *iret );
void dg_cwbt ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret );
void dg_dwpt ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret );
void dg_dump ( const char *func, int *iret );
void dg_fclp ( const int *krad, const int *icntpt, const int *mcnt,
               const float *x, const float *y, int *keep, int *idofcl,
	       int *nclust, int *iret );
void dg_fnds ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, char *gfunc,
	       int *ignum, int *iret );
void dg_fndv ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, char *gvect,
	       int *ignumu, int *ignumv, int *iret );
void dg_g2gc ( const int *inttyp, const int *glbwi, const int *kxi,
               const int *kyi, const float *grdi, const int *kxo,
	       const int *kyo, float *gixo, float *giyo, float *grdo,
	       int *iret );
void dg_g2gi ( const int *inttyp, const int *glbwi, const int *kxi,
               const int *kyi, const float *grdi, const int *kxo,
	       const int *kyo, float *gixo, float *giyo, float *grdo,
	       int *iret );
void dg_g2gs ( const int *inttyp, const int *glbwi, const int *kxi,
               const int *kyi, const float *grdi, const int *kxo,
	       const int *kyo, float *gixo, float *giyo, float *grdo,
	       int *iret );
void dg_grdr ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret );
void dg_grot ( int *iret );
void dg_gtmp ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret );
void dg_lncr ( int *iret );
void dg_lncx ( int *iret );
void dg_mhdr ( const char *func, const int *num1, const int *num2,
               char *time1, char *time2, int *level1, int *level2,
	       int *jvcord, char *parm, int *iret );
void dg_mxnt ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret );
void dg_newg ( const int *grdsiz, float **grdptr, int *iret );
void dg_prcp ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret );
void dg_prdr ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret );
void dg_prft ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret );
void dg_pwts ( const int *kx, const int *ky, int *polen, int *poles,
               int *wrapa, int *wrapc, int *iret );
void dg_rgrd ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
               int *num, int *iret );
void dg_robs ( const int *iflno, const char *gdtime1, const char *gdtime2,
               const int *level1, const int *level2, const int *ivcord,
	       float *grid1, float *grid2, int *igx, int *igy, int *iret );
void dg_sare ( int *iret );
void dg_scal ( const char *parm, const int *num, int *iret );
void dg_snav ( const float *rnav, int *iret );
void dg_stlv ( const char *gdattm, const char *glevel, const char *gvcord,
               const char *caller, char *gfunc, int *iret );
void dg_t2ig ( const float *tfnav, const int *ighd, float *tgrid,
               float *grid, int *igx, int *igy, int *iret );
void dg_t2nr ( const float *urel, const float *vrel, float *unor,
               float * vnor, int *iret );
void dg_tadc ( int *iret );
void dg_trig ( float *ugrd, float *vgrd, int *iret );
void dg_trot ( int *iret );
void dg_tscl ( int *ixtms, int *iytms, float *tgdx, float *tgdy,
               float *cone, int *iret );
void dg_vcrd ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *np, int *iret );
void dg_wobs ( const int *iflno, const char *gdtime1, const char *gdtime2,
               const int *level1, const int *level2, const int *ivcord,
	       float *grid1, float *grid2, int *wcmp, int *wmks, char *wparm,
	       int *igx, int *igy, int *iret );

/*
 * DG globals.
 */
#ifdef DG_GLOBAL
    struct dggrid   _dggrid;
    struct dginpt   _dginpt;
    struct dgtabl   _dgtabl;
    struct dgstck   _dgstck;
    struct dgfile   _dgfile;
    struct mapscl   _mapscl;
    struct dgstrm   _dgstrm;
    struct dgerr    _dgerr;
    struct dgovec   _dgovec;
    struct dgorig   _dgorig;
    struct dgarea   _dgarea;
    struct dgsubg   _dgsubg;
    struct dgrtwd   _dgrtwd;
    struct dglndc   _dglndc;
    struct nfile    _nfile;
    struct hintrp   _hintrp;
#else
    extern struct dggrid   _dggrid;
    extern struct dginpt   _dginpt;
    extern struct dgtabl   _dgtabl;
    extern struct dgstck   _dgstck;
    extern struct dgfile   _dgfile;
    extern struct mapscl   _mapscl;
    extern struct dgstrm   _dgstrm;
    extern struct dgerr    _dgerr;
    extern struct dgovec   _dgovec;
    extern struct dgorig   _dgorig;
    extern struct dgarea   _dgarea;
    extern struct dgsubg   _dgsubg;
    extern struct dgrtwd   _dgrtwd;
    extern struct dglndc   _dglndc;
    extern struct nfile    _nfile;
    extern struct hintrp   _hintrp;
#endif

#endif
