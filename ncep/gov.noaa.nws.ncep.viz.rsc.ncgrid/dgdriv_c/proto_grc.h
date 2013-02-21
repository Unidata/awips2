/************************************************************************
 * proto_grc.h                                                          *
 *                                                                      *
 * This include file contains the public APIs of the GRC library.	*
 *                                                                      *
 * Log:                                                                 *
 **                                                                     *
 * R. Tian/SAIC          9/06   Created                                 *
 * R. Tian/SAIC         10/06   Added grc_wgb2                          *
 ***********************************************************************/

#ifndef PROTO_GRC_H_
#define PROTO_GRC_H_

void grc_acol ( int *kxin, int *kyin, float *grid, int *kxout,
                int *kyout, int *iret );
void grc_algn ( const float *grdin, const float *deltax,
                const float *deltay, float *grdout, int *kx, int *ky,
                int *iret );
void grc_cnav ( const float *rnvblk, const float *gsnvbk,
                const int *navsz, int *gsflag, int *iret );
void grc_dorg ( int *kxin, int *kyin, int *ishft,
                float *grid, int *iret );
void grc_fixa ( const int *igdfln, const char *area, const char *proj,
                char *areout, char *prjout, int *iret );
void grc_gtim ( char *gdattm, char *firstm, char *lasttm, char *gdtim1,
	        char *gdtim2, int *iret );
void grc_intp ( const int*inttyp, const float *gx, const float *gy,
                const int *npts, const int *kx, const int *ky,
                const float *grid, float *sdint,  int *iret );
void grc_levl ( const char *glevel, int *level1, int *level2, int *iret );
void grc_ltln ( int *kx, int *ky, float *rlat, float *rlon, int *iret );
void grc_mbn2 ( const float *deltan, const int *iebnds, const float *dbnds,
                const float *rnvblk, float *anlblk, int *iret );
void grc_mnav ( const char *proj, const int *kx, const int *ky,
                const float *rlat1, const float *rlon1,
                const float *rlat2, const float *rlon2,
                const float *angl1, const float *angl2,
                const float *angl3, const int *angflg, float *rnvblk,
                int *iret );
void grc_pack ( const char *gpack, int *ipktyp, int *nbits, int *iret );
void grc_plin ( char *endpts, const int *npmx, int *npts, float *rgx,
	        float *rgy, float *rlat, float *rlon, int *iret );
void grc_ploc ( char *gpoint, float *rgx, float *rgy, float *rlat,
                float *rlon, int *iret );
void grc_rban ( float *anlblk, float *deltan, float *deltax,
                float *deltay, float *gbnds, float *ebnds, float *dbnds,
                int *iextnd, int *iret );
void grc_rnav ( const float *rnvblk, char *proj, int *kx, int *ky,
                int *iret );
void grc_setr ( int *kxin, int *kyin, int *ishft, int *iret );
void grc_snav ( int *navsz, float *rnvblk, int *iret );
void grc_sscl ( int *iscale, const int *kx, const int *ky, const int *imin,
                const int *jmin, const int *imax, const int *jmax,
                float *grid, float *rmin, float *rmax, int *iret );
void grc_stat ( float *z, int *kx, int *ky, int *imin, int *jmin, int *imax,
                int *jmax, float *rmin, float *rmax, float *ravg,
                float *rdev, int *iret );
void grc_sub2 ( const float *rnvblk, float *rgx2, int *igxold, int *iret );
void grc_suba ( const char *garea, const int *fill, float *rnvblk,
                float *altln, int *ksubx, int *ksuby, int *subset,
                int *iret );
void grc_wgb2 ( FILE **fps, const int *nfps, const int *title,
                const int *num, const char *gdattm1, const char *gdattm2,
                const int *level1, const int *level2, const int *ivcord,
                const char *parm, const int *idis, const int *icat,
                const int *iidn, const int *ipdn, const int *ivco,
                const int *igdn, int *iret );
void grc_wtrm ( FILE *fp, const int *title, const int *ignum,
                const char *gdattm1, const char *gdattm2,
                const int *level1, const int *level2, const int *ivcord,
                const char *parm, int *iret );

#endif
