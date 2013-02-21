/************************************************************************
 * proto_na.h                                                           *
 *                                                                      *
 * This header file contains the public APIs of the NA library.        	*
 *                                                                      *
 * Log:                                                                 *
 **                                                                     *
 * R. Tian/SAIC          7/06   Created                                 *
 ***********************************************************************/

#ifndef PROTO_NA_H_
#define PROTO_NA_H_

/*
 * Interface to applications.
 */
void na_ganl ( const char *anlyss, const float *rnvblk, float *anlblk,
               int *iret );
void na_gcog ( const char *gdoutf, const char *proj, const char *grdarea,
               const char *kxky, const char *cpyfil, const int *fill,
               const char *garea, const float *gdsarr, int *maxg,
               int *igdfln, float *rnvful, int *igrdnm, char *cprj,
               int *ikx, int *iky, int *jx, int *jy, int *ksubx,
               int *ksuby, int *subset, int *iret );
void na_ggds ( const float *gdsarr, char *cprj, int *kx, int *ky,
               float *grdout, float *rnvblk, int *iret );
void na_gnav ( const char *proj, const char *kxky, const char *gdarea,
               char *cprj, int *kx, int *ky,  float *grdout,
               float *rnvblk, int *iret );
void na_gssg ( const int *igxold, const int *ksbx, const int *ksby,
               float *grid, int *igx, int *igy, int *iret );
void na_gtbl ( const char *cpyfil, char *name, char *proj, int *nxgd,
               int *nygd, float *garea, float *rnvblk, float *anlblk,
               int *iret );
void na_init ( int *iret );
void na_rhdr ( const int *jtime, const int *jaccm, const int *jlevel1,
               const int *jlevel2, const int *jvcord, const int *jparm,
               const int *icodtbl, const int *pdsext, char *gdattm1,
               char *gdattm2, int *level1, int *level2, int *ivcord,
               char *parm, int *iscal, float *rmsval, int *ispds,
               char *cpds, int *ihzrmp, int *idrct, int *iret );
void na_rtbl ( const int *ieditn, const int *icenter, const int *icodtbl,
               const char *wtbl, const char *ntbl, const char *vtbl,
	       const char *ctbl, int *iret );
void na_smry ( FILE **fps, const int *nfps, const int *nread,
               const int *nwrit, const char *infil, const char *outfil,
               int *iret );
#endif
