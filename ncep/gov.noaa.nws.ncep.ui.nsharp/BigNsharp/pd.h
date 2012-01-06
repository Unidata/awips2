/************************************************************************
 * pd.h                                                             	*
 * This include file Contains prototypes for the pd library.		*
 *                                                                      *
 **                                                                     *
 * R. Tian/SAIC		 9/05						*
 ***********************************************************************/
#ifndef PD_H_
#define PD_H_

#include "geminc.h"
#include "gemprm.h"

/*
 * Prototypes.
 */
void pd_ctot ( const float *t500, const float *td850, const int *np,
               float *ctot, int *iret );
void pd_dden ( const float *pres, const float *tmpc, const int *np,
               float *dden, int *iret );
void pd_drct ( const float *uwnd, const float *vwnd, const int *npt,
               float *drct, int *iret );
void pd_dwpt ( const float *rmix, const float *pres, const int *np,
               float *dwpc, int *iret );
void pd_fosb ( const float *tmpc, const float *relh, const float *sped,
               const int *np, float *fosb, int *iret );
void pd_hans ( const float *tc1, const float *tc2, const float *dwpc,
               const int *np, const int *type, float *haines, int *iret );
void pd_heat ( const float *tmpf, const float *relh, const int *np,
                float *heat, int *iret );
void pd_inmm ( const float *xinch, const int *np, float *xmm, int *iret );
void pd_mmin ( const float *xmm, const int *np, float *xinch, int *iret );
void pd_kinx ( const float *t850, const float *t700, const float *t500,
               const float *td850, const float *td700, const int *np,
               float *rkinx, int *iret );
void pd_knms ( const float *sknt, const int *np, float *sped, int *iret );
void pd_mskn ( const float *sped, const int *np, float *sknt, int *iret );
void pd_mixr ( const float *dwpc, const float *pres, const int *np,
               float *rmix, int *iret );
void pd_plcl ( const float *pres, const float *tmpc, const float *dwpc,
               const int *np, float *plcl, int *iret );
void pd_tlcl ( const float *tmpc, const float *dwpc, const int *npt,
               float *tlcl, int *iret );
void pd_prcp ( const float *prc1, const float *prc2, const float *rmult,
               const int *np, float *total, int *iret );
void pd_prcr ( const float *prate, const int *nhr, const int *np,
               float *prmm, int *iret );
void pd_relh ( const float *tmpc, const float *dwpc, const int *np,
               float *relh, int *iret );
void pd_rhdp ( const float *tmpc, const float *relh, const int *np,
               float *dwpc, int *iret );
void pd_sduv ( const float *sped, const float *drct, const int *np,
               float *uwnd, float *vwnd, int *iret );
void pd_shmr ( const float *spfh, const int *np, float *mixr, int *iret );
void pd_slvp ( const float *frc, const float *ges, const int *itypbc,
               const float *rmx, const float *rmy, const int *kxx,
               const int *kyy, const float *hddx, const float *hddy,
               float *aa, float *bb, float *fld, int *iret );
void pd_sped ( const float *uwnd, const float *vwnd, const int *np,
               float *sped, int *iret );
void pd_swet ( const float *t850, const float *td850, const float *t500,
               const float *spd850, const float *spd500,
               const float *dir850, const float *dir500, const int *np,
               float *swet, int *iret );
void pd_thta ( const float *tmpc, const float *pres, const int *np,
               float *thta, int *iret );
void pd_thte ( const float *pres, const float *tmpc, const float *dwpc,
               const int *np, float *thte, int *iret );
void pd_thwc ( const float *pres, const float *tmpc, const float *dwpc,
               const int *np, float *thwc, int *iret );
void pd_tmcf ( const float *tmpc, const int *np, float *tmpf, int *iret );
void pd_tmck ( const float *tmpc, const int *np, float *tmpk, int *iret );
void pd_tmfc ( const float *tmpf, const int *np, float *tmpc, int *iret );
void pd_tmfk ( const float *tmpf, const int *np, float *tmpk, int *iret );
void pd_tmkc ( const float *tmpk, const int *np, float *tmpc, int *iret );
void pd_tmkf ( const float *tmpk, const int *np, float *tmpf, int *iret );
void pd_tmpk ( const float *pres, const float *thta, const int *np,
               float *tmpk, int *iret );
void pd_tmst ( const float *thte, const float *pres, const int *np,
               float *tmst, int *iret );
void pd_tmwb ( const float *pres, const float *tmpk, const float *rmix,
               const int *np, float *wetbt, int *iret );
void pd_totl ( const float *t850, const float *td850, const float *t500,
               const int *np, float *totl, int *iret );
void pd_tvrk ( const float *tmpk, const float *rmix, const int *np,
               float *tvrk, int *iret );
void pd_uvsd ( const float *uwnd, const float *vwnd, const int *np,
               float *sped, float *drct, int *iret );
void pd_vpmr ( const float *vapr, const float *pres, const int *np,
               float *mixr, int *iret );
void pd_vtot ( const float *t500, const float *t850, const int *np,
               float *vtot, int *iret );

#endif
