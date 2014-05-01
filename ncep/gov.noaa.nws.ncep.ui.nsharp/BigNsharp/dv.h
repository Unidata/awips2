/************************************************************************
 * dv.h			                                                *
 * This header file Contains prototypes of the DV library           	*
 *                                                                      *
 **                                                                     *
 * S. Gilbert/NCEP        11/05   Created                               *
 ***********************************************************************/
#ifndef DV_H_
#define DV_H_

#include "geminc.h"
#include "gemprm.h"
#include "proto_dg.h"
#include "df.h"
#include "pd.h"

void dv_adv ( int *iret );
void dv_age  ( int *iret );
void dv_avor ( int *iret );
void dv_circ  ( int *iret );
void dv_cros ( int *iret );
void dv_def ( int *iret );
void dv_dirn  ( int *iret );
void dv_dirr  ( int *iret );
void dv_div ( int *iret );
void dv_divt  ( int *iret );
void dv_dot ( int *iret );
void dv_dsub  ( int *iret );
void dv_dvdx ( int *iret );
void dv_dvdy ( int *iret );
void dv_frnt ( int *iret );
void dv_geo  ( int *iret );
void dv_grad ( int *iret );
void dv_gwfv ( int *iret );
void dv_inad ( int *iret );
void dv_isal  ( int *iret );
void dv_kcrs ( int *iret );
void dv_kntv  ( int *iret );
void dv_ltrn ( int *iret );
void dv_mag  ( int *iret );
void dv_mdiv ( int *iret );
void dv_mrad ( int *iret );
void dv_msdv  ( int *iret );
void dv_msfc ( int *iret );
void dv_mtng ( int *iret );
void dv_norm ( int *iret );
void dv_nrmv ( int *iret );
void dv_obs  ( int *iret );
void dv_pvor ( int *iret );
void dv_qvcl ( int *iret );
void dv_qvec ( int *iret );
void dv_rad ( int *iret );
void dv_rich  ( int *iret );
void dv_ross ( int *iret );
void dv_rot  ( int *iret );
void dv_sdiv ( int *iret );
void dv_shr ( int *iret );
void dv_sm5v ( int *iret );
void dv_smul  ( int *iret );
void dv_squo  ( int *iret );
void dv_str ( int *iret );
void dv_tang ( int *iret );
void dv_thrm  ( int *iret );
void dv_tng ( int *iret );
void dv_tngv ( int *iret );
void dv_un  ( int *iret );
void dv_ur  ( int *iret );
void dv_vadd  ( int *iret );
void dv_vasv ( int *iret );
void dv_vavs  ( int *iret );
void dv_vbtw ( int *iret );
void dv_vecn  ( int *iret );
void dv_vecr  ( int *iret );
void dv_vge  ( int *iret );
void dv_vgt  ( int *iret );
void dv_vlav ( int *iret );
void dv_vldf ( int *iret );
void dv_vle  ( int *iret );
void dv_vlt  ( int *iret );
void dv_vmsk  ( int *iret );
void dv_vmul  ( int *iret );
void dv_vn  ( int *iret );
void dv_vor ( int *iret );
void dv_vquo  ( int *iret );
void dv_vr  ( int *iret );
void dv_vsub  ( int *iret );
void dv_wnan  ( int *iret );
void dv_wshr  ( int *iret );

#endif
