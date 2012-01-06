/************************************************************************
 * df.h                                                                 *
 *                                                                      *
 * This include file contains the APIs of the DF library.               *
 *                                                                      *
 * Log:                                                                 *
 **                                                                     *
 * R. Tian/SAIC          2/06   Created                                 *
 * R. Tian/SAIC		 9/06	Removed grc.h, fortran_wrappers.h	*
 * K. Brill/HPC          4/08   Added df_incd; df_bncdf; df_ibncd	*
 ***********************************************************************/

#ifndef DF_H_
#define DF_H_

#include "geminc.h"
#include "gemprm.h"
#include "pd.h"
#include "dv.h"

/*
 * prototypes.
 */
void df_abs ( int *iret );
void df_acos ( int *iret );
void df_add ( int *iret );
void df_and ( const int *nargs, int *iret );
void df_asin ( int *iret );
void df_atan ( int *iret );
void df_atn2 ( int *iret );
void df_avg ( int *iret );
void df_beta ( const int *num, int *iret );
void df_bool ( int *iret );
void df_bvsq ( int *iret );
void df_corl ( const int *num, int *iret );
void df_cos ( int *iret );
void df_dden ( int *iret );
void df_ddt ( int *iret );
void df_ddx ( int *iret );
void df_ddy ( int *iret );
void df_eor ( const int *nargs, int *iret );
void df_eq ( int *iret );
void df_exp ( int *iret );
void df_expi ( int *iret );
void df_fosb ( int *iret );
void df_ge ( int *iret );
void df_gele ( int *iret );
void df_gelt ( int *iret );
void df_gt ( int *iret );
void df_gtle ( int *iret );
void df_gtlt ( int *iret );
void df_gwfs ( int *iret );
void df_high ( int *iret );
void df_hilo  ( const int *hi, const int *lo, int *iret );
void df_igpt ( const int *num, int *iret );
void df_int ( int *iret );
void df_jcbn ( int *iret );
void df_jgpt ( const int *num, int *iret );
void df_knts ( int *iret );
void df_lap ( int *iret );
void df_lav ( int *iret );
void df_ldf ( int *iret );
void df_le ( int *iret );
void df_ln ( int *iret );
void df_log ( int *iret );
void df_lows ( int *iret );
void df_lt ( int *iret );
void df_mask ( int *iret );
void df_mass ( int *iret );
void df_miss ( int *iret );
void df_mixr ( int *iret );
void df_mul ( int *iret );
void df_ncdf ( int *iret );
void df_incd ( int *iret );
void df_bncdf ( int *iret );
void df_ibncd ( int *iret );
void df_ne ( int *iret );
void df_nint ( int *iret );
void df_not ( int *iret );
void df_or ( const int *nargs, int *iret );
void df_paub ( int *iret );
void df_plcl ( int *iret );
void df_pois ( int *iret );
void df_quo ( int *iret );
void df_relh ( int *iret );
void df_savs ( int *iret );
void df_sbtw ( int *iret );
void df_sge ( int *iret );
void df_sgmn ( int *iret );
void df_sgmx ( int *iret );
void df_sgt ( int *iret );
void df_sin ( int *iret );
void df_sle ( int *iret );
void df_slt ( int *iret );
void df_sm5s ( int *iret );
void df_sm9s ( int *iret );
void df_smax ( int *iret );
void df_smin ( int *iret );
void df_sqrt ( int *iret );
void df_stab ( int *iret );
void df_sub ( int *iret );
void df_tan ( int *iret );
void df_tav ( int *iret );
void df_tdf  ( int *iret );
void df_than ( int *iret );
void df_thes ( int *iret );
void df_thta ( int *iret );
void df_thte ( int *iret );
void df_thwc ( int *iret );
void df_tlcl ( int *iret );
void df_tmst ( int *iret );
void df_tmwk ( int *iret );
void df_wndx ( int *iret );
void df_xav ( int *iret );
void df_xsum ( int *iret );
void df_xval ( const int *num, int *iret );
void df_yav ( int *iret );
void df_ysum ( int *iret );
void df_yval ( const int *num, int *iret );

#endif
