/*******************************************************************************
* FILENAME:            read_rwparams.c
*
* Purpose:
* This function reads a record into RWParams struct variable
* from the RWParams table.
*
* calling function: readParams
* functions called: NULL
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         March, 2005
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   March 2005   Guoxian Zhou      Finish first version 
*
********************************************************************************
*/

#include "RWParams.h"
#include "sqlca.h"

void MPEFieldGen_readRWParams ( RWParams * pRWParams, long int * ircpar )
{
   RWParams * pRWParamsHead = NULL;

   /*--------------------------------------*/
   /*   read from RWParams table           */
   /*--------------------------------------*/
   pRWParamsHead = GetRWParams ( "" );

   *ircpar = SQLCODE;

   if ( pRWParamsHead != NULL )
   {
      pRWParams->rw_min_rain		= pRWParamsHead->rw_min_rain;
      pRWParams->rw_sep_dist		= pRWParamsHead->rw_sep_dist;
      pRWParams->rw_lag0_ind_corr	= pRWParamsHead->rw_lag0_ind_corr;
      pRWParams->rw_lag0_cond_corr	= pRWParamsHead->rw_lag0_cond_corr;
      pRWParams->num_near_gages		= pRWParamsHead->num_near_gages;
      pRWParams->num_near_rad_bins	= pRWParamsHead->num_near_rad_bins;
      pRWParams->def_cond_var_rad	= pRWParamsHead->def_cond_var_rad;
      pRWParams->def_ind_corr_scl	= pRWParamsHead->def_ind_corr_scl;
      pRWParams->def_cond_corr_scl	= pRWParamsHead->def_cond_corr_scl;
      pRWParams->min_ind_corr_scl	= pRWParamsHead->min_ind_corr_scl;
      pRWParams->min_cond_corr_scl	= pRWParamsHead->min_cond_corr_scl;
      pRWParams->max_ind_corr_scl	= pRWParamsHead->max_ind_corr_scl;
      pRWParams->max_cond_corr_scl	= pRWParamsHead->max_cond_corr_scl;
      pRWParams->nn_srch_method		= pRWParamsHead->nn_srch_method;

      FreeRWParams ( pRWParamsHead );
      pRWParamsHead = NULL;
   }
} /* end MPEFieldGen_readRWParams */
