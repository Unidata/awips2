/*******************************************************************************
* FILENAME:  write_params.c
*
* Purpose:
* This function writes parameters from pMPEParams_struct into log file
*
* calling function: main_mpe_fieldgen
* functions called: NULL
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         March, 2005
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   March 2005   Guoxian Zhou      Finish first version 
*   April 2007   P Tilles          Removed print of version number
*                                  (moved to main program)
**
********************************************************************************
*/

#include "mpe_fieldgen.h"
#include <unistd.h>

void MPEFieldGen_writeParams(const mpe_params_struct * pMPEParams)
{
    const int hostLen = 128 ;
    char hostname[128] = {'\0'};
    int i ;


    gethostname(hostname, hostLen) ;

    fprintf(logFile, "\nSTATUS: load-in parameters...\n");

    fprintf(logFile, "\tSYSTEM PARAMETERS\n");
    fprintf(logFile, "\tdatabase name\t= %s\n",
        pMPEParams->db_name);
    fprintf(logFile, "\thost name\t= %s\n",
        hostname);
    fprintf(logFile, "\tdpa wind\t= %d (minutes)\n",
        pMPEParams->dpa_wind);
    fprintf(logFile, "\tdaa wind\t= %d (minutes)\n",
        pMPEParams->daa_wind);
    fprintf(logFile, "\txmrgdtform\t= %s\n",
        pMPEParams->xmrgdtform);
    fprintf(logFile, "\trfc name\t= %s\n\n",
        pMPEParams->rfc_name);

    if(pMPEParams->mpe_save_gif == 1)
    	fprintf(logFile, "\tcreate and Save GIF Image = ON\n");
    else
    	fprintf(logFile, "\tcreate and Save GIF Image = OFF\n");
    
    if(pMPEParams->mpe_save_jpeg == 1)
    	fprintf(logFile, "\tcreate and Save JPEG Image = ON\n");
    else
    	fprintf(logFile, "\tcreate and Save JPEG Image = OFF\n");

    if(pMPEParams->mpe_save_grib == 1)
    	fprintf(logFile, "\tcreate and Save GRIB Image = ON\n");
    else
    	fprintf(logFile, "\tcreate and Save GRIB Image = OFF\n");

    if(pMPEParams->mpe_save_netcdf == 1)
    	fprintf(logFile, "\tcreate and Save netCDF Image = ON\n");
    else
    	fprintf(logFile, "\tcreate and Save netCDF Image = OFF\n");

    fprintf(logFile, "\n\tMEAN FIELD BIAS CALCULATION PARAMETERS\n");
    fprintf(logFile, "\tmin_gr_value_bias\t= %5.2f\n",
        pMPEParams->ptrRWBiasStat->min_gr_value_bias);
    fprintf(logFile, "\tnpair_bias_select\t= %ld\n",
        pMPEParams->ptrRWBiasStat->npair_bias_select);
    fprintf(logFile, "\tnpair_svar_update\t= %ld\n",
        pMPEParams->ptrRWBiasStat->npair_svar_update);
    fprintf(logFile, "\tstd_cut\t= %ld\n",
        pMPEParams->ptrRWBiasStat->std_cut);
    fprintf(logFile, "\tlag_cut\t= %ld\n",
        pMPEParams->ptrRWBiasStat->lag_cut);
    fprintf(logFile, "\tinit_span\t= %ld\n",
        pMPEParams->ptrRWBiasStat->init_span);
    fprintf(logFile, "\tbias_qc_opt\t= %ld\n",
        pMPEParams->ptrRWBiasStat->bias_qc_opt);

    fprintf(logFile, "\tmem_span values: \n");
    for ( i = 0; i < pMPEParams->ptrRWBiasStat->num_span; ++i )
    {
        fprintf(logFile, "%14.3f", pMPEParams->memory_spans[i]);
        if((i+1)%5 == 0)
            fprintf(logFile, "\n");
    }
    if(pMPEParams->ptrRWBiasStat->num_span%5 != 0)
        fprintf(logFile, "\n");

    fprintf(logFile, "\n\tMULTISENSOR FIELD ANALYSIS PARAMETERS\n");
    fprintf(logFile, "\trw_min_rain\t= %7.2f\n",
        pMPEParams->ptrRWParams->rw_min_rain);
    fprintf(logFile, "\trw_sep_dist\t= %7.2f\n",
        pMPEParams->ptrRWParams->rw_sep_dist);
    fprintf(logFile, "\trw_lag0_ind_corr\t= %7.2f\n",
        pMPEParams->ptrRWParams->rw_lag0_ind_corr);
    fprintf(logFile, "\trw_lag0_cond_corr\t= %7.2f\n", 
        pMPEParams->ptrRWParams->rw_lag0_cond_corr);
    fprintf(logFile, "\tnum_near_gages\t= %7d\n",
        pMPEParams->ptrRWParams->num_near_gages);
    fprintf(logFile, "\tnum_near_rad_bins\t= %7d\n",
         pMPEParams->ptrRWParams->num_near_rad_bins);
    fprintf(logFile, "\tdef_cond_var_rad\t= %7.2f\n",
         pMPEParams->ptrRWParams->def_cond_var_rad);
    fprintf(logFile, "\tdef_ind_corr_scl\t= %7.2f\n",
         pMPEParams->ptrRWParams->def_ind_corr_scl);
    fprintf(logFile, "\tdef_cond_corr_scl\t= %7.2f\n",
         pMPEParams->ptrRWParams->def_cond_corr_scl);
    fprintf(logFile, "\tmin_ind_corr_scl\t= %7.2f\n",
         pMPEParams->ptrRWParams->min_ind_corr_scl);
    fprintf(logFile, "\tmin_cond_corr_scl\t= %7.2f\n",
         pMPEParams->ptrRWParams->min_cond_corr_scl);
    fprintf(logFile, "\tmax_ind_corr_scl\t= %7.2f\n",
         pMPEParams->ptrRWParams->max_ind_corr_scl);
    fprintf(logFile, "\tmax_cond_corr_scl\t= %7.2f\n",
         pMPEParams->ptrRWParams->max_cond_corr_scl);

    if(pMPEParams->ptrRWParams->nn_srch_method == 1)
        fprintf(logFile, "\tnearest-neighbor search method = "
            "double heap-sorting.\n");
    else if(pMPEParams->ptrRWParams->nn_srch_method == 2)
        fprintf(logFile, "\tnearest-neighbor search method = "
            "spiral search.\n");
    else
        fprintf(logFile, "\tnearest-neighbor search method not defined.\n");

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/write_params.c,v $";
 static char rcs_id2[] = "$Id: write_params.c,v 1.3 2012/07/05 18:40:37 pst Exp $";}
/*  ===================================================  */

}
