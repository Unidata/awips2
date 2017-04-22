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
**
********************************************************************************
*/

#include <unistd.h>

#include "empe_fieldgen.h"

void writeParams(const empe_params_struct * pEMPEParams)
{
    char hostname[20] = {'\0'};
    int i ;
    const int hostLen = 20 ;

    gethostname(hostname, hostLen) ;

    sprintf ( message , "\nSTATUS: load-in parameters...\n"
                        "\tSYSTEM PARAMETERS" ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tdatabase name\t= %s",
        pEMPEParams->db_name ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tversion number\t= OB8.3" ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\thost name\t= %s", hostname ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tdhr window\t= %d (minutes)",
        pEMPEParams->dhr_window ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tdsp window\t= %d (minutes)",
        pEMPEParams->dsp_window ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tdsp duration\t= %d (minutes)",
        pEMPEParams->dsp_duration ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\txmrgdtform\t= %s",
        pEMPEParams->xmrgdtform ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\trfc name\t= %s",
        pEMPEParams->rfc_name ) ;
    hpe_fieldgen_printMessage( message );

/*
    if(pEMPEParams->save_gif == 1)
    {
	    sprintf ( message , "\tcreate and Save GIF Image = ON") ;
	    printMessage( message );
    }
    else
    {
	    sprintf ( message , "\tcreate and Save GIF Image = OFF") ;
	    printMessage( message );
    }
    
    if(pEMPEParams->save_jpeg == 1)
    {
	    sprintf ( message , "\tcreate and Save JPEG Image = ON") ;
	    printMessage( message );
    }
    else
    {
	    sprintf ( message , "\tcreate and Save JPEG Image = OFF") ;
	    printMessage( message );
    }

    if(pEMPEParams->save_grib == 1)
    {
	    sprintf ( message , "\tcreate and Save GRIB Image = ON") ;
	    printMessage( message );
    }
    else
    {
	    sprintf ( message , "\tcreate and Save GRIB Image = OFF") ;
	    printMessage( message );
    }

    if(pEMPEParams->save_netcdf == 1)
    {
	    sprintf ( message , "\tcreate and Save netCDF Image = ON") ;
	    printMessage( message );
    }
    else
    {
	    sprintf ( message , "\tcreate and Save netCDF Image = OFF") ;
	    printMessage( message );
    }
*/

    sprintf ( message , "\n\tMEAN FIELD BIAS CALCULATION PARAMETERS" ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tmin_gr_value_bias\t= %5.2f",
        pEMPEParams->ptrRWBiasStat->min_gr_value_bias ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tnpair_bias_select\t= %ld",
        pEMPEParams->ptrRWBiasStat->npair_bias_select ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tnpair_svar_update\t= %ld",
        pEMPEParams->ptrRWBiasStat->npair_svar_update ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tstd_cut\t= %ld",
        pEMPEParams->ptrRWBiasStat->std_cut ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tlag_cut\t= %ld",
        pEMPEParams->ptrRWBiasStat->lag_cut ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tinit_span\t= %ld",
        pEMPEParams->ptrRWBiasStat->init_span ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tbias_qc_opt\t= %ld",
        pEMPEParams->ptrRWBiasStat->bias_qc_opt ) ;
    hpe_fieldgen_printMessage( message );


    sprintf ( message , "\tmem_span values: " ) ;
    hpe_fieldgen_printMessage( message );


    memset(message, '\0', MESSAGE_LEN);
  	char tmp[20] = {'\0'};

    for ( i = 0; i < pEMPEParams->ptrRWBiasStat->num_span; ++i )
    {
        sprintf(tmp, "%14.3f", pEMPEParams->memory_spans[i]);
    	strcat(message, tmp);
        if((i+1)%5 == 0)
        {
            sprintf(tmp, "\n");
	    	strcat(message, tmp);
        }
    }
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\n\tMULTISENSOR FIELD ANALYSIS PARAMETERS" ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\trw_min_rain\t= %7.2f",
        pEMPEParams->ptrRWParams->rw_min_rain ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\trw_sep_dist\t= %7.2f",
        pEMPEParams->ptrRWParams->rw_sep_dist ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\trw_lag0_ind_corr\t= %7.2f",
        pEMPEParams->ptrRWParams->rw_lag0_ind_corr ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\trw_lag0_cond_corr\t= %7.2f", 
        pEMPEParams->ptrRWParams->rw_lag0_cond_corr ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tnum_near_gages\t= %7d",
        pEMPEParams->ptrRWParams->num_near_gages ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tnum_near_rad_bins\t= %7d",
         pEMPEParams->ptrRWParams->num_near_rad_bins ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tdef_cond_var_rad\t= %7.2f",
         pEMPEParams->ptrRWParams->def_cond_var_rad ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tdef_ind_corr_scl\t= %7.2f",
         pEMPEParams->ptrRWParams->def_ind_corr_scl ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tdef_cond_corr_scl\t= %7.2f",
         pEMPEParams->ptrRWParams->def_cond_corr_scl ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tmin_ind_corr_scl\t= %7.2f",
         pEMPEParams->ptrRWParams->min_ind_corr_scl ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tmin_cond_corr_scl\t= %7.2f",
         pEMPEParams->ptrRWParams->min_cond_corr_scl ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tmax_ind_corr_scl\t= %7.2f",
         pEMPEParams->ptrRWParams->max_ind_corr_scl ) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "\tmax_cond_corr_scl\t= %7.2f",
         pEMPEParams->ptrRWParams->max_cond_corr_scl ) ;
    hpe_fieldgen_printMessage( message );

    if(pEMPEParams->ptrRWParams->nn_srch_method == 1)
    {
	    sprintf ( message , "\tnearest-neighbor search method = "
            "double heap-sorting." ) ;
	    hpe_fieldgen_printMessage( message );
    }
    else
    {
	    sprintf ( message , "\tnearest-neighbor search method = "
            "spiral search." ) ;
	    hpe_fieldgen_printMessage( message );
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc/src/hpe_fieldgen/RCS/write_params.c,v $";
 static char rcs_id2[] = "$Id: write_params.c,v 1.3 2007/10/25 12:52:15 gzhou Exp $";}
/*  ===================================================  */

}
