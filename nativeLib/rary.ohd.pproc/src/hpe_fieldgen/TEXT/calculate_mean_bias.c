/***********************************************************************
* Filename: calculate_mean_bias.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: March 2005
*
* Development Group: OHD
*
* Description:
* Contains routine for performing mean field bias estimation via exponential
* smoothing.
* 
* Modules:
* calculateMeanBias
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: calculateMeanBias
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: March 2005
*
* Description:
* This function is converted from FORTRAN code: mfb_subr.f.
* it performs mean field bias estimation via exponential 
* smoothing (Schweppe 1973)
* 
* Calling Arguments:
* Name            Input/Output Type        Description
* radarID         Input        const char* radar id
* datetime        Input        const char* date and time of current run
* pGageArray      Input        const gage_table_struct*
*                                          info from gage table
* pMPEParams      Input        const mpe_params_struct*
*                                          static parameters
* pGageRadarPairTable
*                 Input        gage_radar_pair_table_struct *
* dualpol_data_avail Input     int         0/1 represents if dualpol data available
*                                          array of positive gage/radar pair
* meanBias        Output       double*     the mean field bias value
* memSpanBias     Output       double*     the mean field bias value
*                                          in mem_span array.
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* readRWBiasDyn, gageRadarPairsQC,
* updateStateVariable, write_rwbiasdyn
*
* Return Value:
* Type          Description
* void
*
* Error Codes/Exceptions:
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date        Developer         Action
* 10/22/1998  D-J Seo           first version
* 03/24/1999  Jay Breidenbach   modification
* 03/04/2005  Guoxian Zhou      finish conversion to C Language 
* 10/05/2005  Guoxian Zhou      modification for empe version 
*
***********************************************************************/

void calculateMeanBias(const char * radarID,
                       const char * datetime ,
                       const empe_params_struct * pMPEParams ,
                       const gage_table_struct * pGageArray,
                       gage_radar_pair_table_struct * pGageRadarPairTable ,
		               int   dualpol_data_avail ,
                       double * meanBias,
                       double * memSpanBias )
{
    double  num_pairs[NUM_MEMORY_SPANS], sumGage[NUM_MEMORY_SPANS], 
            sumRadar[NUM_MEMORY_SPANS], bias[NUM_MEMORY_SPANS] ;
    char    datetime1[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    char    datehour[ANSI_YEARSEC_TIME_LEN + 1]  = {'\0'} ;
    int     lag, i, flag ;
    float * mem_span ;
    double  bias_long ;
    time_t  timet;

    long int irc ;
    int blnComputeBias;

    const int num_span  = pMPEParams->ptrRWBiasStat->num_span ;
    const int lag_cut   = pMPEParams->ptrRWBiasStat->lag_cut ;
    const int init_span = pMPEParams->ptrRWBiasStat->init_span ;

    blnComputeBias = 0;

    /*
     * check that number of memory spans
     * does not exceed NUM_MEMORY_SPANS.
     */

    if(num_span > NUM_MEMORY_SPANS)
    {
        sprintf ( message , "ERROR: number of memory spans %d great than %d"
                            "\n\tProgram exit.", 
                            num_span, NUM_MEMORY_SPANS) ;
        shutdown( message );
    }

    /*      
     * read state variables for current hour from RWBiasDyn table
     * if record not found for current hour,
     * then look back lag_cut hours for a record
     * if record not found within lag_cut hours,
     * then reinitialize state variables to 0.0.
     */

    /* allocate memory for the mem_span data */

    mem_span = (float *)malloc(NUM_MEMORY_SPANS * sizeof( float )); 
    if(mem_span == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in calculateMeanBias function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }

	struct tm * pDateHour = NULL ;

	strcpy(datetime1, datetime);
	yearsec_ansi_to_timet(datetime1, &timet);
	pDateHour = gmtime(&timet) ;

	strftime ( datehour, ANSI_YEARSEC_TIME_LEN + 1,
			"%Y-%m-%d %H:00:00", pDateHour ) ;

    /*
     * compute the mean field bias
     * only when the datetime is at the top hour
     * and the dsp_duration value is 60 minutes.   
     * 
     * comment on temporarily for emp
     * -- gzhou 10/05/2006
     */

/*
    if( (strcmp(datetime, datehour) == 0) &&
        (pMPEParams->dsp_duration == 60) )
    {
    	blnComputeBias = 1;
    }
*/

    mem_span[0] = (float)pMPEParams->ptrRWBiasStat->mem_span1 ;
    mem_span[1] = (float)pMPEParams->ptrRWBiasStat->mem_span2 ;
    mem_span[2] = (float)pMPEParams->ptrRWBiasStat->mem_span3 ;
    mem_span[3] = (float)pMPEParams->ptrRWBiasStat->mem_span4 ;
    mem_span[4] = (float)pMPEParams->ptrRWBiasStat->mem_span5 ;
    mem_span[5] = (float)pMPEParams->ptrRWBiasStat->mem_span6 ;
    mem_span[6] = (float)pMPEParams->ptrRWBiasStat->mem_span7 ;
    mem_span[7] = (float)pMPEParams->ptrRWBiasStat->mem_span8 ;
    mem_span[8] = (float)pMPEParams->ptrRWBiasStat->mem_span9 ;
    mem_span[9] = (float)pMPEParams->ptrRWBiasStat->mem_span10;

    readRWBiasDyn(radarID, pMPEParams->fxa_local_site, datehour, 
             lag_cut, num_pairs, sumGage, 
             sumRadar, bias, &lag, datetime1, dualpol_data_avail, &irc) ;

    if((lag > 1) && (lag < lag_cut))
    {
        sprintf ( message, "state variables used are from %d hours ago", lag);
        hpe_fieldgen_printMessage( message );
    }

    if(irc == 0)
    {
        sprintf ( message , "*** state variables read from database ***\n"
                 "index      memspan  num_pairs  sumgage  sumradar    bias");
        hpe_fieldgen_printMessage( message );

        bias_long = bias[num_span - 1] ;

        for(i = 0; i < num_span; i++)
        {
            sprintf ( message , "%5d %12.3f    %7.1f  %7.2f   %7.2f  %6.2f",
                    i, mem_span[i], num_pairs[i], 
                    sumGage[i], sumRadar[i], bias[i]);
            hpe_fieldgen_printMessage( message );
        }
    }
    else
    {
        if(irc == 100)
        {
            sprintf ( message , "Records not found in RWBiasDyn table "
                        "between current date/time and date/time = %s"
                        " -- state variables reinitialized", datetime1);
            hpe_fieldgen_printMessage( message );
        }
        else
        {
            sprintf ( message , "Database error %ld attempting select in "
                        "the RWBiasDyn table for date/time = %s"
                        " -- state variables reinitialized", irc, datetime1);
            hpe_fieldgen_printMessage( message );
        }

        *meanBias = 1.0 ;
        bias_long = 1.0 ;

        sprintf ( message , "index      memspan  num_pairs"
                            "  sumgage  sumradar    bias");
        hpe_fieldgen_printMessage( message );

        for(i = 0; i < num_span; i++)
        {
            sumGage[i]   = 0.0 ;
            sumRadar[i]  = 0.0 ;
            num_pairs[i] = 0.0 ;
            bias[i]      = 1.0 ;

            sprintf ( message , "%5d %12.3f     %7.1f  %7.2f   %7.2f  %6.2f" ,
                        i, mem_span[i], num_pairs[i], 
                        sumGage[i], sumRadar[i], bias[i]);
            hpe_fieldgen_printMessage( message );        
        }
    }

    /*      
     * if lag > lag_cut, then perform storm-by-storm reinitialization of
     * spatial averages of positive gage and radar rainfall
     */

    if(lag >= lag_cut)
    {
        for(i = 0; i < init_span; i++)
        {
            sumGage[i]  = sumGage[init_span - 1] ;
            sumRadar[i] = sumRadar[init_span - 1] ;
        }
    }
    
    /*      
     * check gage/radar pairs for quality control
     * bias_long is the bias value with longest memory span
     */

	gageRadarPairsQC(pMPEParams, bias_long, pGageRadarPairTable, &flag) ;

    if(flag != 0)
    {
        sprintf ( message , "gage/radar pairs qc not done.");
        hpe_fieldgen_printMessage( message );        
    }

    /*
     * update state variables
     * 
     * comment on temporarily for emp
     * -- gzhou 10/05/2006
     */
/*
    updateStateVariable(pMPEParams, mem_span, lag, 
            pGageRadarPairTable, sumGage, sumRadar, num_pairs) ;
*/

    /*
     * compute the bias for each mem_span value
     * when blnComputeBias == 1
     * 
     */

    if(blnComputeBias == 1)
    {
	    for( i = 0; i < num_span; i++)
	    {
	        if((sumGage[i] == 0.0) || (sumRadar[i] == 0.0))
	        {
	            bias[i] = 1.0 ;
	        }
	        else
	        {
	            bias[i] = sumGage[i] / sumRadar[i] ;
	        }
	    }
    }

    /*      
     * compute the best bias
     */

    *meanBias = 1.0 ;
    for( i = 0; i < num_span; i++)
    {
        if(num_pairs[i] > pMPEParams->ptrRWBiasStat->npair_bias_select)
        {
            *meanBias = bias[i] ;
            *memSpanBias = (double)mem_span[i] ;
            break ;
        }
    }

/*
    sprintf ( message , "computed bias = %5.2f\n"
            "writing state variables to database.\n"
            "index      memspan  num_pairs  sumgage"
            "  sumradar    bias", *meanBias );
    printMessage( message );

    for(i = 0; i < num_span; i++)
    {
        sprintf ( message , "%5d %12.3f    %7.1f  %7.2f   %7.2f  %6.2f" ,
                i, mem_span[i], num_pairs[i], 
                sumGage[i], sumRadar[i], bias[i]);
        printMessage( message );        
    }
*/

    /*
     * write updated state variables back to RWBiasDyn table
     * when blnComputeBias == 1
     * 
     */

/*
    irc = 0;
    if(blnComputeBias == 1)
    {
	    write_rwbiasdyn(radarID, datehour, num_span, num_pairs,
	                    sumGage, sumRadar, bias, &irc) ;
    }

    if(irc != 0)
    {
        sprintf ( message , "Database error %ld "
                    "attempting write to the RWBiasDyn table"
                    " for date = %s", irc, datetime);
        printMessage( message );        
    }
*/

    if(mem_span != NULL)
    {
        free(mem_span) ;
        mem_span = NULL ;
    }    

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc/src/hpe_fieldgen/RCS/calculate_mean_bias.c,v $";
 static char rcs_id2[] = "$Id: calculate_mean_bias.c,v 1.2 2007/10/30 13:39:21 gzhou Exp $";}
/*  ===================================================  */

}
