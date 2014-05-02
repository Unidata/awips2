/******************************************************************************
* FILENAME:            calculateMeanBiasDP.c
*
* Purpose:
* performs mean field bias estimation via exponential 
* smoothing (Schweppe 1973)
*
* calling function: getMeanBiasDP
* functions called: read_daabiasdyn, gageRadarPairsQC,
*                    updateDAAStateVariable, write_daabiasdyn
*
* input variables
*
* radarID - radar id
*
* datetime - YYYY-MM-DD HH:MM:SS formatted datetime
*
* pMPEParams - static parameters
*            - DAA mean-field bias will use same parameters as DPA mean-field bias
*              (for now)
*
* pGageArray - array of gage data
*
* pGageRadarPairTable - array of positive gage/radar pair data
*
*
* output variables
*
* meanBias - the mean field bias value
* memSpanBias - the mean field bias value in mem_span array.

*
*******************************************************************************
*/

#include "mpe_fieldgen.h"

void calculateMeanBiasDP(const char * radarID,
                    const char * datetime ,
                    const mpe_params_struct * pMPEParams ,
                    const gage_table_struct * pGageArray,
                    gage_radar_pair_table_struct * pGageRadarPairTable ,
                    double * meanBias,
                    double * memSpanBias )
{
    double    num_pairs[NUM_MEMORY_SPANS], sumGage[NUM_MEMORY_SPANS], 
            sumRadar[NUM_MEMORY_SPANS], bias[NUM_MEMORY_SPANS] ;
    char    datetime1[ANSI_YEARSEC_TIME_LEN + 1] ;
    int     lag, i, flag ;
    float * mem_span ;
    double  bias_long ;

    long int irc=0 ;

/*    char  site_id[RFC_LEN + 1] = "";     */
    
    const char *site_id = pMPEParams->fxa_local_site;
    
    const int num_span  = pMPEParams->ptrRWBiasStat->num_span ;
    const int lag_cut   = pMPEParams->ptrRWBiasStat->lag_cut ;
    const int init_span = pMPEParams->ptrRWBiasStat->init_span ;

    /**      
     * check that number of memory spans
     * does not exceed NUM_MEMORY_SPANS.
     **/
    if(num_span > NUM_MEMORY_SPANS)
    {
        sprintf ( message , "ERROR: number of memory spans %d great than %d"
                            "\n\tProgram exit.", 
                            num_span, NUM_MEMORY_SPANS) ;
        shutDownMPE( message, logFile );
    }
    
    /**      
     * read state variables for current hour from DAABiasDyn table
     * if record not found for current hour,
     * then look back lag_cut hours for a record
     * if record not found within lag_cut hours,
     * then reinitialize state variables to 0.0.
     **/

    /* allocate memory for the mem_span data */
    mem_span = (float *)malloc(NUM_MEMORY_SPANS * sizeof( float )); 
    if(mem_span == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in calculateMeanBias function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

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

    MPEFieldGen_read_daabiasdyn(radarID, pMPEParams->fxa_local_site, datetime,
             lag_cut, num_pairs, sumGage, 
             sumRadar, bias, &lag, datetime1, &irc) ;

    if((lag > 1) && (lag < lag_cut))
    {
        sprintf ( message , "daa state variables used are from %d hours ago", lag);
        printMessage(message, logFile);        
    }

    if(irc == 0)
    {
        sprintf ( message , "*** state variables read from database ***\n"
                 "index      memspan  num_pairs  sumgage  sumradar    bias");
        printMessage(message, logFile);        

        bias_long = bias[num_span - 1] ;

        for(i = 0; i < num_span; i++)
        {
            sprintf ( message , "%5d %12.3f    %7.1f  %7.2f   %7.2f  %6.2f" ,
                    i, mem_span[i], num_pairs[i], 
                    sumGage[i], sumRadar[i], bias[i]);
            printMessage(message, logFile);        
        }
    }
    else
    {
        if(irc == 100)
        {
            sprintf ( message , "Records not found in DAABiasDyn table "
                        "between current date/time and date/time = %s"
                        " -- state variables reinitialized", datetime1);
        }
        else
        {
            sprintf ( message , "Database error %ld attempting select in "
                        "the DAABiasDyn table for date/time = %s"
                        " -- daa state variables reinitialized", irc, datetime1);
        }
        printMessage(message, logFile);        

        *meanBias = 1.0 ;
        bias_long = 1.0 ;

        sprintf ( message , "index      memspan  num_pairs  sumgage  sumradar    bias");
        printMessage(message, logFile);        

        for(i = 0; i < num_span; i++)
        {
            sumGage[i]   = 0.0 ;
            sumRadar[i]  = 0.0 ;
            num_pairs[i] = 0.0 ;
            bias[i]      = 1.0 ;
            
            sprintf ( message , "%5d%12.3f     %7.1f  %7.2f   %7.2f  %6.2f" ,
                        i, mem_span[i], num_pairs[i], 
                        sumGage[i], sumRadar[i], bias[i]);
            printMessage(message, logFile);        
        }
    }

    /**      
     * if lag > lag_cut, then perform storm-by-storm reinitialization of
     * spatial averages of positive gage and radar rainfall
     **/
    if(lag >= lag_cut)
    {
        for(i = 0; i < init_span; i++)
        {
            sumGage[i] = sumGage[init_span - 1] ;
            sumRadar[i] = sumRadar[init_span - 1] ;
        }
    }
    
    /**      
     * check gage/radar pairs for quality control
     * bias_long is the bias value with longest memory span
     **/
    MPEFieldGen_gageRadarPairsQC(pMPEParams, bias_long, pGageRadarPairTable, &flag) ;

    if(flag != 0)
    {
        sprintf ( message , "gage/radar pairs qc not done.");
        printMessage(message, logFile);        
    }

    /**      
     * update state variables
     **/
    MPEFieldGen_updateStateVariable(pMPEParams, mem_span, lag,
            pGageRadarPairTable, sumGage, sumRadar, num_pairs) ;

    /**      
     * compute the bias for each mem_span value
     **/
    for( i = 0; i < num_span; i++)
    {
        if((sumGage[i] == 0.0) || (sumRadar[i] == 0.0))
            bias[i] = 1.0 ;
        else
            bias[i] = sumGage[i] / sumRadar[i] ;
    }

    /**      
     * compute the best bias
     **/
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

    /**      
     * write updated state variables back to DAABiasDyn table
     **/
    sprintf ( message , "computed bias = %5.2f\n"
            "writing state variables to database.\n"
            "index      memspan  num_pairs  sumgage"
            "  sumradar    bias", *meanBias );
    printMessage(message, logFile);        

    for(i = 0; i < num_span; i++)
    {
        sprintf ( message , "%5d %12.3f    %7.1f  %7.2f   %7.2f  %6.2f" ,
                i, mem_span[i], num_pairs[i], 
                sumGage[i], sumRadar[i], bias[i]);
        printMessage(message, logFile);        
    }

    write_daabiasdyn(radarID, pMPEParams->fxa_local_site, datetime,
                    num_span, num_pairs, sumGage, sumRadar, bias, &irc) ;

    if(irc != 0)
    {
        sprintf ( message , "Database error %ld "
                    "attempting write to the DAABiasDyn table"
                    " for date = %s", irc, datetime);
        printMessage(message, logFile);        
    }
    
    if(mem_span != NULL)
    {
        free(mem_span) ;
        mem_span = NULL ;
    }    

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/calculateMeanBiasDP.c,v $";
 static char rcs_id2[] = "$Id: calculateMeanBiasDP.c,v 1.1 2012/04/25 16:32:12 pst Exp $";}
/*  ===================================================  */

}
