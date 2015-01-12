/*******************************************************************************
* FILENAME:            gage_radar_pairs_qc.c
*
* Purpose:
* This function is converted from FORTRAN code: mfb_gr_pairs_qc.f.
* it performs quality control of mean field bias gage-radar pairs
*
* calling function: calculateMeanBias
* functions called: none
*
* input variables
*
* pMPEParams - static parameters
*
* bias_long - bias value at longest memory span
*
* pGageRadarPairTable - array of positive gage/radar pair data
*
*
* output variables
*
* pGageRadarPairTable - array of positive gage/radar pair data
*                       after quality control
*
* flag - error flag. 1 indicates that QC could not be performed,
*                    0 indicates that QC was performed
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   10/22/1998   D-J Seo           first version
*   02/17/2005   Guoxian Zhou      finish conversion to C Language 
*
********************************************************************************
*/

#include "mpe_fieldgen.h"
#include <math.h>

void MPEFieldGen_gageRadarPairsQC(const mpe_params_struct * pMPEParams ,
                const double bias_long ,
                gage_radar_pair_table_struct * pGageRadarPairTable , 
                int * flag)
{
    int i, count = 0 ;
    double slope, b ;
    double sume1 ;
    double sume2 ;
    double sum_gage ;
    double sum_radar ;
    double sum_gage_radar ;
    double sum_radar2 ;
    double error, ave, var, std ;
    
    *flag = 0 ;

    /* number of gage/radar pairs needed to do standard dev test */
    const int npair_stddev_test = 5 ;

    /**
      * std_cut - cutoff error standard deviation
      **/
    const int std_cut = pMPEParams->ptrRWBiasStat->std_cut ;

    /**
      * option -- 1 for one-parameter regression (i.e., slope-only),
      *           2 for two-parameter regression (i.e., slope and intercept)
      **/
    const int option =  pMPEParams->ptrRWBiasStat->bias_qc_opt ;

    /**
      * for case when number of gage/radar pairs < npair_stddev_test,
      * gage value difference from regression line (units = inches)
      **/
    const double gage_cut_value = 3.0 ;

    const int npair = pGageRadarPairTable->pairNum ;

    /**      
     * if there are >= npair_stddev_test number of positive gage/radar pairs,
     * then fit a gage value vs radar value regression line  and throw out
     * any gage/radar pair which is more than istd_cut standard deviations
     * away from the line
     * if there are < npair_stddev_test number of positive gage/radar pairs,
     * then assume a regression line with slope = longest term bias value 
     * if the gage value differs by more than gage_cut_value inches from the  
     * point on the line corresponding to the radar value, then throw out
     * the gage/radar pair
     **/

    if ( npair < 1 )
    {
       return ;
    }

    if(npair >= npair_stddev_test)
    {
        /**      
         * fit two/one-parameter least-square regression line 
         **/
        sum_gage = 0.0 ;
        sum_radar = 0.0 ;
        sum_gage_radar = 0.0 ;
        sum_radar2 = 0.0 ;

        for(i = 0; i < npair; i++)
        {
            sum_gage += 
                pGageRadarPairTable->ptrGageRadarPair[i].gageValue ;

            sum_radar += 
                pGageRadarPairTable->ptrGageRadarPair[i].radarValue ;

            sum_gage_radar += 
                pGageRadarPairTable->ptrGageRadarPair[i].gageValue *
                  pGageRadarPairTable->ptrGageRadarPair[i].radarValue ;

            sum_radar2 += 
                pGageRadarPairTable->ptrGageRadarPair[i].radarValue *
                pGageRadarPairTable->ptrGageRadarPair[i].radarValue ;
        }

        if(option == 1)
            slope = sum_gage_radar / sum_radar2 ;
        else
            slope = (npair * sum_gage_radar - sum_gage * sum_radar)
                  / (npair * sum_radar2 - sum_radar * sum_radar) ;

        /**      
         * check if the slope is positive 
         **/

        if(slope <= 0.0)
        {
            *flag = 1 ;
            return ;
        }

        if(option == 1)
            b = 0.0 ;
        else
            b = (sum_gage - sum_radar * slope) / npair ;

        /**      
         * compute error statistics
         **/
        sume1 = 0.0 ;
        sume2 = 0.0 ;        
        for(i = 0; i < npair; i++)
        {
            error = pGageRadarPairTable->ptrGageRadarPair[i].gageValue
                 - slope * pGageRadarPairTable->ptrGageRadarPair[i].radarValue
                 - b ;
            sume1 += error ;
            sume2 += error * error ;
        }

        ave = sume1 / npair ;
        var = sume2 / (npair - 1) - sume1 * sume1 / (npair * (npair - 1)) ;

        if(var <= 0.0)
        {
            *flag = 1 ;
            return ;
        }

        std = sqrt(var) ;

        /**      
         * standardize errors and cull pairs
         **/
        
        for(i = 0; i < npair; i++)
        {
            error = pGageRadarPairTable->ptrGageRadarPair[i].gageValue
                 - slope * pGageRadarPairTable->ptrGageRadarPair[i].radarValue
                 - b ;

            error = (error - ave) / std ;

            if(fabs(error) <= std_cut)
            {
                pGageRadarPairTable->ptrGageRadarPair[count].hrap_x = 
                    pGageRadarPairTable->ptrGageRadarPair[i].hrap_x ;

                pGageRadarPairTable->ptrGageRadarPair[count].hrap_y = 
                    pGageRadarPairTable->ptrGageRadarPair[i].hrap_y ;

                pGageRadarPairTable->ptrGageRadarPair[count].gageValue = 
                    pGageRadarPairTable->ptrGageRadarPair[i].gageValue ;

                pGageRadarPairTable->ptrGageRadarPair[count].radarValue = 
                    pGageRadarPairTable->ptrGageRadarPair[i].radarValue ;

                count ++ ;
            }
            else
            {
                sprintf ( message , "gage val = %6.2f  radar val = %6.2f"
                            " excluded because > %d stand dev from mean\n",
                            pGageRadarPairTable->ptrGageRadarPair[i].gageValue,
                            pGageRadarPairTable->ptrGageRadarPair[i].radarValue,
                            std_cut ) ;
                printMessage(message, logFile);        
            }
        }
    }
    else /* (npair < npair_stddev_test) */
    {
        slope = bias_long ;
        b = 0.0 ;

        for(i = 0; i < npair ; i ++)
        {
             error = pGageRadarPairTable->ptrGageRadarPair[i].gageValue
                - slope * pGageRadarPairTable->ptrGageRadarPair[i].radarValue
                - b ;
         
            if(fabs(error) <= gage_cut_value)
            {
                pGageRadarPairTable->ptrGageRadarPair[count].hrap_x = 
                    pGageRadarPairTable->ptrGageRadarPair[i].hrap_x ;

                pGageRadarPairTable->ptrGageRadarPair[count].hrap_y = 
                    pGageRadarPairTable->ptrGageRadarPair[i].hrap_y ;

                pGageRadarPairTable->ptrGageRadarPair[count].gageValue = 
                    pGageRadarPairTable->ptrGageRadarPair[i].gageValue ;

                pGageRadarPairTable->ptrGageRadarPair[count].radarValue = 
                    pGageRadarPairTable->ptrGageRadarPair[i].radarValue ;

                count ++ ;
            }
            else
            {
                sprintf ( message , "gage val = %6.2f  radar val = %6.2f"
                    " excluded because stand dev > %4.1f from mean\n",
                    pGageRadarPairTable->ptrGageRadarPair[i].gageValue,
                    pGageRadarPairTable->ptrGageRadarPair[i].radarValue,
                    gage_cut_value ) ;
                printMessage(message, logFile);        
            }
        }
    } /* (npair < npair_stddev_test) */

    pGageRadarPairTable->pairNum = count;

} /* end MPEFieldGen_gageRadarPairsQC */
