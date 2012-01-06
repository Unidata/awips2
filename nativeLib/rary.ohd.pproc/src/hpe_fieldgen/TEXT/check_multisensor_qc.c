/*******************************************************************************
* FILENAME:            check_multisensor_qc.c
*
* Purpose:
* This function is converted from FORTRAN code: mlts_qc.f.
* it performs QCs gage data using satellite precip, radar
* precip and lightning data.
*
* calling function: runERMosaic
* functions called: writeGageQC
*
* input variables
*
* datetime - YYYY-MM-DD HH:MM:SS formatted datetime
*
* mosaic - two-dimensional raw radar data
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain 
*
* pGageArray - array of gage data
*
*
* output variables
*
* qcGage  : Quality controlled gage data of dimension gage number 
*           double qcGage[gages_num]
*
*         :  qcGage code is -1 for multi sensor test (radar)
*         :  qcGage code is -2 for multi sensor test (satellite)
*         :  qcGage code is -3 for multi sensor test (lightning)
*         :  qcGage code is -9 for 8 point neighbor check
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   02/13/2001   C. R. Kondragunta first version
*   Sept. 2003   Feng Ding         modification
*   March 2005   Guoxian Zhou      finish conversion to C Language 
*   08/08/2005   Guoxian Zhou      finish component testing
*
********************************************************************************
*/

#include "empe_fieldgen.h"

void checkMultisensorQC(const char * datetime ,
                        double ** mosaic ,
                        const geo_data_struct *pGeoData,                    
                        const gage_table_struct * pGageArray)
{
    const int dur_1h = 1001 ;
    const int qctype = 2 ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int pseudoSize = pGageArray->pseudoGageNum ;
    const int gageSize   = pGageArray->totalGageNum ;
    const float threshold = 0.0 ;

    char tokenvalue[TOKEN_VALUE_LEN] = {'\0'} ;
    int i, j, k, i1, i2, j1, j2 ;
    int messageid ;
    long int irc = 0 ;
    static float precip_limit = 1.0 ;
    static int first = 1 ;
    int qcflag = 0 ;
    
    float ** dmgge;
    float ** dmgge1; 
    float * qcGage ;
    
    if(first == 1)
    {
        hpe_fieldgen_getAppsDefaults("mpe_msc_precip_limit", tokenvalue) ;

        precip_limit = atof(tokenvalue);

        if((precip_limit < 0.1) || (precip_limit > 10000.0)) 
            precip_limit = 1.0 ;

        sprintf( message , "Precipitation limit of Multisensor Check"
                " is: %f (mm)",    precip_limit ) ;
        hpe_fieldgen_printMessage( message );
        
        first = 0 ;
    }


    /*      
     * allocate memory for dmgge variable
     */

    dmgge = (float **)malloc(rowSize * sizeof(float *)); 
    if(dmgge == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in checkMultisensorQC function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < rowSize; i++)
    {
        dmgge[i] = (float *)malloc(colSize * sizeof(float)); 
        if(dmgge[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in checkMultisensorQC function."
                "\n\tProgram exit.") ;
            shutdown( message );
        }
        for(j = 0; j < colSize; j++)
        {
             dmgge[i][j] = -999.0 ;
        }
    }    

    /*      
     * allocate memory for dmgge1 variable
     */

    dmgge1 = (float **)malloc(rowSize * sizeof(float *)); 
    if(dmgge1 == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in checkMultisensorQC function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < rowSize; i++)
    {
        dmgge1[i] = (float *)malloc(colSize * sizeof(float)); 
        if(dmgge1[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in checkMultisensorQC function."
                "\n\tProgram exit.") ;
            shutdown( message );
        }
        for(j = 0; j < colSize; j++)
        {
             dmgge1[i][j] = -999.0 ;
        }
    }    

    /*      
     * allocate memory for qcGage variable
     */

    qcGage = (float *)malloc(gageSize * sizeof(float)); 
    if(qcGage == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in checkMultisensorQC function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }


    for(k = pseudoSize; k < gageSize; k++)
    {
        i = pGageArray->ptrGageRecords[k].hrap_y ;
        j = pGageArray->ptrGageRecords[k].hrap_x ;
         dmgge[i][j] = pGageArray->ptrGageRecords[k].gageValue ;
        dmgge1[i][j] = pGageArray->ptrGageRecords[k].gageValue ;
        qcGage[k]    = pGageArray->ptrGageRecords[k].gageValue ;
    }

    /**      
     * 3 point neighbor multi sensor check to identify stuck gages.
     * If radar estimates show at least 1 mm of rainfall in the any
     * of the neighboring eight gauges, then the gauge in the middle
     * is flagged as stuck     
     **/
    for(i = 0; i < colSize; i++)
    {
        for(j = 0; j < rowSize; j++)
        {
            if ((i == 0) ||
                (j == 0) ||
                (i == colSize - 1) ||
                (j == rowSize - 1) )
            {
                if ((dmgge[j][i] == 0.0) &&
                    (mosaic[j][i] > 1.0) )
                    dmgge[j][i] = -10000.0 ;
            }
            else
            {
                i1 = i - 1 ;
                i2 = i + 1 ;
                j1 = j - 1 ;
                j2 = j + 1 ;
                if((dmgge[j][i] == 0.0) &&
                 ((mosaic[j1][i1] > precip_limit) ||
                  (mosaic[j][i1] > 1.0) ||
                  (mosaic[j2][i1] > precip_limit) || 
                  (mosaic[j1][i] > 1.0) || 
                  (mosaic[j][i] > precip_limit) ||
                  (mosaic[j2][i] > 1.0) ||
                  (mosaic[j1][i2] > precip_limit) ||
                  (mosaic[j][i2] > 1.0) ||
                  (mosaic[j2][i2] > precip_limit) ) )
                {
                     dmgge[j][i] = -10000.0 ;
                }
            }
        }
    }

    /**      
     * Four side point checking is done here for ZERO value (stuck) gages.
     * Checks for rainfall in all the neighboring grid points.
     * If at least one box in all four directions has rainfall
     * greater that ZERO, then that particual gage with zero 
     * value is flagged as sruck and set to -9
     **/

    for(i = 1; i < colSize - 1; i++)
    {
        for(j = 1; j < rowSize - 1; j++)
        {
            i1 = i - 1 ;
            i2 = i + 1 ;
            j1 = j - 1 ;
            j2 = j + 1 ;
            if (dmgge1[j][i] == 0.0 )
            {
                if(( dmgge1[j1][i1] > 0.0 ||
                     dmgge1[j][i1] > 0.0  ||
                     dmgge1[j2][i1] > 0.0 ) && 
                     ( dmgge1[j1][i1] > 0.0 ||
                       dmgge1[j1][i] > 0.0 || 
                       dmgge1[j1][i2] > 0.0 ) &&
                       ( dmgge1[j1][i2] > 0.0 ||
                         dmgge1[j][i2] > 0.0 ||
                         dmgge1[j2][i2] > 0.0 ) &&
                        ( dmgge1[j2][i1] > 0.0 ||
                          dmgge1[j2][i] > 0.0 ||
                          dmgge1[j2][i2] > 0.0 ))
                {
                     dmgge1[j][i] = -10000.0 ;
                }
            }
        }
    }

    int flag = 0 ;
    for(k = pseudoSize; k < gageSize; k++)
    {
        i = pGageArray->ptrGageRecords[k].hrap_y ;
        j = pGageArray->ptrGageRecords[k].hrap_x ;

        if(dmgge[i][j] == -10000.0)
        {
            if(flag == 0)
            {
                sprintf ( message , "  Gauge ID  Value(mm)  "
                    "Radar mosaic check"
                    "  Surrounding rain gauge check") ;
                hpe_fieldgen_printMessage( message );        
                flag = 1 ;
            }
            qcGage[k] = -1.0 ;
            sprintf ( message , "  %8s %10.4f      Failed",
                    pGageArray->ptrGageRecords[k].gageID,
                    pGageArray->ptrGageRecords[k].gageValue ) ;

            hpe_fieldgen_printMessage( message );
        }
        else if(dmgge1[i][j] == -10000.0)
        {
            if(flag == 0)
            {
                sprintf ( message , "  Gauge ID  Value(mm)  "
                    "Radar mosaic check"
                    "  Surrounding rain gauge check") ;
                hpe_fieldgen_printMessage( message );        
                flag = 1 ;
            }
            qcGage[k] = -9.0 ;
            sprintf ( message , "  %8s %10.4f      Failed",
                    pGageArray->ptrGageRecords[k].gageID,
                    pGageArray->ptrGageRecords[k].gageValue ) ;
            hpe_fieldgen_printMessage( message );
        }
        else
        {
            qcGage[k] = pGageArray->ptrGageRecords[k].gageValue ;
        }
    }

    for(k = pseudoSize; k < gageSize; k++)
    {
        if (qcGage[k] < threshold)
        {
            qcflag = -1 ;
            irc = 0 ;
            messageid = 0 ;

            writeGageQC( pGageArray->ptrGageRecords[k].gageID ,
                        pGageArray->ptrGageRecords[k].gageValue ,
                        pGageArray->ptrGageRecords[k].gageTS ,
                        dur_1h, datetime, qctype, &irc, &messageid) ;

            if (messageid == 1)
            {
                if(irc == -284)
                {
                    sprintf ( message , "Two gages have same "
                            "gageid, obstime, and dur:\n"
                            "gageid:%s, datetime:%s, qctype:%d, qcflag:%d ",
                            pGageArray->ptrGageRecords[k].gageID,
                            datetime, qctype, qcflag ) ;
                    hpe_fieldgen_printMessage( message );
                }
                else
                {
                    sprintf ( message , "Database error # %ld occurred "
                            "attempting to select quality_code "
                            " from procprecip table for this gage:\n"
                            "gageid:%s, datetime:%s, qctype:%d, qcflag:%d ",
                            irc, pGageArray->ptrGageRecords[k].gageID,
                            datetime, qctype, qcflag ) ;
                    hpe_fieldgen_printMessage( message );
                }
            }
            else if (messageid == 2)
            {
                sprintf ( message, "qctype value is not valid for this gage:\n"
                        "gageid:%s, datetime:%s, qctype:%d, qcflag:%d ",
                        pGageArray->ptrGageRecords[k].gageID,
                        datetime, qctype, qcflag ) ;
                hpe_fieldgen_printMessage( message );
            }
            else if (messageid == 3)
            {
                sprintf ( message , "set_qccode return value"
                        " is not valid for this gage:\n"
                        "gageid:%s, datetime:%s, qctype:%d, qcflag:%d ",
                        pGageArray->ptrGageRecords[k].gageID,
                        datetime, qctype, qcflag ) ;
                hpe_fieldgen_printMessage( message );
            }
            else if (messageid == 4)
            {
                sprintf ( message , "Database error # %ld occurred "
                        "attempting to update quality_code "
                        " from procprecip table for this gage:\n"
                        "gageid:%s, datetime:%s, qctype:%d, qcflag:%d ",
                        irc, pGageArray->ptrGageRecords[k].gageID,
                        datetime, qctype, qcflag ) ;
                hpe_fieldgen_printMessage( message );
            }
            else if (messageid == 5)
            {
                sprintf ( message , "Database error # %ld occurred. "
                        "Could not write nulll value "
                        " in procprecip table for this gage:\n"
                        "gageid:%s, datetime:%s, qctype:%d, qcflag:%d ",
                        irc, pGageArray->ptrGageRecords[k].gageID,
                        datetime, qctype, qcflag ) ;
                hpe_fieldgen_printMessage( message );
            }
        }
    }


    for(i = 0; i < rowSize; i++)
    {
        if(dmgge[i] != NULL)
        {
            free(dmgge[i]);
            dmgge[i] = NULL;
        }
    }
    if(dmgge != NULL)
    {
        free(dmgge);
        dmgge = NULL;
    }

    for(i = 0; i < rowSize; i++)
    {
        if(dmgge1[i] != NULL)
        {
            free(dmgge1[i]);
            dmgge1[i] = NULL;
        }
    }
    if(dmgge1 != NULL)
    {
        free(dmgge1);
        dmgge1 = NULL;
    }

    if(qcGage != NULL)
    {
        free(qcGage);
        qcGage = NULL;
    }

}
