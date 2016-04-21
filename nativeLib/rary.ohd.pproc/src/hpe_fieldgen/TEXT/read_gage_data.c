/*******************************************************************************
* FILENAME:  read_gage_data.c
*
* Purpose:
* This function is converted from FORTRAN code: read_gage_data.f.
* it reads multiple hour rain gage data.
* data is read from the PseudoGageVal and ProcPrecip tables
*
* calling function: main_mpe_fieldgen
* functions called: readPseudoPrecip, readGagePrecip, checkMultiple
*                   read_lightning, writeGageQC
*
* input variables
*
* pRunDate - date and time of current run.
*
* pMPEParams - static parameters
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain 
*
* output variables
*
* pGageTable - array of gage data
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   01/25/2005   Guoxian Zhou      convert to C Language 
*   03/22/2005   Guoxian Zhou      retrieve multiple hours data
*   06/29/2005   Guoxian Zhou      finish component testing
********************************************************************************
*/

#include "empe_fieldgen.h"

void readGageData(const run_date_struct * pRunDate ,
                 const empe_params_struct * pMPEParams , 
                 const geo_data_struct *pGeoData ,
                 gage_table_struct ** pGageTable ,
                 gage_table_struct ** pGageTableP3 ,
                 gage_table_struct ** pQCGageTable)
{
    int qctype, boxfailed;
    char tokenvalue[TOKEN_VALUE_LEN] = {'\0'};
    int *gageqc ;
    int curr_hrap_x, curr_hrap_y; 
    long int irc;
    int dur_1h ;
    int messageid, num_strike, i, j ;
    char ** datetimes = NULL ;
    time_t time_in_ticks;
    struct tm * pRunTime = NULL ;
    
    const int runHours = pRunDate->hourNum ;
    int * gageNumber = NULL ;  /* store the max size for gage record */

    /* store the max size for gage record for P3LMOSAIC products */

    int * gageNumberP3 = NULL ;  

    /*
     * qctype = 0 for setting quality_code back to Default value
     *        = 1 for SCC_QC: Spatial Consistency Check, 
     *        = 2  for MSC_QC: Multisensor Consistency Check      
     */

    qctype = 1;

    /*
     * dur_1h = 1001 is the value of duration in "procprecip" table.
     */

    dur_1h = 1001;

    /*
     * allocate memory and
     * initialize the gageNumber array to GAGE_NUMBER.
     */

    gageNumber = (int *)malloc(runHours * sizeof(int) ) ;
    if(gageNumber == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
                        " in readGageData function."
                        "\n\tProgram exit.");
        shutdown( message);
    }
    for(i = 0; i < runHours; i++)
    {
        gageNumber[i] = GAGE_NUMBER ;
    }

    gageNumberP3 = (int *)malloc(runHours * sizeof(int) ) ;
    if(gageNumberP3 == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
                        " in readGageData function."
                        "\n\tProgram exit.");
        shutdown( message);
    }
    for(i = 0; i < runHours; i++)
    {
        gageNumberP3[i] = GAGE_NUMBER_P3 ;
    }

    /*
     * create run time strings array.
     */

    datetimes = (char **)malloc(runHours * sizeof(char *) ) ;
    if(datetimes == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
                        " in readGageData function."
                        "\n\tProgram exit.");
        shutdown( message);
    }

    time_in_ticks = pRunDate->tRunTime;



    for(i = runHours - 1; i >= 0; i--)
    {
        datetimes[i] = 
            (char *)malloc((ANSI_YEARSEC_TIME_LEN + 1) * sizeof(char)); 
        if(datetimes[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                                " in readGageData function."
                                "\n\tProgram exit.") ;
            shutdown( message);
        }

        memset ( datetimes[i] , '\0' , ANSI_YEARSEC_TIME_LEN + 1 ) ;
        pRunTime = gmtime(&time_in_ticks) ;
        strftime ( datetimes[i], ANSI_YEARSEC_TIME_LEN + 1,
            "%Y-%m-%d %H:00:00", pRunTime ) ;
        time_in_ticks -= SECONDS_PER_HOUR;
    }




    /*
     * Read the pseudo gage values for this hour.  This now uses the
     * PseudoGageVal dbgen routines.
     */

    readPseudoPrecip(runHours, datetimes, 
                     pMPEParams->hrap_grid_factor,
                     pGeoData, pGageTable, pGageTableP3,
                     gageNumber, gageNumberP3 ); 

    /*
     * Read and compute hourly precipitation amounts from data 
     * stored in the HourlyPC and HourlyPP tables.  This is 
     * new functionality and is the result of the gage precipitation
     * processor.  Added by Bryon Lawrence, September 9, 2004.
     */

    readGagePrecip (runHours, datetimes,
                    pMPEParams->hrap_grid_factor,
                    pGeoData, pMPEParams->gage_qc, 
                    pGageTable, pGageTableP3, gageNumber, gageNumberP3 ) ;

    /*
     * fill in the qc gage struct data.
     */

    for(i = 0; i < pRunDate->hourNum; i++)
    {
        pQCGageTable[i]->pseudoGageNum
            = pGageTable[i]->pseudoGageNum;
        pQCGageTable[i]->totalGageNum    
            = pGageTable[i]->totalGageNum;

        if(pGageTable[i]->totalGageNum > 0)
        {
            for(j = 0; j < pGageTable[i]->totalGageNum; j ++)
            {
                pQCGageTable[i]->ptrGageRecords[j].gageValue
                    = pGageTable[i]->ptrGageRecords[j].gageValue ;

                pQCGageTable[i]->ptrGageRecords[j].hrap_x
                    = pGageTable[i]->ptrGageRecords[j].hrap_x ;

                pQCGageTable[i]->ptrGageRecords[j].hrap_y
                    = pGageTable[i]->ptrGageRecords[j].hrap_y ; 

                strcpy(pQCGageTable[i]->ptrGageRecords[j].gageID,
                        pGageTable[i]->ptrGageRecords[j].gageID) ;

                strcpy(pQCGageTable[i]->ptrGageRecords[j].gageTS,
                        pGageTable[i]->ptrGageRecords[j].gageTS) ;

                pQCGageTable[i]->ptrGageRecords[j].latitude
                    = pGageTable[i]->ptrGageRecords[j].latitude ;

                pQCGageTable[i]->ptrGageRecords[j].longitude
                    = pGageTable[i]->ptrGageRecords[j].longitude ; 
            }
        }
        else
        {
            pQCGageTable[i]->ptrGageRecords = NULL ;
        }
    }

    for(i = 0; i < pRunDate->hourNum; i++)
    {
        if(pGageTable[i]->totalGageNum > 1)
        {
            /*
             * check for duplicate and/or multiple entries
             * in the same HRAP bin:
             * if found, perform arithmetic averaging
             */

            checkMultiple( pGageTable[i]) ;
        }
        if(pGageTableP3[i]->totalGageNum > 1)
        {
            
              // check for duplicate and/or multiple entries
              // in the same HRAP bin:
              // if found, perform arithmetic averaging
             
            checkMultiple( pGageTableP3[i]) ;
        }
    }

    /*
     * Start of Feng Ding mod. 
     * Spatial Consistency Check 
     */

    if(pMPEParams->gage_qc != 1)
    {
        return ;
    }

    sprintf( message , "\nGage QC - Spatial Consistency Check") ;
    hpe_fieldgen_printMessage( message);

    boxfailed = 4 ;
    if(hpe_fieldgen_getAppsDefaults("mpe_scc_boxes_failed", tokenvalue) != -1)
    {
        if(hpe_fieldgen_isDigits(tokenvalue) == 1)
        {
            boxfailed = atoi(tokenvalue);
            if((boxfailed < 1) || (boxfailed > 4))
            {
                boxfailed = 4 ;
            }
        }
    }

    sprintf( message , "\tLimit of number of boxes failed is: %d", boxfailed );
    hpe_fieldgen_printMessage( message);

    for(i = 0; i < runHours; i++)
    {
        if( pGageTable[i]->pseudoGageNum == pGageTable[i]->totalGageNum )
        {
            sprintf( message , "STATUS: No hourly gages data available"
                " for %s, skip Spatial Consistency Check.",
                datetimes[i] ) ;
            hpe_fieldgen_printMessage( message);
        }
        else
        {
            gageqc = (int *)malloc(pGageTable[i]->totalGageNum * sizeof(int)) ;
            if(gageqc == NULL)
            {
                sprintf ( message , "ERROR: Memory allocation failure"
                                " in readGageData function."
                                "\n\tProgram exit.");
                shutdown( message);
            }

            hpe_fieldgen_getCurrentTime(currTime) ;
            sprintf( message , "%s = time begin spatial consistency check "
                     "for %s", currTime, datetimes[i]) ;
            hpe_fieldgen_printMessage( message);

            sprintf( message , "STATUS:mpe_fieldgen  in readGageData() before call to checkSpatialConsistency() - 6.5 \n");
                            printMessage( message, logFile );
                            fflush(logFile);
                            fflush(stdout);


            checkSpatialConsistency(pMPEParams, pGeoData,
                                    pGageTable[i], gageqc); 

            sprintf( message , "STATUS:mpe_fieldgen  in readGageData() after call to checkSpatialConsistency() - 6.6 \n");
                                      printMessage( message, logFile );
                                      fflush(logFile);
                                      fflush(stdout);


            hpe_fieldgen_getCurrentTime(currTime) ;
            sprintf( message , "%s = time end spatial consistency check "
                     "for %s", currTime, datetimes[i]) ;
            hpe_fieldgen_printMessage( message);

            sprintf( message , "Gage QC - Lightning Check\n" 
                    "%s = time begin lightning check "
                    "for %s", currTime, datetimes[i]) ;
            hpe_fieldgen_printMessage( message);

            for( j = pGageTable[i]->pseudoGageNum;
                 j < pGageTable[i]->totalGageNum;
                 j++)
            {
                if (gageqc[j] >= boxfailed)
                {
                    num_strike = 0 ;
                    curr_hrap_x = pGageTable[i]->ptrGageRecords[j].hrap_x;
                    curr_hrap_y = pGageTable[i]->ptrGageRecords[j].hrap_y;
                    irc = 0 ;

                    read_lightning(datetimes[i], &curr_hrap_x,
                        &curr_hrap_y, &num_strike, &irc) ;

                    if(num_strike > 0)
                    {
                        gageqc[j] = 0 ;
                        sprintf( message , "\tNot flagged %s "
                            "since lightning detected.", 
                            pGageTable[i]->ptrGageRecords[j].gageID);
                        hpe_fieldgen_printMessage( message);
                    }
                    else
                    {
                        if (irc < 0)
                        {
                            sprintf( message , "Database error %ld occurred "
                                "attempting to select num_strike"
                                " in lightning table.", irc);
                            hpe_fieldgen_printMessage( message);
                        }

                        irc = 0 ;
                        messageid = 0 ;
                        writeGageQC(pGageTable[i]->ptrGageRecords[j].gageID,
                                    pGageTable[i]->ptrGageRecords[j].gageValue,
                                    pGageTable[i]->ptrGageRecords[j].gageTS,
                                    dur_1h, datetimes[i], qctype, &irc,
                                    &messageid) ;

                        if (messageid == 1)
                        {
                            if(irc == -284)
                            {
                                sprintf( message , "Two gages have same "
                                    " gageid, obstime, and dur:");
                                hpe_fieldgen_printMessage( message);
                                sprintf( message , "\t%8s %20s %6d %6d", 
                                    pGageTable[i]->ptrGageRecords[j].gageID,
                                    datetimes[i], qctype, gageqc[j]);
                                hpe_fieldgen_printMessage( message);
                            }
                            else 
                            {
                                sprintf( message , "Database error# %ld"
                                    " occurred attempting to select "
                                    " quality_code from procprecip table"
                                    " for this gage:", irc);
                                hpe_fieldgen_printMessage( message);
                                sprintf( message , "\t%8s %20s %6d %6d", 
                                    pGageTable[i]->ptrGageRecords[j].gageID,
                                    datetimes[i], qctype, gageqc[j]);
                                hpe_fieldgen_printMessage( message);
                            }
                        }
                        else if (messageid == 2)
                        {
                            sprintf( message , "qctype value is not valid"
                                " for this gage:");
                            hpe_fieldgen_printMessage( message);
                            sprintf( message , "\t%8s %20s %6d %6d", 
                                pGageTable[i]->ptrGageRecords[j].gageID,
                                datetimes[i], qctype, gageqc[j]);
                            hpe_fieldgen_printMessage( message);
                        }
                        if (messageid == 3)
                        {
                            sprintf( message , "set_qccode return value is "
                                    "not valid for this gage:");
                            hpe_fieldgen_printMessage( message);
                            sprintf( message , "\t%8s %20s %6d %6d", 
                                pGageTable[i]->ptrGageRecords[j].gageID,
                                datetimes[i], qctype, gageqc[j]);
                            hpe_fieldgen_printMessage( message);
                        }
                        if (messageid == 4)
                        {
                            sprintf( message , "Database error# %ld"
                                    " occurred. attempting to update"
                                    " quality_code in procprecip table"
                                    " for this gage:", irc);
                            hpe_fieldgen_printMessage( message);
                            sprintf( message , "\t%8s %20s %6d %6d", 
                                pGageTable[i]->ptrGageRecords[j].gageID,
                                datetimes[i], qctype, gageqc[j]);
                            hpe_fieldgen_printMessage( message);
                        }
                        if (messageid == 5)
                        {
                            sprintf( message , "Database error# %ld"
                                  " occurred. Could not write null value"
                                  " in procprecip table for this gage:", irc);
                            hpe_fieldgen_printMessage( message);
                            sprintf( message , "\t%8s %20s %6d %6d", 
                                pGageTable[i]->ptrGageRecords[j].gageID,
                                datetimes[i], qctype, gageqc[j]);
                            hpe_fieldgen_printMessage( message);
                        }
                    }
                }
            }
            hpe_fieldgen_getCurrentTime(currTime) ;
            sprintf( message , "\tUpdate QC setting in HourlyPP table for"
                    " gauges \n\texceeded threshold for %d"
                    " boxes and no lightning nearby.\n"
                    "%s = time end   lightning check "
                    "for %s", boxfailed, currTime, datetimes[i]) ;
            hpe_fieldgen_printMessage( message);

            if(gageqc != NULL)
            {
                free(gageqc); 
                gageqc = NULL ;
            }
        }
    }

    /* End of Feng Ding mod. */
    
    if(gageNumber != NULL)
    {
        free(gageNumber);
        gageNumber = NULL ;
    }
    if(gageNumberP3 != NULL)
    {
        free(gageNumberP3);
        gageNumberP3 = NULL ;
    }
   
    if(datetimes != NULL)
    {
        for(i=0; i < runHours; i++)
        {
            free(datetimes[i]) ;
            datetimes[i] = NULL ;
        }
    }

    if(datetimes != NULL)
    {        
        free(datetimes) ;
        datetimes = NULL ;
    }
}
