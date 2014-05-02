/***********************************************************************
* Filename: read_mean_bias.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: October 2006
*
* Development Group: OHD
*
* Description:
* Contains routine for reading the mean bias value from the database.
* 
* Modules:
* readMeanBias
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: readMeanBias
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: October 2006
*
* Description:
* This function is modified from the get_mean_bias.c.
* 
* Calling Arguments:
* Name            Input/Output Type          Description
*
* pRunDate        Input     const run_date_struct *
*                                            date/time 
* pRadarLocRecord Input     const radarLoc_table_struct *
*                                            info from radarLoc table
* pMPEParams      Input     const mpe_params_struct *
*                                            static parameters
* dualpol_data_avail Input  int              0/1 represent if dual pol data is available
* meanFieldBias   Output    double[][]       the mean field bias value
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* readRadarResult, binary_search
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
* 10/24/2006  Guoxian Zhou      modification for empe version 
* 07/2013     JingtaoD          dual pol
***********************************************************************/
extern int dualpol_on_flag;
void readMeanBias(const run_date_struct * pRunDate,
                  const radarLoc_table_struct * pRadarLocTable ,
                  const empe_params_struct * pMPEParams ,
                  double * meanFieldBias,
                  int dualpol_data_avail)
{
    static char datetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    static char radarID[ RADAR_ID_LEN + 1] = {'\0'} ;

    int i;
    short pps_radar_count, dp_radar_count;
    int blnEditBias,  blnIgnoreRadar ;

    double editBiasValue = 0.0 ;
    double memSpanBias ;
    double meanBias = 0.0 ;

    struct tm * pRunTime = NULL;
    long int    irc ;

    int  dualpol_meanbias_flag[MAX_RADAR_NUM];
    /*int  dualpol_data_avail[MAX_RADAR_NUM]; */
    
    /* initialize no dualpol MFB and no dual pol product avialble for each radar */
    for (i = 0; i < MAX_RADAR_NUM; i++)
    {
       dualpol_meanbias_flag[i] = 0;
     /*  dualpol_data_avail[i] = 0;*/
    }
       
    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime ( datetime, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y-%m-%d %H:00:00", pRunTime ) ;

    
    /* for single pol product which use rwradarresult table */
    radar_result_struct * pRadarResult = NULL;

    
    /* for dual pol proudcts which use daaradarresult table */    
    radar_result_struct * pDAARadarResult = NULL;
   
    /* malloc space */
    pRadarResult = (radar_result_struct *) 
        malloc(pRadarLocTable->radarNum * sizeof(radar_result_struct)); 
    if(pRadarResult == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in readMeanBias function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    
    pDAARadarResult = (radar_result_struct *) 
        malloc(pRadarLocTable->radarNum * sizeof(radar_result_struct)); 
    if(pDAARadarResult == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in readMeanBias function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }

    for(i = 0; i < pRadarLocTable->radarNum; i++ )
    {
        memset(pRadarResult[i].radID, '\0', RADAR_ID_LEN + 1) ;
        pRadarResult[i].edit_bias = 0;
        pRadarResult[i].ignore_radar = 0;
        pRadarResult[i].bias = 0.0 ;
	
	memset(pDAARadarResult[i].radID, '\0', RADAR_ID_LEN + 1) ;
        pDAARadarResult[i].edit_bias = 0;
        pDAARadarResult[i].ignore_radar = 0;
        pDAARadarResult[i].bias = 0.0 ;
    }

    /*      
     * Read the edit bias flag, edited bias, ignore radar flag
     * from RWRadarResult table for the top hour
     */

     /* when dualpol_on_flag is set as zero, no any dual pol products will be used even
      though they are available. Only single pol radar producats are used. This keeps the old
      functionality */
   
    if (dualpol_on_flag == 0)
    { 
       sprintf( message, "STATUS: in readMeanBias function, try to load mean bias from RWRadarResult table."); 
       printLogMessage( message );
       
       readRadarResult (datetime, pRadarResult, &pps_radar_count, dualpol_meanbias_flag, &irc) ; /* for single pol */
      
       if (irc < 0)
       {
          sprintf ( message , "ERROR: in readMeanBias function. Database error #%ld attempting to "
                    "select record from RWRadarResult table.", irc) ;
          printLogMessage( message );
       }            
    }
    /* Able to use dual pol products if available for this radar, if not available for this radar, use the single
    pol product */ 
   else
   {
      sprintf( message, "STATUS: in readMeanBias function, try to load mean bias from DAARadarResult table");     
      printLogMessage( message );
       
      readDAARadarResult (datetime, pDAARadarResult, &dp_radar_count, dualpol_meanbias_flag, &irc) ; /* for dual pol */
      if (irc < 0  || dp_radar_count <= 0)
      {
          if (irc < 0)
	  { 
             sprintf ( message , "ERROR: in readMeanBias function. Database error #%ld attempting to "
                      "select record from DAARadarResult table. Try RWRadarResult table.", irc) ;
             printLogMessage( message );
	  } 
	  else if (dp_radar_count <= 0)
	  {
	     sprintf ( message , "STATUS: in readMeanBias function. No data found for radar "
                       "from DAARadarResult table. Try RWRadarResult table.") ;
             printLogMessage( message );
	  }       
	  
	  readRadarResult (datetime, pRadarResult, &pps_radar_count, dualpol_meanbias_flag, &irc) ; /* for single pol */  
	  if (irc < 0)
          {
             sprintf ( message , "ERROR: in readMeanBias function. Database error #%ld attempting to "
                       "select record from RWRadarResult table.", irc) ;
             printLogMessage( message );
          }
	  
      }
                     
    }

    /*      
     * begin loop on radars
     */

    for(i = 0; i < pRadarLocTable->radarNum; i++ )
    {
        meanFieldBias[i] = 0.0 ;

        strcpy(radarID, pRadarLocTable->ptrRadarLocRecords[i].radarID) ;

        /*
         * initialize edit bias flag, ignore radar flag
         */

        blnEditBias    = 0 ;
        blnIgnoreRadar = 0 ;
        editBiasValue  = 0.0 ;

        radar_result_struct * pRadarInfo = NULL ;

        if (dualpol_meanbias_flag[i] == 1)
	   pRadarInfo = (radar_result_struct *)
                         binary_search ( pDAARadarResult, radarID, dp_radar_count,
                         sizeof ( radar_result_struct), compare_radar_id );
	else
           pRadarInfo = (radar_result_struct *)
                         binary_search ( pRadarResult, radarID, pps_radar_count,
                         sizeof ( radar_result_struct), compare_radar_id );

        /*
         * load in the radar info 
         */

        if ( pRadarInfo != NULL )
        {
            editBiasValue  = pRadarInfo->bias;
            blnEditBias    = pRadarInfo->edit_bias;
            blnIgnoreRadar = pRadarInfo->ignore_radar;
        }

        /*      
         * load the mean field bias value 
         * and save it to meanFieldBias[i]
         */

       retrieveMeanBias(radarID, datetime, pMPEParams , dualpol_data_avail,
                        &meanBias, &memSpanBias);

        meanFieldBias[i] = meanBias ;

        /*
         * if blnEditBias = 1, then use edited bias value
         */

        if(blnEditBias == 1)
        {
            meanFieldBias[i] = editBiasValue ;

            sprintf ( message , "in readMeanBias function. Edited bias value = %4.2f used.",
                editBiasValue);
            hpe_fieldgen_printMessage( message );        
        }
    }

    if(pRadarResult != NULL)
    {
        free(pRadarResult);
        pRadarResult = NULL;
    }

}
