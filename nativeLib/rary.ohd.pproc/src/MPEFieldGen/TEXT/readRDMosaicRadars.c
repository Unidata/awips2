/*******************************************************************************
* FILENAME:            readRDMosaicRadars.c
*
* Purpose:
* reads the radar precip accumulation array
* if radar has a dual-pol product use it else use single-pol product
*
* calling function: run_rdmosaic
* functions called: haveUsefulDAARadar, haveUsefulDPARadar       
*
* input variables
*
* datetime - date and time of current run in timestamp format
*          - minutes and seconds set to 00
*
* dpa_wind -  parameters
* daa_wind -  parameters
*
* output variables
*
* radar - two-dimensional array of radar data
*
* radarAvailFlag - DAA/DPA radar availability flag
*                - for radar fields in the RDMOSAIC field
*                - used to define the rad_avail field in the DAARadarResult table
*                - used to define the iflarad_dp[n] values used by the GUI
*                - note that this flag can pertain to a DPA or a DAA product
*     = 0 -- no radar data available OR radar data product available but field set to missing
*     = 1 -- radar data available (some values > 0.0)
*     = 2 -- radar data available (all values = 0.0)
*
* daa_avail_flag - DAA radar availability flag
*                - used for DAA MFB generation
*                - note that this flag is different from the radarAvailFlag (above)
*     = 0 -- DAA radar product not available
*     = 1 -- DAA radar product available
*
* ignoreDAARadar - Ignore DAA Radar Product Flag
*                - the DAA Radar product can be set to "ignored" via the 4-panel "single-site" window 
*                = 0 -- ignore flag not set (use radar) (default)
*                = 1 -- ignore flag set via 4-panel window 
*
* ignoreDPARadar - Ignore DPA Radar Product Flag
*                - the DPA Radar product can be set to "ignored" via the 4-panel "single-site" window	
*                = 0 -- ignore flag not set (use radar) (default)
*                = 1 -- ignore flag set via 4-panel window
*
* useDAA - Use DAA radar product flag
*        - combines ignoreDAARadar flag with check if useful DAA product is available
*        = 0 -- DAA product not available or ignored
*        = 1 -- DAA product available 
*
* useDPA - Use DPA radar product flag
*        - combines ignoreDPARadar flag with check if useful DPA product is available
*        = 0 -- DPA product not available or ignored
*        = 1 -- DPA product available
*
* haveGoodRadarProduct - DAA/DPA good radar product flag
*                      = 0 -- good radar product is NOT available (set field to all missing)
*                      = 1 -- good radar product available (could be all 0.0 product)
**
********************************************************************************
*/

/*
 * 	* SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2014               ptilles     Initial creation/ checkin for 14.3.1
 * Dec 12, 2014  16804	  cgobs       Fix problems with NULL Products' being considered missing, when certain null products
 * 									  should be considered all zero fields. (RM 16804 = DIM 17655)
 *
 *
 */

#include "mpe_fieldgen.h"
#define ACC_MIN 0.01

void readRDMosaicRadars(const char * radarID,
                const char * datetime,
                const int    dpa_wind, 
                const int    daa_wind, 
                const int    minCoverageDur, 
                const int    ignoreDPARadar,
                const int    ignoreDAARadar,
                float radar [ ] [ NUM_DPA_COLS ] ,
                int *    radarAvailFlag,
                short *    daa_avail_flag)
{
    char    fname[FNAME_LEN] = "" ;
    int     i, j ;
    int     itim, coverageDur, nullProductFlag;
    int len, ierr ;
    double  bias, xmax ;
    long int irc = 0 ;
    int useDAA = 0;
    int useDPA = 0;
    
    /**
     * the dpa product is coded as a value between 0 and 255.   0 and 255
     * are special values. when converting from level (0-255) to dba, a 
     * level of "0" is set to -98 dba.  -98 dba = 1.58489e-10 mm of 
     * accumulation.   this small positive value must be set to zero.
     * the acc_min theshold is used to check for values less than the 
     * minimum detectible value so that they can be set to zero.
     **/

    *radarAvailFlag = 0 ;
    *daa_avail_flag = 0;
      
   if(ignoreDAARadar == 1)
   {
      sprintf ( message , "STATUS: DAA radar product ignored") ;
      printMessage(message, logFile);
   }

   if(ignoreDPARadar == 1)
   {
      sprintf ( message , "STATUS: DPA radar product ignored") ;
      printMessage(message, logFile);
   }

   useDAA = (! ignoreDAARadar && haveUsefulDAARadar(radarID, datetime, daa_wind, minCoverageDur, radar,
                                                      radarAvailFlag) );
   
   if (useDAA)
   {
       *daa_avail_flag = 1;
   }
   
   else //try DPA
   {
       useDPA = (! ignoreDPARadar && haveUsefulDPARadar(radarID, datetime, dpa_wind, radar, radarAvailFlag));

       if ( ! useDPA )
       {

          /*---------------------------*/
          /* set radar to all missing  */
          /*---------------------------*/

          for( i = 0; i < NUM_DPA_COLS; i++)
          {
              for( j = 0; j < NUM_DPA_COLS; j++)
	      {
                  radar[i][j] = RADAR_DEFAULT ;
              }
          }

          sprintf ( message , "STATUS: radar marked as missing .") ;
          printMessage(message, logFile);

       }

   }

   return;
   
}
   
//------------------------------------------------------------------------------------------
   
int haveUsefulDAARadar(const char * radarId, const char * datetime, const int daa_wind,
                const int minCoverageDur, float radar [ ] [ NUM_DPA_COLS ] , 
                int * radarAvailFlag)


{
    char    fname[FNAME_LEN] = "" ;
    int     i, j ;
    int     itim, coverageDur, nullProductFlag;
    int len, ierr ;
    int haveGoodRadarProduct = 0;
    double  bias, xmax ;
    long int irc = 0 ;

    readDAARadar(radarId, datetime, daa_wind, &xmax, &bias, fname, &itim,
                 &coverageDur, &nullProductFlag, &irc) ;

    if(irc == 0) //DAA is available
    {

        sprintf ( message , "DAA product found for radar = %s\n", radarId) ;
        printMessage(message, logFile);

        *radarAvailFlag = 1 ;

        if(itim != 0)
        {
            sprintf ( message , "STATUS: no top-of-hour product found -- %d, "
                "product used instead.", itim ) ;
            printMessage(message, logFile);
        }
        
        len = strlen(fname) ;
        ierr = 0 ;
  

        /*---------------------------------------*/
        /* check if product is a null product    */
        /*---------------------------------------*/
        if(nullProductFlag == 0) //not a null product
        {
            sprintf ( message , "Maximum radar value = %7.2f mm", xmax ) ;
            printMessage(message, logFile);

            sprintf ( message , "reading DAA decoded product with filename = %s\n", fname);
            printMessage(message, logFile);

            read_daa_decoded(fname, &len, radar, &ierr) ;

            if(ierr != 0)
            {
             /*-------------------------------------------------------*/
             /* error reading DAA product - missing data substituted  */
             /*-------------------------------------------------------*/
             
                sprintf ( message , "ERROR: #%d encountered reading radar file = %s"
                          " -- missing data substituted.", ierr, fname ) ;
                printMessage(message, logFile);
                
                 haveGoodRadarProduct = 0;
                *radarAvailFlag = 0 ;

                for( i = 0; i < NUM_DPA_COLS; i++)
                {
                    for( j = 0; j < NUM_DPA_COLS; j++)
            		  radar[i][j] = RADAR_DEFAULT ;
                }
            }

            else //ierr == 0, which means it is a good product
            {
                haveGoodRadarProduct = 1;
        	    *radarAvailFlag = 1 ;
            }
        
        } // end if (nullProductFlag == 0)
       
        else //is a null product of some kind
        {

    	   /*--------------------------------*/
    	   /* null product processing        */
    	   /*--------------------------------*/

            if(nullProductFlag == 5)
            {
           
                if(coverageDur >= minCoverageDur)
                {
    			   /*-----------------------------------------------------------------------------------------*/
    			   /* if coverageDur >= min_coverage_dur then set field to all 0.0  */
    			   /*-----------------------------------------------------------------------------------------*/
                    haveGoodRadarProduct = 1;
                    *radarAvailFlag = 2 ;
    			   
                    sprintf ( message , "null product flag = 5 and coverage > min coverage --  all 0.0 field substituted.");
                    printMessage(message, logFile);

                    for( i = 0; i < NUM_DPA_COLS; i++)
                    {
    				   for( j = 0; j < NUM_DPA_COLS; j++)
    					   radar[i][j] = 0.0 ;

                    }
                }
                
                else //coverageDur < minCoverageDur
                {
                    if(isPrevHourNullProdFlag5(radarId, datetime, daa_wind))
                    {
    				   /*-------------------------------------------------------------------------------*/
    				   /* read DAARadar record for previous hour                                        */
    				   /* if the product for the previous hour also has null_product_flag = 5,          */
    				   /*    then set field to all 0.0                                                  */
    				   /*-------------------------------------------------------------------------------*/
    				   haveGoodRadarProduct = 1;
    				   *radarAvailFlag = 2 ;
    				   sprintf ( message , "null product flag = 5 for current and previous products --  all 0.0 field substituted.");
    				   printMessage(message, logFile);

                        for( i = 0; i < NUM_DPA_COLS; i++)
                        {
                            for( j = 0; j < NUM_DPA_COLS; j++)
    						   radar[i][j] = 0.0 ;
                        } //end for i
    		    	}

                    else //not isPrevHourNullProdFlag5
                    {

                        haveGoodRadarProduct = 0;
                        *radarAvailFlag = 0 ;

                        sprintf ( message , "unacceptable null product with coverage < min coverage or unspecified --  all missing field substituted.");
                        printMessage(message, logFile);

                        for( i = 0; i < NUM_DPA_COLS; i++)
                        {
                            for( j = 0; j < NUM_DPA_COLS; j++)
    						   radar[i][j] = RADAR_DEFAULT ;
                        }

                    } //end else not isPrevHourNullProdFlag5
                } //end else coverageDur < minCoverageDur
            } //end if (nullProductFlag == 5)
            
      
            else //does not have a null product flag == 5, so consider it missing
            {

                haveGoodRadarProduct = 0;
                *radarAvailFlag = 0 ;
                sprintf ( message , "null product with coverage < min coverage or unspecified --  all missing field substituted.");
                printMessage(message, logFile);

    		    for( i = 0; i < NUM_DPA_COLS; i++)
    		    {
    			    for( j = 0; j < NUM_DPA_COLS; j++)
    				    radar[i][j] = RADAR_DEFAULT ;
    		    }
    	    } //end else consider it missing
        } //end else is a null product of some kind        

    } //end if (irc == 0) -> good read of DAA product
    
    else //not able to read DAA product
    {

    	if( isMissingHourCoveredByNullProduct(radarId, datetime, daa_wind))
    	{
    		haveGoodRadarProduct = 1;
    		*radarAvailFlag = 2 ;

    		for( i = 0; i < NUM_DPA_COLS; i++)
    		{
    			for( j = 0; j < NUM_DPA_COLS; j++)
    				radar[i][j] = 0.0 ;
    		} //end for i

    		sprintf ( message , "haveUsefulDAARadar():  missing hour covered by later hour null product -- all zero field used. ");
    		printMessage(message, logFile);

    	}

    	else
    	{
    		haveGoodRadarProduct = 0;
    	  	*radarAvailFlag = 0 ;

    	  	sprintf ( message , "haveUsefulDAARadar():  data for hour not available -- all missing field used. ");
    	  	printMessage(message, logFile);

    	}

    }
/*
    sprintf ( message , "leaving haveUsefulDAARadar(): haveGoodRadarProduct = %d  and *radarAvailFlag = %d",
    					haveGoodRadarProduct, *radarAvailFlag);
    printMessage(message, logFile);

*/
    return haveGoodRadarProduct;

} //end haveUsefulDAARadar()
   
//------------------------------------------------------------------------------------------
int haveUsefulDPARadar(const char * radarID, const char * datetime, const int dpa_wind,
                float radar [ ] [ NUM_DPA_COLS ] , int *    radarAvailFlag)
   
{
	char    fname[FNAME_LEN] = "" ;
	int     i, j ;
	int     itim;
	int len, ierr ;
	int haveGoodRadarProduct = 0;
	double  bias, xmax ;
	long int irc = 0 ;

	irc = 0;
	readDPARadar(radarID, datetime, dpa_wind, &xmax, &bias, fname, &itim, &irc);

	if(irc == 0) //should be a radar file available
	{
		sprintf ( message , "DPA product found for radar = %s\n", radarID) ;
		printMessage(message, logFile);

	}

	if((irc != 0) || (xmax == -99.0)) // dpa radar
	{

		sprintf ( message , " no radar data for current hour"
				" -- missing data substituted\n");
		printMessage(message, logFile);


		haveGoodRadarProduct = 0;

	}
	else //read DPA product
	{
		/*-----------------------------------------*/
		/* xmax > 0.0 -- read decoded DPA product  */
		/*-----------------------------------------*/

		if(xmax > 0.0)
		{

			len = strlen(fname) ;
			ierr = 0 ;

			sprintf ( message , "Maximum radar value = %7.2f mm", xmax ) ;
			printMessage(message, logFile);

			read_stage1_decoded_(fname, &len, radar, &ierr) ;

			if(ierr != 0)
			{
				haveGoodRadarProduct  = 0;

				sprintf ( message , "ERROR: #%d encountered reading radar file = %s"
						" -- missing data substituted.", ierr, fname ) ;
				printMessage(message, logFile);
				*radarAvailFlag = 0 ;

				for( i = 0; i < NUM_DPA_COLS; i++)
				{
					for( j = 0; j < NUM_DPA_COLS; j++)
						radar[i][j] = RADAR_DEFAULT ;
				}

			}
			else  //product is good, but just check for below minimum and correct to 0.0
			{
				haveGoodRadarProduct = 1;

				for( i = 0; i < NUM_DPA_COLS; i++)
				{
					for( j = 0; j < NUM_DPA_COLS; j++)
					{
						if(radar[i][j] < ACC_MIN)
							radar[i][j] = 0.0 ;
					}
				}


			}
		}
	   
	   else //zero product
	   {
		   haveGoodRadarProduct = 1;
		   *radarAvailFlag = 2 ;

		   sprintf ( message , "STATUS: radar data all zero for current hour.") ;
		   printMessage(message, logFile);

		   if(itim != 0)
		   {
			   sprintf ( message , "STATUS: no top-of-hour product found -- %d, "
					   "product used instead.", itim ) ;
			   printMessage(message, logFile);
		   }

		   for( i = 0; i < NUM_DPA_COLS; i++)
		   {
			   for( j = 0; j < NUM_DPA_COLS; j++)
				   radar[i][j] = 0.0 ;
		   }

	   }
     }
     
    return haveGoodRadarProduct;

}  //end haveUsefulDPARadar()

/*---------------------------------------------------------------------*/

int isPrevHourNullProdFlag5(const char * radarId,
                   const char * datetime,
                   const int daawind
                   )
{
/*
   this function searches the DAARadar table for a record for the previous hour

   if no such top-of-hour record is found, then a search is done on either side
     of the hour up to idaawind minutes to look for a record

   if no record is found within the window, then return 0 (false)

   if a record is found, then the value of the nullproductflag is checked
      if nullproductflag = 5, then return 1 (true)
      else return 0 (false)

   calling subroutine: readRDMosaicRadars
*/

    int found = 0;
    int result = 0;
    int minOff = 0;
    int nullProductFlag;
   
    char where [ 256 ];
    DAARadar * pDAARadarHead = NULL ;
    char queryObsTime[ANSI_TIME_LEN + 1];

    time_t currentHourObsTime = 0;
    time_t previousHourObsTime = 0;
   
    time_t beforeTOHObsTime = 0;
    time_t afterTOHObsTime = 0;


  //  memset (radarId, '\0',RADAR_ID_LEN + 1 );
  //  strncpy (radarId, radar, RADAR_ID_LEN );

   /*
   sprintf ( message , "radar id = %s  obstime = %s  daa_wind = %d.\n", rrad, str, idaawind);
   printMessage(message, logFile);
  */
   /*------------------------------------------------------------------------*/
   /*  subtract 1 hour from obstime                                          */
   /*------------------------------------------------------------------------*/

    yearsec_ansi_to_timet(datetime, &currentHourObsTime);
 
    previousHourObsTime = currentHourObsTime - SECONDS_PER_HOUR;

    timet_to_yearsec_ansi(previousHourObsTime, queryObsTime);
  
   /*------------------------------------------------------------------------*/
   /*  search for top-of-hour record from previous hour                      */
   /*------------------------------------------------------------------------*/
    sprintf ( where , "WHERE radid='%s' AND obstime='%s' ",
                     radarId, queryObsTime ) ;

    pDAARadarHead = GetDAARadar ( where );

    if ( pDAARadarHead != NULL )
    {
      /* A top-of-hour record from previous hour has been found. */
        nullProductFlag = pDAARadarHead->null_product_flag;
        found = 1;
        
         FreeDAARadar ( pDAARadarHead );
         pDAARadarHead = NULL;
    }
    
    else
    {
      /*  Search for non-top-of-hour record.
          If searching in window around 00z, then need to use date
           of previous day.  */

        for ( minOff = 1; minOff < daawind + 1; minOff ++)
        {
    	    /*  check after TOH   */
   
            afterTOHObsTime = previousHourObsTime + (minOff * SECONDS_PER_MINUTE);
	        timet_to_yearsec_ansi(afterTOHObsTime, queryObsTime);
   
            sprintf ( where , "WHERE radid='%s' AND obstime='%s' ",
                     radarId, queryObsTime ) ;

            pDAARadarHead = GetDAARadar ( where );

            if ( pDAARadarHead != NULL )
            {
                  /* A record has been found. */
                nullProductFlag = pDAARadarHead->null_product_flag;
                found = 1;
            
                FreeDAARadar ( pDAARadarHead );
                pDAARadarHead = NULL;
                break ;
            }

            /*  check before TOH  */
 
            beforeTOHObsTime = previousHourObsTime - (minOff * SECONDS_PER_MINUTE);
            timet_to_yearsec_ansi(beforeTOHObsTime, queryObsTime);
            
            sprintf ( where , "WHERE radid='%s' AND obstime='%s' ",
                     radarId, queryObsTime ) ;

            pDAARadarHead = GetDAARadar ( where );

            if ( pDAARadarHead != NULL )
            {
            /* A record has been found. */
                nullProductFlag = pDAARadarHead->null_product_flag;
                found = 1 ;
            
                FreeDAARadar ( pDAARadarHead );
                pDAARadarHead = NULL;
                break;
            }
      } //end for minOff
   }
/*-----------------------------------------------------------------*/
   
    result = 0;
    if ( ( found == 1 ) && (nullProductFlag == 5))
    {
         result =  1;   
    }
  

    if ( pDAARadarHead != NULL )
    {
        FreeDAARadar ( pDAARadarHead );
        pDAARadarHead = NULL;
    }
  
     return result;
    
} //end isPrevHourNullProdFlag5  
 
 
  /*---------------------------------------------------------------------*/
  
   
int isMissingHourCoveredByNullProduct(const char * radarId,
                   const char * datetimeString,
                   const int daawind)
{

	// This function determines if a null product for the next hour is available that indicates that there was no precip 
	// during the period for which the product in question was missing

    int covered = 0;
    int minOff = 0;
    int nullProductFlag  = 0;
    int coverageDur = 0;
    char where [ 256 ];
    DAARadar * pDAARadarHead = NULL ;
   
    char queryObsTime[ANSI_TIME_LEN + 1];
  
    time_t currentHourObsTime = 0;
    time_t nextHourObsTime = 0;
   
    time_t beforeTOHObsTime = 0;
    time_t afterTOHObsTime = 0;

  //  memset ( radarId,'\0',RADAR_ID_LEN + 1 );
  //  strncpy (radarId,rad, RADAR_ID_LEN );

   /*
   sprintf ( message , "radar id = %s  obstime = %s  daa_wind = %d.\n", rrad, str, idaawind);
   printMessage(message, logFile);
  */
   /*------------------------------------------------------------------------*/
   /*  add 1 hour to obstime                                          */
   /*------------------------------------------------------------------------*/

    yearsec_ansi_to_timet(datetimeString, &currentHourObsTime);

    nextHourObsTime = currentHourObsTime + SECONDS_PER_HOUR;

    timet_to_yearsec_ansi(nextHourObsTime, queryObsTime);
  /*------------------------------------------------------------------------*/
   /*  search for top-of-hour record from previous hour                      */
   /*------------------------------------------------------------------------*/
    sprintf ( where , "WHERE radid='%s' AND obstime='%s' ",
                     radarId, queryObsTime ) ;

    pDAARadarHead = GetDAARadar ( where );

    if ( pDAARadarHead != NULL )
    {
      /* A top-of-hour record from next hour has been found. */
  
         covered = isCovered(pDAARadarHead, currentHourObsTime);
         FreeDAARadar ( pDAARadarHead );
         pDAARadarHead = NULL;
    }
    else //there is no record exactly at the top of the hour
    {
  
  
        for ( minOff = 1; minOff < daawind + 1; minOff ++)
        {
      
        /*  check  after TOH  */
      
            afterTOHObsTime = nextHourObsTime + (minOff * SECONDS_PER_MINUTE);
	        timet_to_yearsec_ansi(afterTOHObsTime, queryObsTime);


            sprintf ( where , "WHERE radid='%s' AND obstime='%s' ",
                     radarId, queryObsTime ) ;


             sprintf ( message , " after TOH: WHERE clause = :%s:", where);

              printMessage(message, logFile);


            pDAARadarHead = GetDAARadar ( where );

            if ( pDAARadarHead != NULL )
            {
                covered = isCovered(pDAARadarHead, currentHourObsTime);
                FreeDAARadar ( pDAARadarHead );
                pDAARadarHead = NULL;
                break;
            }

         /*  check before TOH  */
        
            beforeTOHObsTime = nextHourObsTime - (minOff * SECONDS_PER_MINUTE);
	        timet_to_yearsec_ansi(beforeTOHObsTime, queryObsTime);

            sprintf ( where , "WHERE radid='%s' AND obstime='%s' ",
                     radarId, queryObsTime ) ;


            sprintf ( message , " before TOH: WHERE clause = :%s:", where);

            printMessage(message, logFile);

            pDAARadarHead = GetDAARadar ( where );

            if ( pDAARadarHead != NULL )
            {
                covered = isCovered(pDAARadarHead, currentHourObsTime);
                FreeDAARadar ( pDAARadarHead );
                pDAARadarHead = NULL;
                break;
            }
        } //end for minutes
    } //end else no record at exactly TOH
/*-----------------------------------------------------------------*/
   
    return covered;

} //end isMissingHourCoveredByNullProduct		   
   

int isCovered(DAARadar *pDAARadar, time_t originalObsTime)
{
    int covered = 0;

    if (pDAARadar != NULL)
    {
    
        int nullProductFlag = pDAARadar->null_product_flag;
        int coverageDurInMinutes = pDAARadar->coverage_dur;
       
     
        if ((nullProductFlag == 5) && (coverageDurInMinutes > 0))
        {
        
            int obstime = pDAARadar->obstime;
        
            int secondsSinceRain = coverageDurInMinutes * SECONDS_PER_MINUTE;
        
            time_t noRainSinceTime = obstime - secondsSinceRain;
   
            if (noRainSinceTime <= originalObsTime)
            {
                covered = 1; //True
            }
        }
     
    }
    
    return covered;
}
