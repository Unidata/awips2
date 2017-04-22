
/*************************************************************************
 * processHourlyPPPC()
 * 
 * This function fills into the precip array with the observed/calculated
 * PPQ data from HourlyPP/HourlyPC tables.
 *
 * Modified Dave Miller Nov 2009.  See reason in compare_lid_ts_ext.c
 ***************************************************************************/

#include "dqc_preproc.h"

void processHourlyPPPC (const time_t start_time_t, const int numDays ) 
{
    const static char * ts = "!PM" ;

    HourlyPC * pHourlyPC = NULL ;
    HourlyPP * pHourlyPP = NULL ;
    static HourlyPC * pOrigHourlyPC = NULL ;
    static HourlyPP * pOrigHourlyPP = NULL ;

    int pc_rec_cnt ;
    int pp_rec_cnt ;
    short int advance = 1 ;
    short int duration[2] ; /* The duration in hours. */
    short int num_periods[2];
    time_t start_time ;
    time_t end_time ;
    struct total_precip total_precip ;
    int i, index, j;

    double min_percent = 0.0 ;
    double maxPrecip = 0.0 ;
    struct precip_info * pStationInfo = NULL ;

    char strDate[numDays][11];
    char strCurrDate[11] = {'\0'};

    unsigned char precip_settings = PRECIP_TS_SINGLE | PRECIP_PP;

    static const char * mpe_load_hourlyPC_token = "mpe_load_hourlypc";
    char mpe_load_hourlyPC[10] = {'\0'} ;
    int length ;
    char strCompare[10] = {'\0'};
    static int blnLoadHourlyPC = 0 ;
    static int first = 0; 
   
    /*
     * Compute the date string array.
     */
    start_time = start_time_t ;

    for(i = 0; i < numDays; i++)
    {
        start_time += 24 * SECONDS_PER_HOUR ;
        timet_to_yearday_ansi(start_time, strDate[i]);
    }

    if(first == 0)
    {
        /*
         * Read token if need load hourlyPC data.
         */
        dqc_preprocessor_getAppsDefaults ( mpe_load_hourlyPC_token, mpe_load_hourlyPC ); 

        length = strlen ( mpe_load_hourlyPC );

        if ( length == 0 )
        {
            printf ( "Token \"%s\" is not defined. Default to \"ON\"\n", 
                      mpe_load_hourlyPC_token );
            blnLoadHourlyPC = 1 ;
        }
        else if( (strcmp(mpe_load_hourlyPC, "ON") == 0) ||
                 (strcmp(mpe_load_hourlyPC, "on") == 0) )
        {
            printf ( "Token \"%s\" is set to \"ON\"\n", 
                      mpe_load_hourlyPC_token );
            blnLoadHourlyPC = 1 ;
        }
        else
        {
            printf ( "Token \"%s\" is set to \"OFF\"\n", 
                      mpe_load_hourlyPC_token );
        }
        first = 1;
    }

    /*
     * Compute the beginning/ending data retrieval time.
     */
     
    start_time = start_time_t ;
    end_time = start_time_t + numDays * 24 * SECONDS_PER_HOUR;

    /*
     * Load the PC and PP hourly data 
     */
    if ( pOrigHourlyPC == NULL )
    {
        pOrigHourlyPC = (HourlyPC *) load_PC_hourly ( start_time, end_time,
                         NULL, &ts, 1, & pc_rec_cnt ) ;
    }
    if ( pOrigHourlyPP == NULL )
    {
        pOrigHourlyPP = (HourlyPP *) load_PP_hourly ( start_time, end_time,
                         NULL, &ts, 1, & pp_rec_cnt ) ;
    }

    /*
     * Load the observed PPQ data.
     */
    if ( pOrigHourlyPP != NULL )
    {
        pHourlyPP = ( HourlyPP * ) ListFirst(&pOrigHourlyPP->list ) ; 
    }

    /*
     * Fill in the observed PPQ data.
     */
    while ( pHourlyPP != NULL )
    {
        index = -1 ;
        date_t_to_ansi_date ( pHourlyPP->obsdate, strCurrDate );

        for(i = 0; i < numDays; i++)
        {
            if(strncmp(strCurrDate, strDate[i], 10) == 0 )
            {
                index = i ;
                break;
            }
        }

        /* 
         * Mismatch pHourlyPP->obsdate value, skip it.
         * Such case should not occur.
         */
        if(index == -1)
        {
            pHourlyPP = ( HourlyPP * ) ListNext ( & pHourlyPP->node ) ;
            continue;
        }

        /* 
         * get the info for the matching identifier,
         * if there is a match.
         */
        if ( precip_count > 0 )
        {

	    sprintf(strCompare, "%s",pHourlyPP->lid);

        /* Must normalize lengths of strings in the linked list before comparing.
	   This is also done in the station lid and source search value within compare_lid_source.
        */

	    while (strlen(strCompare)<8)
	       strcat(strCompare,"0");

            sprintf(strCompare, "%s%c",strCompare,pHourlyPP->ts[1]);
            pStationInfo = (struct precip_info *) binary_search ( pPrecipInfo, strCompare,
                            precip_count, sizeof ( struct precip_info), compare_lid_source );
        }

        if ( pStationInfo != NULL )
        {
            if( (IsNull(SHORT, (void *)&(pHourlyPP->sixhr18)) == NOTNULL)
                && (pHourlyPP->sixhr18 != PRECIP_MISSING) 
                && (pHourlyPP->sixhr18 >= 0.0) )
            {
                if( ((index+1) < numDays)
                    && (pStationInfo->pPPQ[index + 1][0] == PRECIP_MISSING) )
                {
                    pStationInfo->pPPQ[index + 1][0] = 
                               ( ( double) pHourlyPP->sixhr18 ) / 100.0 ;
    
                    strcpy(pStationInfo->pPPQPE[index + 1], "PP");
                    if(pStationInfo->source == 'Z')
                        pStationInfo->source = pHourlyPP->ts[1];
                }
            }

            if( (IsNull(SHORT, (void *)&(pHourlyPP->sixhr24)) == NOTNULL)
                && (pHourlyPP->sixhr24 != PRECIP_MISSING) 
                && (pHourlyPP->sixhr24 >= 0.0) )
            {
                if( ((index+1) < numDays)
                    && (pStationInfo->pPPQ[index + 1][1] == PRECIP_MISSING) )
                {
                    pStationInfo->pPPQ[index + 1][1] = 
                                      ( ( double ) pHourlyPP->sixhr24 ) /
                                      100.0 ;
    
                    strcpy(pStationInfo->pPPQPE[index + 1], "PP");
                    if(pStationInfo->source == 'Z')
                        pStationInfo->source = pHourlyPP->ts[1];
                }
            }

            if( (IsNull(SHORT, (void *)&(pHourlyPP->sixhr06)) == NOTNULL)
                && (pHourlyPP->sixhr06 != PRECIP_MISSING) 
                && (pHourlyPP->sixhr06 >= 0.0)
                && (pStationInfo->pPPQ[index][2] == PRECIP_MISSING) )
            {
                pStationInfo->pPPQ[index][2] = 
                                    ( ( double ) pHourlyPP->sixhr06 ) / 
                                    100.0 ;

                strcpy(pStationInfo->pPPQPE[index], "PP");
                if(pStationInfo->source == 'Z')
                    pStationInfo->source = pHourlyPP->ts[1];
            }

            if( (IsNull(SHORT, (void *)&(pHourlyPP->sixhr12)) == NOTNULL)
                && (pHourlyPP->sixhr12 != PRECIP_MISSING) 
                && (pHourlyPP->sixhr12 >= 0.0)
                && (pStationInfo->pPPQ[index][3] == PRECIP_MISSING) )
            {
                pStationInfo->pPPQ[index][3] = 
                                 ( ( double ) pHourlyPP->sixhr12 ) /
                                 100.0 ;

                strcpy(pStationInfo->pPPQPE[index], "PP");
                if(pStationInfo->source == 'Z')
                    pStationInfo->source = pHourlyPP->ts[1];
            }

        }

        pHourlyPP = ( HourlyPP * ) ListNext ( & pHourlyPP->node ) ;
    }

     /*
     *  Compute the total hourly precipitation for PPQ and PPD.
     *  Use PC in the computation of 6 hour totals and 24 hour totals. 
     *  Do not use PP.
     *  Only do this if the mpe_load_hourlypc token is set to 'ON'. 
     */
    /* initialize duration array */
    
    duration[0] = 6;
    duration[1] = 24; 
    num_periods[0] = 4;
    num_periods[1] = 1;
    
    if ( blnLoadHourlyPC == 1 )
    {
       for (j = 0; j < 2; j++)
       {                 
	  start_time = start_time_t ;

	  for(index = 0; index < numDays; index ++)
	  {
             for(i = 0; i < num_periods[j]; i ++)
             {
        	/*
        	 * Compute the ending data retrieval time.
        	 */
        	end_time = start_time + duration[j] * SECONDS_PER_HOUR ;

        	pHourlyPC = pOrigHourlyPC ;

        	while ( pHourlyPC != (HourlyPC *) NULL )
        	{
                    /*
                     * Initialize the total_precip structure.
                     */
                    memset(total_precip.lid, '\0' , LOC_ID_LEN + 1 ) ;
                    memset(total_precip.PE , '\0' , SHEF_PE_LEN + 1 ) ;
                    memset(total_precip.TS , '\0' , SHEF_TS_LEN + 1 ) ;

                    /*
                     * Get the total precip for the station.
                     */
                    /*
                     * Calculated by HourlyPP and HourlyPC,
                     * HourlyPP will be used if available,
                     * otherwise use hourlyPC.
                     */
                    total_precip = 
                	  get_total_hourly_precip ( & pHourlyPC ,
                                                    NULL ,
                                                    end_time ,
                                                    duration[j] ,
                                                    min_percent ,
                                                    precip_settings ,
                                                    advance ,
                                                    & pc_rec_cnt ,
                                                    & pp_rec_cnt ) ; 

                    /*
                     * For each station with a precipitation total greater than
                     * or equal to 0.0, process the gage's value.
                     */
                    if ( ( total_precip.value >= 0.0 ) &&
                	 ( total_precip.value != PRECIP_MISSING ) )
                    {
                	/*
                	 * Apply a range check to the totals derived
                	 * from PC data. Load the max hourly precip data
                	 * for range check.
                	 */
                	maxPrecip = 
                            dqc_preprocessor_readPrecipLimit(total_precip.lid, end_time, duration[j]);

                	if(maxPrecip != PRECIP_MISSING)
                	{
                            if(total_precip.value > maxPrecip)
                        	continue ;
                	}
    
                	/*
                	 * get the info for the matching identifier,
                	 * if there is a match.
                	 */

                	if ( precip_count > 0 )
               	        {


	                    sprintf(strCompare, "%s",total_precip.lid);

        /* Must normalize lengths of strings in the linked list before comparing.
	   This is also done in the station lid and source search value within compare_lid_source.
        */

	                    while (strlen(strCompare)<8)
	                       strcat(strCompare,"0");
                            sprintf(strCompare, "%s%c",strCompare,total_precip.TS[1]);

                            pStationInfo = (struct precip_info *) 
                                           binary_search ( pPrecipInfo, 
                                           strCompare,
                                           precip_count, 
                                           sizeof ( struct precip_info), 
                                           compare_lid_source );
                	}
     
                	if ( pStationInfo != NULL )
                	{
                            if (j == 0)
			    {
			       if ( pStationInfo->pPPQ[index][i] == PRECIP_MISSING)
                               {
                        	   pStationInfo->pPPQ[index][i] = total_precip.value ;

                        	   strcpy(pStationInfo->pPPQPE[index], 
                                	  total_precip.PE);

                        	   if(pStationInfo->source == 'Z')
                        	   {
                                       pStationInfo->source = total_precip.TS[1] ;
                        	   }
                               }
			    }
			    else
			    {
			       if (pStationInfo->pPPD[index] == PRECIP_MISSING) 
				{
				    pStationInfo->pPPD[index] = total_precip.value;
				    pStationInfo->source = total_precip.TS[1];   
				    
				}
			    
			    }
                	}

                    }
                }

                start_time += duration[j] * SECONDS_PER_HOUR ;
             }
          }
       }
    }
    if(pOrigHourlyPC != NULL)
    {
        FreeHourlyPC(pOrigHourlyPC);
        pOrigHourlyPC = NULL ;
    }

    if(pOrigHourlyPP != NULL)
    {
        FreeHourlyPP(pOrigHourlyPP);
        pOrigHourlyPP = NULL ;
    }
}
