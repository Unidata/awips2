
/**********************************************************************
 * readPrecipLimit ( )
 * 
 * This function loads the precipitation limit value from
 * the LocDataLimits table and the DataLimits table.
 *********************************************************************/

#include "dqc_preproc.h"
#include "mpe_log_utils.h"


int  check_range_date(time_t    data_timet,
                      char *    start_ansi_monthday,
                      char *    end_ansi_monthday);

double dqc_preprocessor_readPrecipLimit( const char * gageID ,
                      const time_t datetime,
                      int runHours)
{
    char where_clause [ 100 ] ;
    static LocDataLimits * pLocDataLimitsNode = NULL ;
    static LocDataLimits * pLocDataLimitsVal    = NULL ;

    static DataLimits * pDataLimitsNode = NULL ;
    static DataLimits * pDataLimitsVal    = NULL ;

    static int flag1 = 0 ;
    static int flag2 = 0 ;

    int mon, day, matchlimit;
    time_t orig_time_t, start_time_t, end_time_t ;
    struct tm * pDateStruct = NULL ;
    char message[500]={'\0'};    /* included for debug purposes */


    time(&orig_time_t);
    pDateStruct = gmtime(&orig_time_t);


    /*
     * Construct the where clause for the query to retrieve
     * the max data list.
     */
    sprintf ( where_clause , " WHERE pe = 'PP' AND dur IN "
                " (1006, 2001, 5004) "
                " ORDER BY monthdaystart ASC, dur ASC" );

    /*
     * Select value from LocDataLimits table first.
     */ 
    if(flag1 == 0 )
    {
        pLocDataLimitsVal = GetLocDataLimits ( where_clause ) ;
        flag1 = 1 ;
    }

    if ( pLocDataLimitsVal != NULL )
    {
        pLocDataLimitsNode = ( LocDataLimits * ) ListFirst(&pLocDataLimitsVal->list ) ; 
    }

    while ( pLocDataLimitsNode != NULL )
    {
        if ( !strcmp(pLocDataLimitsNode->lid, gageID) )
        {

	/* 09/07/2010 DTM revamped this section as the previous check against the time_t datetime
        did not work properly for dates on the first of the month.  Included the check_range_date
	routine taken from shefdecode source and this worked
        */


	    matchlimit = check_range_date(datetime,
	                                  pLocDataLimitsNode->monthdaystart,
					  pLocDataLimitsNode->monthdayend);

 	    if(matchlimit)
	    {



                if(IsNull(DOUBLE, &(pLocDataLimitsNode->gross_range_max)) == NOTNULL)
                {
                    if(pLocDataLimitsNode->dur == 1006 && runHours == 6)
                    {
                        return pLocDataLimitsNode->gross_range_max ;
                    }
                    else if(pLocDataLimitsNode->dur == 2001 && runHours == 24)
                    {
                        return pLocDataLimitsNode->gross_range_max ;
                    }
                    else if(pLocDataLimitsNode->dur == 5004 && runHours == 24)
                    {
                        return pLocDataLimitsNode->gross_range_max ;
                    }
                }
            }
        }
        pLocDataLimitsNode = (LocDataLimits *)ListNext( & pLocDataLimitsNode->node ) ;
    }

    /*
     * There is no complete valid data set from LocDataLimits
     * table. select value from DataLimits table.
     */ 
    if(flag2 == 0 )
    {
        pDataLimitsVal = GetDataLimits ( where_clause ) ;
        flag2 = 1 ;
    }

    if ( pDataLimitsVal != NULL )
    {
        pDataLimitsNode = ( DataLimits * ) ListFirst(&pDataLimitsVal->list ) ; 
    }

    while ( pDataLimitsNode != NULL )
    {

     /* 09/07/2010 DTM revamped this section as the previous check against the time_t datetime
        did not work properly for dates on the first of the month.  Included the check_range_date
	routine taken from shefdecode source and this worked.  Note that this seemed to work for
	precip but not for temperature.  Still code is cleaner using this routine.
     */

	matchlimit = check_range_date(datetime,pDataLimitsNode->monthdaystart,pDataLimitsNode->monthdayend);


 	if(matchlimit)
	{


            if(IsNull(DOUBLE, &(pDataLimitsNode->gross_range_max)) == NOTNULL)
            {
                if(pDataLimitsNode->dur == 1006 && runHours == 6)
                {
                    return pDataLimitsNode->gross_range_max ;
                }
                else if(pDataLimitsNode->dur == 2001 && runHours == 24)
                {
                    return pDataLimitsNode->gross_range_max ;
                }
                else if(pDataLimitsNode->dur == 5004 && runHours == 24)
                {
                    return pDataLimitsNode->gross_range_max ;
                }
            }
        }
        pDataLimitsNode = (DataLimits *)ListNext( & pDataLimitsNode->node ) ;
    }

    return PRECIP_MISSING ;
}


/**********************************************************************
 * readTemperatureLimit ( )
 * 
 * This function loads the temperature limit value from
 * the LocDataLimits table and the DataLimits table.
 *********************************************************************/

void readTemperatureLimit( const char * gageID ,
                           const time_t datetime,
                           double * maxValue,
                           double * minValue)
{
    char where_clause [ 100 ];
    static LocDataLimits * pLocDataLimitsNode = NULL;
    static LocDataLimits * pLocDataLimitsVal  = NULL;

    static DataLimits * pDataLimitsNode = NULL;
    static DataLimits * pDataLimitsVal    = NULL;

    static int flag1 = 0;
    static int flag2 = 0;

    int mon, day, matchlimit;
    time_t orig_time_t, start_time_t, end_time_t;
    struct tm * pDateStruct = NULL;
    char message[500]={'\0'};  /* included for possible debugging */

    
    pDateStruct = gmtime(&datetime);

    *maxValue = MISSING_MAX_TEMPERATURE;
    *minValue = MISSING_MIN_TEMPERATURE;

    /*
     * Construct the where clause for the query to retrieve
     * the max/min data list.
     */
    sprintf ( where_clause , " WHERE pe = 'TA' "
                " ORDER BY monthdaystart ASC, dur ASC" );

    /*
     * Select value from LocDataLimits table first.
     */ 
    if(flag1 == 0 )
    {
        pLocDataLimitsVal = GetLocDataLimits ( where_clause );
        flag1 = 1;
    }

    if ( pLocDataLimitsVal != NULL )
    {
        pLocDataLimitsNode = ( LocDataLimits * ) ListFirst(&pLocDataLimitsVal->list );
    }

    while ( pLocDataLimitsNode != NULL )
    {
        if ( !strcmp(pLocDataLimitsNode->lid, gageID) )
        {

     /* 09/07/2010 DTM revamped this section as the previous check against the time_t datetime
        did not work properly for dates on the first of the month.  Included the check_range_date
	routine taken from shefdecode source and this worked
     */


	    matchlimit = check_range_date(datetime,
	                                  pLocDataLimitsNode->monthdaystart,
					  pLocDataLimitsNode->monthdayend);

 	    if(matchlimit)
	    {



        	if ( (IsNull(DOUBLE, &(pLocDataLimitsNode->gross_range_max)) == NOTNULL) &&
                     (IsNull(DOUBLE, &(pLocDataLimitsNode->gross_range_min)) == NOTNULL) )
        	{
                    *maxValue = pLocDataLimitsNode->gross_range_max;
                    *minValue = pLocDataLimitsNode->gross_range_min;
                    return;
        	}

	     }
        }
        pLocDataLimitsNode = (LocDataLimits *)ListNext( & pLocDataLimitsNode->node );
    }

    /*
     * There is no valid data set from LocDataLimits table. 
     * select value from DataLimits table.
     */ 
    if(flag2 == 0 )
    {
        pDataLimitsVal = GetDataLimits ( where_clause );
        flag2 = 1;
    }

    if ( pDataLimitsVal != NULL )
    {
        pDataLimitsNode = ( DataLimits * ) ListFirst(&pDataLimitsVal->list );
    }

    while ( pDataLimitsNode != NULL )
    {
     /* 09/07/2010 DTM revamped this section as the previous check against the time_t datetime
        did not work properly for dates on the first of the month.  Included the check_range_date
	routine taken from shefdecode source and this worked
     */

	matchlimit = check_range_date(datetime,pDataLimitsNode->monthdaystart,pDataLimitsNode->monthdayend);

 	if(matchlimit)
	{



            if ( (IsNull(DOUBLE, &(pDataLimitsNode->gross_range_max)) == NOTNULL) &&
                 (IsNull(DOUBLE, &(pDataLimitsNode->gross_range_min)) == NOTNULL) )
            {
                *maxValue = pDataLimitsNode->gross_range_max;
                *minValue = pDataLimitsNode->gross_range_min;
                return ;
            }



        }
        pDataLimitsNode = (DataLimits *)ListNext( & pDataLimitsNode->node );
    }

    return;
}
