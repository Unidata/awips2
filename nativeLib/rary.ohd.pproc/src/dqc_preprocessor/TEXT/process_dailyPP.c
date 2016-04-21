
/**********************************************************************
 * getDailyPP()
 *
 * This function loads the PPD data from DailyPP table.
 *
 * Modified Dave Miller Nov 2009.  See reason in compare_lid_ts_ext.c
 *********************************************************************/

#include "dqc_preproc.h"

extern struct precip_info * pPrecipInfo ;
extern int precip_count ;

DailyPP * getDailyPPRecord ( const time_t start_time_t, const int numDays )
{
   /* static DailyPP * pDailyPPHead = NULL ;*/

    DailyPP *pDailyPPHead = NULL;

    char dailyPP_query[200] = {'\0'};
    char start_date[11] = {'\0'};
    char   end_date[11] = {'\0'};

    time_t run_time_t = start_time_t ;

    timet_to_yearday_ansi(run_time_t, start_date);

    run_time_t += numDays * 24 * SECONDS_PER_HOUR ;
    timet_to_yearday_ansi(run_time_t, end_date);

    sprintf(dailyPP_query, "WHERE obstime > '%s' "
                        " AND obstime <= '%s'"
                        " ORDER BY lid, obstime",
                        start_date, end_date);

    if(pDailyPPHead == NULL)
    {
        pDailyPPHead = GetDailyPP ( dailyPP_query );
    }

    return pDailyPPHead;
}

/*************************************************************************
 * insertDailyPP()
 *
 * This function fills into the precip array with loaded the PPD data
 * from DailyPP table.
 ***************************************************************************/

void processDailyPP (const time_t start_time_t, const int numDays )
{
    DailyPP * pDailyPPHead = NULL;
    DailyPP * pDailyPPNode = NULL;

    int i, index;
    char strCompare[10] = {'\0'};

    struct precip_info * pStationInfo = NULL ;

    struct tm * tm_time = NULL;

/*    char strDate[numDays][11];*/

    char **strDate;


    char strCurrDate[21] = {'\0'};

    time_t curr_time_t;

    /* initialize */

    strDate = (char **)malloc(sizeof(char *) * numDays);
    if (strDate == NULL)
       printf("\n Fail in malloc strDate.\n");

    for (i = 0; i < numDays; i++)
    {
        strDate[i] = (char *)malloc(12);
	if (strDate[i] == NULL)
	   printf("\nFail in malloc strDate.\n");

    }

    /*
     * Compute the start time.
     */
    curr_time_t = start_time_t ;

    /*
     * Compute the date string array.
     */
    for(i = 0; i < numDays; i++)
    {
        curr_time_t += 24 * SECONDS_PER_HOUR ;
        tm_time = gmtime(&curr_time_t);
    /*    strftime(strDate[i], sizeof(strDate[i]), "%Y-%m-%d", tm_time);*/

        sprintf(strDate[i], "%04d-%02d-%02d", tm_time->tm_year+1900,
	                                      tm_time->tm_mon+1, tm_time->tm_mday);

    }

    pDailyPPHead = getDailyPPRecord (start_time_t, numDays ) ;

    if(pDailyPPHead != NULL)
    {
        pDailyPPNode = ( DailyPP * ) ListFirst ( & pDailyPPHead->list );
    }

    while ( pDailyPPNode != NULL )
    {
        index = -1 ;

        yearsec_dt_to_ansi(pDailyPPNode->obstime, strCurrDate);

        for(i = 0; i < numDays; i++)
        {
            if(strncmp(strCurrDate, strDate[i], 10) == 0 )
            {
                index = i ;
                break;
            }
        }

        /*
         * Mismatch pDailyPPNode->obstime value, skip it.
         */
        if(index < 0 || index >= numDays)
        {
            pDailyPPNode = ( DailyPP * ) ListNext ( & pDailyPPNode->node );
            continue;
        }

        /*
         * get the info for the matching identifier,
         * if there is a match.
         */

	strcpy(strCompare,pDailyPPNode->lid);

        /* Must normalize lengths of strings in the linked list before comparing.
	   This is also done in the station lid and source search value within compare_lid_source.
        */

	while (strlen(strCompare)<8)
	   strcat(strCompare,"0");

	sprintf(strCompare, "%s%c",strCompare,pDailyPPNode->ts[1]);

        pStationInfo = (struct precip_info *) binary_search ( pPrecipInfo, strCompare,
                        precip_count, sizeof ( struct precip_info), compare_lid_source );

        if ( pStationInfo != NULL )
        {
            if( (IsNull(DOUBLE, (void *)&(pDailyPPNode->value)) == NOTNULL)
                && (pDailyPPNode->value != PRECIP_MISSING)
                && (pDailyPPNode->value >= 0.0))
            {

                pStationInfo->pPPD[index] = pDailyPPNode->value ;
                pStationInfo->source = pDailyPPNode->ts[1] ;
            }
        }

        pDailyPPNode = ( DailyPP * ) ListNext ( & pDailyPPNode->node );
    }

    if(pDailyPPNode != NULL)
    {
        FreeDailyPP(pDailyPPNode);
        pDailyPPNode = NULL ;
    }
}
