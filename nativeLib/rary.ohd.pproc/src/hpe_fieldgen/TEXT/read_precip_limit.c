/*******************************************************************************
* FILENAME:             read_precip_limit.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*   MODULE 1:           readPrecipLimit
* DESCRIPTION:          This routine retrieves the maximum hourly precip
*                       data from the LocDataLimits table
*                       or the DataLimits table.
*
* ORIGINAL AUTHOR:      Guoxian ZHou
* CREATION DATE:        October 18, 2005
* ORGANIZATION:         OHD-11 HSEB
* MACHINE:              Redhat Linux
*
* input variables
*
* gageID - gage id
*
* datetime - date value in data type time_t
*
* output variables
*
* return value
*
*    maximum hourly precip value.
*
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        10/18/2005   Guoxian Zhou      Original Coding
********************************************************************************
*/

#include "DataLimits.h"
#include "DbmsUtils.h"
#include "LocDataLimits.h"
#include "empe_fieldgen.h"

double readPrecipLimit( const char * gageID ,
                        const time_t datetime)
{
    char where_clause [ 100 ] = {'\0'} ;
    static LocDataLimits * pLocDataLimitsNode = NULL ;
    static LocDataLimits * pLocDataLimitsVal  = NULL ;

    static DataLimits * pDataLimitsNode = NULL ;
    static DataLimits * pDataLimitsVal  = NULL ;

    static int flag1 = 0 ;
    static int flag2 = 0 ;

    int mon, day;
    time_t orig_time_t, start_time_t, end_time_t ;
    struct tm * pDateStruct = NULL ;

    time(&orig_time_t);
    pDateStruct = gmtime(&orig_time_t);

    /*
     * Construct the where clause for the query to retrieve
     * the max precip data list.
     */

    sprintf ( where_clause , " where pe = 'PP' and dur = 1001 "
	          "order by monthdaystart" );

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
        pLocDataLimitsNode = 
            ( LocDataLimits * ) ListFirst(&pLocDataLimitsVal->list ) ; 
    }

    while ( pLocDataLimitsNode != NULL )
    {
        if ( !strcmp(pLocDataLimitsNode->lid, gageID) )
        {
            sscanf(pLocDataLimitsNode->monthdaystart, "%d-%d", &mon, &day);
            pDateStruct->tm_mon = mon - 1;
            pDateStruct->tm_mday = day;
            start_time_t = mktime(pDateStruct);

            sscanf(pLocDataLimitsNode->monthdayend, "%d-%d", &mon, &day);
            pDateStruct->tm_mon = mon - 1;
            pDateStruct->tm_mday = day;
            end_time_t = mktime(pDateStruct);

            if ( (datetime >= start_time_t) &&
                 (datetime <= end_time_t) )
            {
                if(IsNull(DOUBLE, (void *)&(pLocDataLimitsNode->gross_range_max))
                    == NOTNULL)
                {
                    return pLocDataLimitsNode->gross_range_max ;
                }
            }
        }

        pLocDataLimitsNode 
            = (LocDataLimits *)ListNext( & pLocDataLimitsNode->node ) ;
    }

    /*
     * There is no valid data from LocDataLimits
     * table. select value from DataLimits table.
     */ 

    if(flag2 == 0 )
    {
        pDataLimitsVal = GetDataLimits ( where_clause ) ;
        flag2 = 1 ;
    }

    if ( pDataLimitsVal != NULL )
    {
        pDataLimitsNode = 
            ( DataLimits * ) ListFirst(&pDataLimitsVal->list ) ; 
    }

    while ( pDataLimitsNode != NULL )
    {
        sscanf(pDataLimitsNode->monthdaystart, "%d-%d", &mon, &day);
        pDateStruct->tm_mon = mon - 1;
        pDateStruct->tm_mday = day;
        start_time_t = mktime(pDateStruct);

        sscanf(pDataLimitsNode->monthdayend, "%d-%d", &mon, &day);
        pDateStruct->tm_mon = mon - 1;
        pDateStruct->tm_mday = day;
        end_time_t = mktime(pDateStruct);

        if ( (datetime >= start_time_t) &&
             (datetime <= end_time_t) )
        {
            if(IsNull(DOUBLE, (void *)&(pDataLimitsNode->gross_range_max))
                == NOTNULL)
            {
                return pDataLimitsNode->gross_range_max ;
            }
        }

        pDataLimitsNode
            = (DataLimits *)ListNext( & pDataLimitsNode->node ) ;
    }

    /*
     * There is no valid data from LocDataLimits and DataLimits
     * tables. return a default max value.
     */ 

    return RANGE_CHECK_DEFAULT ;
}
