#include <string.h>

#include "DbmsDefs.h"
#include "RWRadarResult.h"
#include "empe_fieldgen.h"
#include "sqlca.h"
#include "time_convert.h"

/* This subroutine searches the RWRadarResult table for the ignore radar flag,
   edit bias flag and edited bias value

   calling subroutine: runERMosaic
*/

void readRadarResult (const char * datetime,
                      radar_result_struct * pRadarResult,
					  short * count ,
		      int   * dual_pol_flag,
                      long int * irc)
{
    char editb [ BOOL_LEN + 1 ] = {'\0'};
    char ignrad [ BOOL_LEN + 1 ] = {'\0'};
    char str [ ANSI_YEARSEC_TIME_LEN + 1 ] = {'\0'};
    char where [ 100 ] = {'\0'};
	int num = 0 ;

    RWRadarResult * pRWRadarResultHead = NULL ;
    RWRadarResult * pRWRadarResultNode = NULL ;

    strncpy ( str, datetime, ANSI_YEARSEC_TIME_LEN );

    /*
     *  Prepare the where clause. 
     */

    sprintf ( where, "WHERE obstime = '%s' order by radid", str );

    pRWRadarResultHead = GetRWRadarResult ( where );

    *irc = SQLCODE;

    if ( pRWRadarResultHead != NULL )
    {
        pRWRadarResultNode = ( RWRadarResult * )
                             ListFirst(&pRWRadarResultHead->list ) ; 
    }

    while ( pRWRadarResultNode != NULL )
    {
        strcpy ( ignrad, pRWRadarResultNode->ignore_radar );
        strcpy ( editb , pRWRadarResultNode->edit_bias );

        strcpy ( pRadarResult[num].radID , pRWRadarResultNode->radid );
        pRadarResult[num].bias = pRWRadarResultNode->rw_bias_val_used ;

        pRadarResult[num].edit_bias = 0;
	    dual_pol_flag[num] = 0;

        /*
         * Has the radar bias value been edited?
         */

        if ( strcmp ( editb, "y" ) == 0 )
        {
			pRadarResult[num].edit_bias = 1;
        }

        pRadarResult[num].ignore_radar = 0;

        /*
         * Is this radar being ignored?
         */

        if ( strcmp ( ignrad, "y" ) == 0 )
        {
			pRadarResult[num].ignore_radar = 1;
        }

		num ++ ;
	
        pRWRadarResultNode = ( RWRadarResult * )
                             ListNext ( & pRWRadarResultNode->node ) ;
    }

      /*
       * Free the memory associated with the retrieved RWRadarResult data.
       */

      pRWRadarResultNode = NULL;
	  FreeRWRadarResult ( pRWRadarResultHead) ;
      pRWRadarResultHead = NULL ;

	*count = num ;

    return ;
}
