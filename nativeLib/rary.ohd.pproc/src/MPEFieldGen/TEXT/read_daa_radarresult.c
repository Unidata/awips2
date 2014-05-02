#include <string.h>

#include "DbmsDefs.h"
#include "DAARadarResult.h"
#include "sqlca.h"
#include "time_convert.h"
#include "mpe_fieldgen.h"

/* This subroutine searches the DAARadarResult table for the ignore radar flag,
   edit bias flag and edited bias value

   calling subroutine: runRDMosaic
*/

void MPEFieldGen_readDAARadarResult (const char * datetime,
                      radar_result_struct * pRadarResult,
                      short * count ,
                      long int * irc)
{
    char editb [ BOOL_LEN + 1 ];
    char ignrad [ BOOL_LEN + 1 ];
    char str [ ANSI_YEARSEC_TIME_LEN + 1 ];
    char where [ 100 ];
    int num = 0 ;

    DAARadarResult * pDAARadarResultHead = NULL ;
    DAARadarResult * pDAARadarResultNode = NULL ;

    memset ( str, '\0', ANSI_YEARSEC_TIME_LEN + 1 );
    strncpy ( str, datetime, ANSI_YEARSEC_TIME_LEN );

    memset ( ignrad, '\0', BOOL_LEN + 1 );
    memset ( editb, '\0', BOOL_LEN + 1 );

    /* Prepare the where clause. */
    sprintf ( where, "WHERE obstime = '%s' order by radid", str );

    pDAARadarResultHead = GetDAARadarResult ( where );

    *irc = 0;

    if ( pDAARadarResultHead != NULL )
    {
        pDAARadarResultNode = ( DAARadarResult * ) ListFirst(&pDAARadarResultHead->list ) ; 
    }

    while ( pDAARadarResultNode != NULL )
    {
        strcpy ( ignrad, pDAARadarResultNode->ignore_radar );
        strcpy ( editb , pDAARadarResultNode->edit_bias );

        strcpy ( pRadarResult[num].radID , pDAARadarResultNode->radid );
        pRadarResult[num].bias = pDAARadarResultNode->rw_bias_val_used ;

        pRadarResult[num].edit_bias = 0;

        /* Has the radar bias value been edited? */
        if ( strcmp ( editb, "y" ) == 0 )
			pRadarResult[num].edit_bias = 1;

        pRadarResult[num].ignore_daa_radar = 0;

        /* Is this radar being ignored? */
        if ( strcmp ( ignrad, "y" ) == 0 )
			pRadarResult[num].ignore_daa_radar = 1;

		num ++ ;
	
        pDAARadarResultNode = ( DAARadarResult * ) ListNext ( & pDAARadarResultNode->node ) ;
    } /* while ( pDAARadarResultNode != NULL ) */

      /* Free the memory associated with the retrieved DAARadarResult
         data. */
      pDAARadarResultNode = NULL;
	  FreeDAARadarResult ( pDAARadarResultHead) ;
      pDAARadarResultHead = NULL ;

	*count = num ;

    return ;

} /* end MPEFieldGen_readDAARadarResult */
