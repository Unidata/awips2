#include <string.h>

#include "DbmsDefs.h"
#include "DAARadarResult.h"
#include "empe_fieldgen.h"
#include "sqlca.h"
#include "time_convert.h"

/* This subroutine searches the DAARadarResult table for the ignore radar flag,
   edit bias flag and edited bias value

   calling subroutine: runERMosaic
*/

void readDAARadarResult (const char * datetime,
                         radar_result_struct * pDAARadarResult,
			 short * count ,
			 int   * dual_pol_flag,
                         long int * irc)
{
    char editb [ BOOL_LEN + 1 ] = {'\0'};
    char ignrad [ BOOL_LEN + 1 ] = {'\0'};
    char str [ ANSI_YEARSEC_TIME_LEN + 1 ] = {'\0'};
    char where [ 200 ] = {'\0'};
    int num = 0 ;

    DAARadarResult * pDAARadarResultHead = NULL ;
    DAARadarResult * pDAARadarResultNode = NULL ;

    strncpy ( str, datetime, ANSI_YEARSEC_TIME_LEN );

    /*
     *  Prepare the where clause. 
     */

    sprintf ( where, "WHERE obstime = '%s' order by radid", str );

    pDAARadarResultHead = GetDAARadarResult ( where );

    *irc = SQLCODE;

    if ( pDAARadarResultHead != NULL )
    {
        pDAARadarResultNode = ( DAARadarResult * )
                             ListFirst(&pDAARadarResultHead->list ) ; 
    }

    while ( pDAARadarResultNode != NULL )
    {
        strcpy ( ignrad, pDAARadarResultNode->ignore_radar );
        strcpy ( editb , pDAARadarResultNode->edit_bias );

        strcpy ( pDAARadarResult[num].radID , pDAARadarResultNode->radid );
        pDAARadarResult[num].bias = pDAARadarResultNode->rw_bias_val_used ;

        pDAARadarResult[num].edit_bias = 0;
	
	dual_pol_flag[num] = 1;

        /*
         * Has the radar bias value been edited?
         */

        if ( strcmp ( editb, "y" ) == 0 )
        {
	     pDAARadarResult[num].edit_bias = 1;
        }

        pDAARadarResult[num].ignore_radar = 0;

        /*
         * Is this radar being ignored?
         */

        if ( strcmp ( ignrad, "y" ) == 0 )
        {
	     pDAARadarResult[num].ignore_radar = 1;
        }

	num ++ ;
	
        pDAARadarResultNode = ( DAARadarResult * )
                             ListNext ( & pDAARadarResultNode->node ) ;
    }

      /*
       * Free the memory associated with the retrieved DAARadarResult data.
       */

      pDAARadarResultNode = NULL;
	  FreeDAARadarResult ( pDAARadarResultHead) ;
      pDAARadarResultHead = NULL ;

	*count = num ;

    return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc/src/hpe_fieldgen/RCS/read_daaradarresult.c,v $";
 static char rcs_id2[] = "$Id: read_daaradarresult.c,v 1.1 2012/09/12 18:03:07 deng Exp $";}
/*  ===================================================  */

}
