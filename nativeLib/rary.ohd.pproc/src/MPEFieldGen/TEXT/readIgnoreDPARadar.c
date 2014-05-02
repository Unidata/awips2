#include <string.h>

#include "DbmsDefs.h"
#include "RWRadarResult.h"
#include "sqlca.h"
#include "time_convert.h"
#include "mpe_fieldgen.h"

/* This subroutine reads ignore_radar flag value from the RWRadarResult table 

   calling subroutine: runRDMosaic
*/

void readIgnoreDPARadar (const char * radid, const char * datetime,
                             int * ignoreFlag , long int * irc)
{
    char ignrad [ BOOL_LEN + 1 ];
    char str [ ANSI_YEARSEC_TIME_LEN + 1 ];
    char rad [ RADAR_ID_LEN + 1 ];
    char where [ 100 ];

    *ignoreFlag = 0;

    RWRadarResult * pRWRadarResultHead = NULL ;
    RWRadarResult * pRWRadarResultNode = NULL ;

    memset ( str, '\0', ANSI_YEARSEC_TIME_LEN + 1 );
    strncpy ( str, datetime, ANSI_YEARSEC_TIME_LEN );

    memset ( rad, '\0', RADAR_ID_LEN + 1 );
    strncpy ( rad, radid, RADAR_ID_LEN );

    memset ( ignrad, '\0', BOOL_LEN + 1 );

    sprintf ( where, "WHERE radid = '%s' AND obstime = '%s'", rad, str );

    pRWRadarResultHead = GetRWRadarResult ( where );

    *irc = SQLCODE;

    if ( pRWRadarResultHead != NULL )
    {
        pRWRadarResultNode = ( RWRadarResult * ) ListFirst(&pRWRadarResultHead->list ) ; 
    }

     strcpy ( ignrad, pRWRadarResultNode->ignore_radar );
     if ( strcmp ( ignrad, "y" ) == 0 ) *ignoreFlag = 1;

     pRWRadarResultNode = NULL;
     FreeRWRadarResult ( pRWRadarResultHead) ;
     pRWRadarResultHead = NULL ;

    return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/readIgnoreDPARadar.c,v $";
 static char rcs_id2[] = "$Id: readIgnoreDPARadar.c,v 1.1 2012/09/10 19:37:29 pst Exp $";}
/*  ===================================================  */

}
