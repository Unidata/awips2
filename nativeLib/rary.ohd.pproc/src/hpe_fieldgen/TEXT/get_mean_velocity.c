/***********************************************************************
 * Filename: get_mean_velocity.c
 *
 * Original Author: Guoxian Zhou
 *
 * File Creation Date: Feb. 06, 2008
 *
 * Development Group: OHD
 *
 * Description:
 *   This searches the Velocity table for the records
 *   during previous given time interval and get the mean value.
 * 
 * Modules:
 * getMeanVelocity
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "empe_fieldgen.h"

/***********************************************************************
 * Module Name: getMeanVelocity
 *
 * Original Author: Guoxian Zhou
 *
 * Module Creation Date: Feb. 06, 2008
 *
 * Description:
 *   This subroutine searches the Velocity table for the records
 *   during previous given time interval and get the mean value.
 * 
 * Calling Arguments:
 * Name          Input/Output Type          Description
 * mosaicid      Input        const char *  mosaic id
 * datetime      Input        const char *  run time string
 * search_window Input        const int     product search window value (seconds)
 * pMeanVelocity Output       Velocity *    mean velocity
 * status        Output       int *         status
 *
 * Required
 * None
 *
 * Required Files/Databases:
 * None
 *
 * calling function: qcvect
 *
 * Non System Routines Called:
 * GetVelocity, yearsec_dt_to_timet
 *
 * Return Value:
 * Type          Description
 * void
 *
 * Error Codes/Exceptions:
 * 
 *
 * OS Specific Assumptions:
 * None
 *
 * Local Variables:
 * Name     Type       Description
 *
 * Modification History:
 * Date        Developer     Action
 * 2/06/2008   Guoxian Zhou  First version
 *
 ***********************************************************************/

void getMeanVelocity(const char * mosaicid, const time_t currTimeT,
        const int search_window, Velocity * pMeanVelocity, int * status,
        int * count)
{

    char where [ 256 ] = { '\0' };

    char strCurrTime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    char strStartTime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };

    time_t startTimeT;

    Velocity * pVelocityHead = NULL;
    Velocity * pVelocityNode = NULL;

    double usum = 0.0;
    double vsum = 0.0;
    int denom = 0;
    
    int recSize = 0;

    *status = 0;

    timet_to_yearsec_ansi(currTimeT, strCurrTime);

    startTimeT = currTimeT - search_window;
    timet_to_yearsec_ansi(startTimeT, strStartTime);

    /*
     * Initialize the parameters.
     */

    strcpy(pMeanVelocity->mosaicid, "");
    pMeanVelocity->createtime = currTimeT;
    pMeanVelocity->count = 0;
    pMeanVelocity->umean = 0.0;
    pMeanVelocity->vmean = 0.0;

    timet_to_yearsec_ansi(startTimeT, strStartTime);

    /*
     * search for record(s) within the dhr_wind range
     */

    sprintf(where, "WHERE mosaicid = '%s' AND createtime >= '%s' "
        "AND createtime < '%s' ORDER BY createtime", mosaicid, strStartTime,
            strCurrTime) ;

    pVelocityHead = GetVelocity(where);

    if (pVelocityHead != NULL)
    {
        pVelocityNode = ( Velocity * ) ListFirst(&pVelocityHead->list) ;

        strcpy(pMeanVelocity->mosaicid, pVelocityNode->mosaicid);
    } else
    {
        /*
         * there is no record found.
         */

        *status = 100;
        return;
    }

    while (pVelocityNode != NULL)
    {

        usum += pVelocityNode->umean * pVelocityNode->count;
        vsum += pVelocityNode->vmean * pVelocityNode->count;
        denom += pVelocityNode->count;

        recSize ++;

        pVelocityNode = ( Velocity * ) ListNext( &pVelocityNode->node) ;
    }

    if (denom != 0)
    {
        pMeanVelocity->umean = usum / denom;
        pMeanVelocity->vmean = vsum / denom;
        pMeanVelocity->count = denom;
    }

    if (pVelocityHead != NULL)
    {
        FreeVelocity(pVelocityHead);
        pVelocityHead = NULL;
    }
    
    *count = recSize;

    return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/get_mean_velocity.c,v $";
 static char rcs_id2[] = "$Id: get_mean_velocity.c,v 1.5 2008/12/04 21:43:47 gzhou Exp $";}
/*  ===================================================  */

}
