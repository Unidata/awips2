#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include "dg.h"

void db_setsubgnav ( float lllat, float lllon, float urlat, float urlon, int *iret )
/************************************************************************
 * db_setsubgnav                                                        *
 *                                                                      *
 * This subroutine initializes internal sub grid navigation.            *
 * dgc_setsubgnav (lllat, lllon, urlat, urlon, irer )                   *
 * Input parameters:                                                    * 
 * 		lllat     float     Lower left latitude                 *
 *              lllon     float     Lower left Longitude               *
 *              urlat     float     Upper right latitude                *
 *              urlon     float     Upper right Longitude              *
 * Output parameters:                                                   *
 *             *iret      int        Return code                        *
 *                                        0 = normal return             *
 *                                      -46 = invalid grid point        *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo       12/04               Initial                             *
 ***********************************************************************/  
{
     int nc,ier;
     char gprj[5];
     float rltmin, rlnmin, rltmax, rlnmax;
     float  dlatll, dlonll, dlatur, dlonur;
/*----------------------------------------------------------------------*/
     *iret = 0;
     cst_itos ( (int *)&_dgsubg.refnav[1], 1, &nc, gprj, &ier );
     cst_rmbl ( gprj, gprj, &nc, &ier );
     /*
      * Define sub-grid area
      */
    rltmin = G_MIN ( lllat, urlat );
    rlnmin = G_MIN ( lllon, urlon );
    rltmax = G_MAX ( lllat, urlat );
    rlnmax = G_MAX ( lllon, urlon );
    /*
     * Take care of the sub-grid area across the date-line
     */
    if ( ( rlnmax - rlnmin ) > 180. ) {
        dlatll = rltmin;
        dlonll = rlnmax;
        dlatur = rltmax;
        dlonur = rlnmin;
    } else {
        dlatll = rltmin;
        dlonll = rlnmin;
        dlatur = rltmax;
        dlonur = rlnmax;
    }

    /*
     * *Set internal sub-grid navigation
     */
    gsmprj ( gprj, &_dgsubg.refnav[10], &_dgsubg.refnav[11], &_dgsubg.refnav[12],
        &dlatll, &dlonll, &dlatur, &dlonur, &ier, strlen(gprj) );
    /*
     * IF set sub-grid navigation fail, change center longitude
     */
    if ( ier != 0 ) {
        *iret = -46;
    }   
}
