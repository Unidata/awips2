#include "dg.h"

void dgc_ntim ( const int *chngnv, const int *coladd, char *time1,
                char *time2, int *nxtok, int *iret )
/************************************************************************
 * dgc_ntim                                                             *
 *                                                                      *
 * This subroutine initialize processing for the next time.             *
 *                                                                      *
 * dgc_ntim ( chngnv, coladd, time1, time2, nxtok, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *chngnv		const int *	Flag to change navigation       *
 *	*coladd		const int *	Flag to add a column of data	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*time1		char		Next time(1) from dtmlst	*
 *	*time2		char		Next time(2) from dtmlst	*
 *	*nxtok		int		Flag for existance of next time	*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *					-50 = navigation not same	*
 *					-57 = grid file open failed	*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/03						*
 * K. Brill/HPC		 2/04	CALL DG_SNAV and set DGAREA block	*
 * R. Tian/SAIC		 2/04	Modified to use new GD file management	*
 * R. Tian/SAIC		12/04	Modified to call DG_INXT		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get the next time.
     */
    if ( _nfile.itmlst > _nfile.ntmlst - 1 ) {
	time1[0] = '\0';
	time2[0] = '\0';
	*nxtok = G_FALSE;
	return;
    } else {
	strcpy ( time1, _nfile.dtmlst1[_nfile.itmlst] );
	strcpy ( time2, _nfile.dtmlst2[_nfile.itmlst] );
	*nxtok = G_TRUE;
    }
    _nfile.itmlst++;

    dgc_inxt ( chngnv, coladd, time1, time2, iret );

    return;
}
