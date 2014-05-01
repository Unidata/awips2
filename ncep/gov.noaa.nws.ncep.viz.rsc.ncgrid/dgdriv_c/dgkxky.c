#include "dg.h"

void dg_kxky ( int *kx, int *ky, int *iret )
/************************************************************************
 * dg_kxky								*
 *									*
 * This subroutine returns the grid dimensions used for grid diagnostic	*
 * calculation. Either DGC_SUBG or DGC_NTIM must have been called first	*
 * for a normal return.							*
 *									*
 * dg_kxky ( kx, ky, iret )						*
 *									*
 * Output parameters:							*
 *      *kx		int		Number of grid pts in x		*
 *      *ky		int		Number of grid pts in y		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -2 = Grid size is invalid	*
 **									*
 * Log:									*
 * R. Tian/SAIC		 9/06						*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret  = 0;

    if ( _dgfile.dgset == G_FALSE ) {
	*iret = -2;
	return;
    }
	  
    *kx = _dgfile.kxd;
    *ky = _dgfile.kyd;

    return;
}
