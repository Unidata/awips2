#include "dg.h"

void dg_fall ( int *iret )
/************************************************************************
 * dg_fall								*
 *									*
 * This subroutine frees the actual memory for all internal grids, and	*
 * reset the grid information.						*
 *									*
 * dg_fall ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * R. Tian/SAIC		 3/06						*
 * T. Piper/SAIC	03/08	Replaced cmm_free1d with G_FREE		*
 ***********************************************************************/
{
    int ii;
/*---------------------------------------------------------------------*/
    *iret = 0;

    for ( ii = 0; ii < _dggrid.maxdgg; ii++ ) {
/*
 * Reset the grid information.
 */
	_dggrid.iusesv[ii] = 0;
	_dggrid.dttimd1[ii][0] = '\0';
	_dggrid.dttimd2[ii][0] = '\0';
	_dggrid.leveld1[ii] = 0;
	_dggrid.leveld2[ii] = 0;
	_dggrid.ivcrdd[ii] = 0;;
	_dggrid.gparmd[ii][0] = '\0';
	_dggrid.savflg[ii] = G_FALSE;

/*
 * If grid exists, free its memory.
 */
        if ( _dggrid.dgg[ii].grid ) {
#ifdef MEM_DEBUG
	    printf ( "Freed grid at %p with size: %d\n",
		(void *)(_dggrid.dgg[ii].grid), _dggrid.dgg[ii].size );
#endif
	    G_FREE ( _dggrid.dgg[ii].grid, float );
	    _dggrid.dgg[ii].size = 0;
        }
    }

    return;
}
