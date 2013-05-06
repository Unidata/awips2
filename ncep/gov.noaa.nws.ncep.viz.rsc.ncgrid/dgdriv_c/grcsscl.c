#include "geminc.h"
#include "gemprm.h"

void grc_sscl ( int *iscale, const int *kx, const int *ky, const int *imin,
                const int *jmin, const int *imax, const int *jmax,
                float *grid, float *rmin, float *rmax, int *iret )
/************************************************************************
 * GR_SSCL								*
 *									*
 * This subroutine scales a scalar grid.  If the scaling factor    	*
 * is missing, undefined or greater than 20 in absolute value, 		*
 * an appropriate scaling factor will be computed.  If the data  	*
 * are too small to be scaled with ISCALE = 20, ISCALE is set 		*
 * to IMISSD and IRET = -14.  The maximun and minimun values of the	*
 * grid are also returned.						*
 *									*
 * GR_SSCL  ( ISCALE, KX, KY, IMIN, JMIN, IMAX, JMAX, GRID,		*
 *           RMIN, RMAX, IRET )						*
 *									*
 * Input and output parameters:						*
 *	ISCALE		INTEGER		Input/output scale factor	*
 * Input parameters:							*
 *	KX		INTEGER		Number of grid points in x dir	*
 *	KY		INTEGER		Number of grid points in y dir	*
 *	IMIN		INTEGER		Minimum x grid point		*
 *	JMIN		INTEGER		Minimum y grid point		*
 *	IMAX		INTEGER		Maximum x grid point		*
 *	JMAX		INTEGER		Maximum y grid point		*
 *									*
 * Input and output parameters:						*
 *	GRID (KX,KY)	REAL		Grid of data to be scaled	*
 *									*
 * Output parameters:							*
 *	RMIN		REAL		Data minimum			*
 *	RMAX		REAL		Data maximum			*
 *	IRET		INTEGER		Return code			*
 *					  0 = normal return		*
 *					 -8 = no data in range		*
 *					 -9 = invalid subset range	*
 *					-14 = scaling not possible	*
 **									*
 * M. desJardins/GSFC	 6/88						*
 * G. Huffman/GSC	 1/89	Warn for SCALE in [-100,-20],[20,100]	*
 * M. desJardins/GSFC	 7/89	Scale larger to get integral intervals	*
 * K. Brill/GSC          5/90   Return missing scale for small grd val  *
 * K. Brill/GSC          5/90   Change SSCALE + 10. to * 10. & 5 to 9.01*
 * K. Brill/NMC		08/91	Allow for scaling when rmax = rmin	*
 * L. Sager/NMC		 8/93	GR_SSCL created from GR_SCAL		*
 * P. Bruehl/Unidata	 9/94	Cleaned up (as per MdJ), scale < abs(20)*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/

    gr_sscl ( iscale, kx, ky, imin, jmin, imax, jmax, grid, rmin, rmax,
        iret );

    return;
}
