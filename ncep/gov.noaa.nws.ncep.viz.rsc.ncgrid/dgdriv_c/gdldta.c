#include "gdlist.h"

void gdldta ( const char *gdfile, const char *gdtime1, const char *gdtime2,
              const int *level1, const int *level2, const int *ivcord,
	      const char *parm, const float *grid, const int *kx,
	      const int *ky, const char *area, const int *ix1, const int *iy1,
	      const int *ix2, const int *iy2, const int *iscale,
	      const int *termflg, const int *fileflg, const char *outfil,
	      int *iret )
/************************************************************************
 * gdldta								*
 *									*
 * This subroutine prints data for the GDLIST program.			*
 *									*
 * gdldta  ( gdfile, gdtime1, gdtime2, level1, level2, ivcord, parm,	*
 *           grid, kx, ky, area, ix1, iy1, ix2, iy2, iscale, termflg,	*
 *           fileflg, outfil, iret )					*
 * Input parameters:							*
 *	*gdfile		const char	Grid file			*
 *	*gdtime1	const char	Grid time			*
 *	*gdtime2	const char	Grid time			*
 *	*level1		const int	Grid level			*
 *	*level2		const int	Grid level			*
 *	*ivcord		const int	Grid coordinate			*
 *	*parm		const char	Grid parameter			*
 *	*grid		const float	Grid data			*
 *	*kx		const int	Number of grid points in x dir	*
 *	*ky		const int	Number of grid points in y dir	*
 *	*area		const char	Data area			*
 *	*ix1		const int	First column			*
 *	*iy1		const int	First row			*
 *	*ix2		const int	Last column			*
 *	*iy2		const int	Last row			*
 *	*iscale		const int	Scaling factor			*
 *	*termflg	const int	Terminal output flag		*
 *	*fileflg	const int	File output flag		*
 *	*outifl		const int	Output file name		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 4/85						*
 * M. desJardins/GSFC	 6/88	Cleaned up				*
 * K. Brill/GSC          3/90   Fixed so there are no writes past col 80*
 * R. Tian/SAIC		10/04	Modified to list grid sub-set		*
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 ************************************************************************/
{
    int tltflg, first;
    int nfps, ixx1, ixx2, iyy1, iyy2, kxw, kyw, zero, one, i, j, k,
        ii, jj, kk, ier;
    float fxx1, fxx2, fyy1, fyy2;
    FILE *fps[2], *fp;
/*----------------------------------------------------------------------*/
    *iret = 0;
    tltflg = G_TRUE;
    zero = 0;
    one = 1;

    /*
     * Loop through each output device.
     */
    nfps = 0;
    if ( *termflg == G_TRUE ) {
        fps[nfps++] = stdout;
    }
    if ( *fileflg == G_TRUE ) {
        fp = cfl_wopn ( (char *)outfil, &ier );
	fps[nfps++] = fp;
    }
    for ( ii = 0; ii < nfps; ii++ ) {
	/*
	 * Write out the grid file name.
	 */
	fprintf ( fps[ii], "\n\n Grid file: %s\n", gdfile );

	/*	 
	 * Write out the grid identifier.
	 */
	fprintf ( fps[ii], " GRID IDENTIFIER: \n" );
	grc_wtrm ( fps[ii], &tltflg, &zero, gdtime1, gdtime2, level1, level2,
	           ivcord, parm, &ier );

	/*
	 * Write out the area being printed.
	 */
	fxx1 = (float)(*ix1);
	fxx2 = (float)(*ix2);
	fyy1 = (float)(*iy1);
	fyy2 = (float)(*iy2);
	dg_igrg ( &one, &fxx1, &fyy1, &fxx1, &fyy1, &ier );
	dg_igrg ( &one, &fxx2, &fyy2, &fxx2, &fyy2, &ier );
	ixx1 = G_NINT ( fxx1 );
	ixx2 = G_NINT ( fxx2 );
	iyy1 = G_NINT ( fyy1 );
	iyy2 = G_NINT ( fyy2 );
	dg_qkxy ( &kxw, &kyw, &ier );
	fprintf ( fps[ii], " AREA: %-47.47s     GRID SIZE: %5d%5d\n"
	                   " COLUMNS: %5d%5d     ROWS: %5d%5d\n\n",
	    area, kxw, kyw, ixx1, ixx2, iyy1, iyy2 );

	/*
	 * Write out the scale factor.
	 */
	fprintf ( fps[ii], " Scale factor: 10**%2d\n\n\n", *iscale );

	/*
	 * Write out column numbers.
	 */
	first = G_TRUE;
	k = ixx1;
	while ( k <= ixx2 ) {
	    if ( first == G_TRUE ) {
	        fprintf ( fps[ii], " COLUMN:" );
	        first = G_FALSE;
	    } else {
	        fprintf ( fps[ii], "\n        " );
	    }
	    for ( i =0; i < 8 && k <= ixx2; i++, k++ ) {
	        fprintf ( fps[ii], "   %4d  ", k );
	    }
	}
	fprintf ( fps[ii], "\n" );

	/*
	 * Loop through rows from the top down.
	 */
	for ( jj = iyy2, j = *iy2; j >= *iy1; j--, jj-- ) {
	    first = G_TRUE;
	    k = *ix1;
	    while ( k <= *ix2 ) {
	        if ( first == G_TRUE ) {
		    fprintf ( fps[ii], " ROW%3d ", jj );
		    first = G_FALSE;
		} else {
		    fprintf ( fps[ii], "\n        " );
		}
		for ( i = 0; i < 8 && k <= *ix2; i++, k++ ) {
		    kk = ( j - 1 ) * (*kx) + k - 1;
		    fprintf ( fps[ii], "%9.2f", grid[kk] );
	        }
	    }
	    fprintf ( fps[ii], "\n" );
	}
    }

    /*
     * Close file output.
     */
    if ( *fileflg == G_TRUE ) cfl_clos ( fp, &ier );

    return;
}
