#include "geminc.h"
#include "gemprm.h"

void grc_rnav  ( const float *rnvblk, char *proj, int *kx, int *ky, 
		int *iret )
/************************************************************************
 * GR_RNAV								*
 *									*
 * This subroutine gets the projection and grid size from a grid	*
 * navigation block.							*
 *									*
 * GR_RNAV  ( RNVBLK, PROJ, KX, KY, IRET )				*
 *									*
 * Input parameters:							*
 *	RNVBLK (LLNNAV)	REAL		Navigation block		*
 *									*
 * Output parameters:							*
 *	PROJ		CHAR*		Projection name			*
 *	KX		INTEGER		Number of points in x dir	*
 *	KY		INTEGER		Number of points in y dir	*
 *	IRET		INTEGER		Return code			*
 *					  0 = normal return		*
 *					 -6 = invalid navigation	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 8/88	Modified from GR_RSNV			*
 * M. Linda/GSC		 9/97	Corrected right border of prologue	*
 * D.W.Plummer/NCEP      2/06   Translated from FORTRAN                 *
 ***********************************************************************/
{
   char tproj[5];
   int nc, ier;
/*---------------------------------------------------------------------*/
	gr_rnav ( (float *)rnvblk, tproj, kx, ky, iret, sizeof(tproj) );
	tproj[4] = '\0';
	cst_lstr ( tproj, &nc, &ier );
	tproj[nc] = '\0';
	strcpy ( proj, tproj );

	return;
}
