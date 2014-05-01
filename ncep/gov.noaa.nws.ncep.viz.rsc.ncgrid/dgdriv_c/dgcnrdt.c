#include "dg.h"

void dgc_nrdt ( const int *ifpn, const char *time1, const char *time2,
    const int *level1, const int *level2, const int *ivcord,
    const char *parm, float *grid, int *igx, int *igy, int *ighd,
    int *iret )
/************************************************************************
 * dgc_nrdt								*
 *									*
 * This subroutine reads the requested grid from a grid file by calling *
 * GD_RDAT. It also applies any specific functions to the grid, for	*
 * example, adding a column of data or subsetting.			*
 *									*
 * dgc_nrdt ( ifpn, time1, time2, level1, level2, ivcord, parm, grid,	*
 *           igx, igy, ighd, iret )					*
 *									*
 * Input parameters:							*
 *	*ifpn 		const int	GDFILE entry position number	*
 *	*time1		const char   	GEMPAK grid date-time		*
 *	*time2		const char   	GEMPAK grid date-time		*
 *	*level1		const int	GEMPAK grid levle		*
 *	*level2		const int	GEMPAK grid levle		*
 *	*ivcord		const int	GEMPAK vertical coordinate	*
 *					  0 = NONE			*
 *					  1 = PRES			*
 *					  2 = THTA			*
 *					  3 = HGHT			*
 *	*parm		const char	GEMPAK parameter name		*
 *									*
 * Output parameters:							*
 *	*grid		float		Grid data			*
 *	*igx		int		Number of horizontal points	*
 *	*igy		int		Number of vertical points	*
 *	*ighd          	int		Grid header			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid not found		*
 *					-30 = open grid failed		*
 *					-31 = navigation not same	*
 **									*
 * Log:									*
 * R. Tian/SAIC 	10/03						*
 * K. Brill/HPC		01/04	Check Ret code from GD subroutines; do	*
 *				not pass parameter as returnable arg	*
 * K. Brill/HPC		02/04	CALL ER_WMSG for -31			*
 * K. Brill/HPC		02/04	Remove all other ER_WMSG calls		*
 * R. Tian/SAIC          2/04   Modified to use new GD file management  *
 * R. Tian/SAIC          5/04   Added call to DG_T2IG			*
 * R. Tian/SAIC		11/04	Added check for PARM == DRCT		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 * D.W.Plummer/NCEP	10/06	Dynamically allocate transfer grid	*
 ************************************************************************/
{
    char filnam[MXFLSZ+1], tmpnam[MXFLSZ+1], uparm[14];
    float rnvblk[LLNNAV], adum;
    int ifidx, igdf, mxgd, navsz, zero, ier;
    float *transfer_grid;
/*----------------------------------------------------------------------*/
    *iret = 0;
    navsz = LLNNAV;
    zero = 0;

    ifidx = (*ifpn) - 1;
    if ( strlen ( _nfile.ntmplt[ifidx] ) == 0  ||
	( strcmp ( time1, _nfile.crtgdt1[ifidx] ) == 0 && 
	  strcmp ( time2, _nfile.crtgdt2[ifidx] ) == 0 ) ) {
	/*
	 * This GDFILE entry is an actual file
	 */
	strcpy ( filnam, _nfile.crtfnm[ifidx] );
    } else {
	/*
	 * This GDFILE entry is a template 
	 */
	cfl_mnam ( (char *)time1, _nfile.ntmplt[ifidx], tmpnam, &ier );
	strcpy ( filnam, _nfile.gflpth[ifidx] );
	strcat ( filnam, "/" );
	strcat ( filnam, tmpnam );
	strcpy ( _nfile.crtfnm[ifidx], filnam );
     }

    gd_open ( filnam, &_nfile.outflg[ifidx], &zero, &navsz, &igdf, &adum,
               rnvblk, &mxgd, &ier, strlen(filnam) );
    if ( ier != 0 ) {
	*iret = -30;
	return;
    }

    /*
     * Allocate the transfer grid based on the kx and ky of the nav block. 
     * Add 1 to kx to account for the possibility of adding a column later.
     */
    G_MALLOC ( transfer_grid, float, ((int)((rnvblk[4]+1)*rnvblk[5])), 
	     "Error allocating transfer grid" );

    cgd_rdat ( &igdf, time1, time2, level1, level2, ivcord, parm,
               transfer_grid, igx, igy, ighd, &ier );
    if ( ier != 0 ) {
	*iret = -7;
    }
    else  {

        strcpy ( _nfile.crtgdt1[ifidx], time1 );
        strcpy ( _nfile.crtgdt2[ifidx], time2 );

        /*
         * Transfer just-read grid navigation to internal grid navigation.
         */
        cst_lcuc ( (char *)parm, uparm, &ier );
        if ( strcmp ( uparm, "DRCT" ) == 0 ) {
	    ighd[1] = 1;
        }

        dg_t2ig ( rnvblk, ighd, transfer_grid, grid, igx, igy, &ier );
        if ( ier != 0 ) {
	    *iret = -7;
        }

    }
   
    if ( transfer_grid != NULL ) 
        G_FREE ( transfer_grid, float );

    return;
}
