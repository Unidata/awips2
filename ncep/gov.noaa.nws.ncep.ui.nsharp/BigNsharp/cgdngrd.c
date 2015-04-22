#include "fortran_wrappers.h"

void cgd_ngrd ( int iacss, int *numgrd, char *firstm, char *lasttm,
                int *iret )
/************************************************************************
 * cgd_ngrd                                                             *
 *                                                                      *
 * This subroutine returns the number of grids in a grid file along     *
 * with the first and last time.                                        *
 *                                                                      *
 * cgd_ngrd ( iacss, numgrd, firstm, lasttm, iret )                     *
 *                                                                      *
 * Input parameters:                                                    *
 *      *iacss		int		Grid access number              *
 *                                                                      *
 * Output parameters:                                                   *
 *      *numgrd		int		Number of grids                 *
 *      *firstm		char		Earliest time1 in file          *
 *      *lasttm		char		Latest time1 in file            *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                       -4 = file not open             *
 *                                       -6 = read error                *
 **                                                                     *
 * Log:                                                                 *
 * R.Tian/SAIC		 1/06	C wrapper of GD_NGRD			*
 ************************************************************************/
{
    char ftime[MXFLSZ+1], ltime[MXFLSZ+1];
    int len, ier;
/*----------------------------------------------------------------------*/
    /*
     * CALL GD_NGRD.
     */
    fprintf(stderr, "cgdngrd.c: entering\n");
    fprintf(stderr, "cgdngrd.c: before gd_ngrd iacss = %d\n", iacss);
    gd_ngrd ( &iacss, &numgrd, ftime, ltime, &iret );
    fprintf(stderr, "cgdngrd.c: after gd_ngrd iacss = %d\n", iacss);
    fprintf(stderr, "cgdngrd.c: after gd_ngrd numgrd = %d\n", numgrd);
    fprintf(stderr, "cgdngrd.c: after gd_ngrd iret = %d\n", iret);

    /*
     * Convert Fortran string to C string.
     */
    ftime[MXFLSZ] = '\0';
    cst_lstr ( ftime, &len, &ier );
    ftime[len] = '\0';
    strcpy ( firstm, ftime );

    fprintf(stderr, "cgdngrd.c: after gd_ngrd ftime = %s\n", ftime);

    ltime[MXFLSZ] = '\0';
    cst_lstr ( ltime, &len, &ier );
    ltime[len] = '\0';
    strcpy ( lasttm, ltime );

    fprintf(stderr, "cgdngrd.c: after gd_ngrd ltime = %s\n", ltime);

    return;
}
