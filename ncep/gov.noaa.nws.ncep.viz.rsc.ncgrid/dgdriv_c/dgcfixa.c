#include "dg.h"

void dgc_fixa ( const char *area, const char *proj, char *areout,
                char *prjout, int *iret )
/************************************************************************
 * dgc_fixa								*
 *									*
 * This subroutine takes AREA and replaces GRID or DSET with the grid   *
 * area, EXTEND with the extend area, and DATA with the data area.	*
 * GRID or DSET is obtained from the navigation block; EXTEND and	*
 * DATA are obtained from the analysis block.				*
 *									*
 * dgc_fixa ( area, proj, areout, prjout, iret )			*
 *									*
 * Input parameters:							*
 *	*area		const char	Area				*
 *	*proj		const char	Projection			*
 *									*
 * Output parameters:							*
 *	*areout		char		New area			*
 *	*prjout		char		New projection			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-54 = grid file open failed	*
 **									*
 * Log:									*
 * K. Brill/HPC		01/04	Created from GR_FIXA for use with new	*
 *				DG subroutines--do not pass in file nmbr*
 * R. Tian/SAIC          3/04   Modified to use new GD file management  *
 * R. Tian/SIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char str1[29], str2[29], str3[29], str4[29], aaa[73];
    char str6[29], str7[29], str8[29], prj[21];
    float adum1, adum2, dn, dx, dy, gbnds[4], ebnds[4], dbnds[4],
          rnav[LLNNAV], ranl[LLNANL];
    int igdfln, mxgd, navlen, ianlsz, iex[4], nc, zero, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    gd_open ( _nfile.crtfnm[_nfile.irefnv], &_nfile.outflg[_nfile.irefnv],
               &zero, &zero, &igdfln, &adum1, &adum2, &mxgd, &ier,
	       strlen(_nfile.crtfnm[_nfile.irefnv]) );
    if ( ier != 0 ) {
	*iret = -54;
	return;
    }

    /*
     * Convert to upper case.
     */
    cst_lcuc ( (char *)area, areout, &ier );
    cst_lcuc ( (char *)proj, prjout, &ier );

    /*
     * Check for word GRID.
     */
    if ( strstr ( areout, "GRID" ) || strstr ( areout, "DSET" ) ) {
	/*
	 * Read in navigation block.
	 */
	gd_gnav ( &igdfln, rnav, &navlen, &ier );
	if ( navlen >= 13 ) {
	    cst_rlch ( rnav[6], 2, str1, &ier );
	    cst_rlch ( rnav[7], 2, str2, &ier );
	    cst_rlch ( rnav[8], 2, str3, &ier );
	    cst_rlch ( rnav[9], 2, str4, &ier );
	    cst_ldsp ( str1, str1, &nc, &ier );
	    cst_ldsp ( str2, str2, &nc, &ier );
	    cst_ldsp ( str3, str3, &nc, &ier );
	    cst_ldsp ( str4, str4, &nc, &ier );

	    strcpy ( aaa, str1 );
	    strcat ( aaa, ";" );
	    strcat ( aaa, str2 );
	    strcat ( aaa, ";" );
	    strcat ( aaa, str3 );
	    strcat ( aaa, ";" );
	    strcat ( aaa, str4 );
	    if ( strstr ( areout, "GRID" ) ) {
	        cst_rpst ( areout, "GRID", aaa, areout, &ier );
	    } else {
	        cst_rpst ( areout, "DSET", aaa, areout, &ier );
	    }

	    /*
	     * Set projection if PROJ is blank.
	     */
	    if ( strlen ( prjout ) == 0 ) {
		cst_itos ( (int *)(&rnav[1]), 1, &nc, prj, &ier );
		cst_rmbl ( prj, prj, &nc, &ier );
		cst_rlch ( rnav[10], 2, str6, &ier );
		cst_rlch ( rnav[11], 2, str7, &ier );
		cst_rlch ( rnav[12], 2, str8, &ier );
	        cst_ldsp ( str6, str6, &nc, &ier );
	        cst_ldsp ( str7, str7, &nc, &ier );
	        cst_ldsp ( str8, str8, &nc, &ier );
		strcpy ( prjout, prj );
		strcat ( prjout, "/" );
		strcat ( prjout, str6 );
		strcat ( prjout, ";" );
		strcat ( prjout, str7 );
		strcat ( prjout, ";" );
		strcat ( prjout, str8 );
	    }
	}
    }

    /*
     * Check for EXTEND.
     */
    if ( strstr ( areout, "EXTEND" ) ) {
	/*
	 * Read in analysis block.
	 */
	gd_ganl ( &igdfln, ranl, &ianlsz, &ier );
	grc_rban ( ranl, &dn, &dx, &dy, gbnds, ebnds, dbnds, iex, &ier );
	if ( ier == 0 ) {
	    cst_rlch ( ebnds[0], 2, str1, &ier );
	    cst_rlch ( ebnds[1], 2, str2, &ier );
	    cst_rlch ( ebnds[2], 2, str3, &ier );
	    cst_rlch ( ebnds[3], 2, str4, &ier );
	    cst_ldsp ( str1, str1, &nc, &ier );
	    cst_ldsp ( str2, str2, &nc, &ier );
	    cst_ldsp ( str3, str3, &nc, &ier );
	    cst_ldsp ( str4, str4, &nc, &ier );

	    strcpy ( aaa, str1 );
	    strcat ( aaa, ";" );
	    strcat ( aaa, str2 );
	    strcat ( aaa, ";" );
	    strcat ( aaa, str3 );
	    strcat ( aaa, ";" );
	    strcat ( aaa, str4 );
	    cst_rpst ( areout, "EXTEND", aaa, areout, &ier );
	}
    }

    /*
     * Check for DATA.
     */
    if ( strstr ( areout, "DATA" ) ) {
	/*
	 * Read in analysis block.
	 */
	gd_ganl ( &igdfln, ranl, &ianlsz, &ier );
	grc_rban ( ranl, &dn, &dx, &dy, gbnds, ebnds, dbnds, iex, &ier );
	if ( ier == 0 ) {
	    cst_rlch ( dbnds[0], 2, str1, &ier );
	    cst_rlch ( dbnds[1], 2, str2, &ier );
	    cst_rlch ( dbnds[2], 2, str3, &ier );
	    cst_rlch ( dbnds[3], 2, str4, &ier );
	    cst_ldsp ( str1, str1, &nc, &ier );
	    cst_ldsp ( str2, str2, &nc, &ier );
	    cst_ldsp ( str3, str3, &nc, &ier );
	    cst_ldsp ( str4, str4, &nc, &ier );

	    strcpy ( aaa, str1 );
	    strcat ( aaa, ";" );
	    strcat ( aaa, str2 );
	    strcat ( aaa, ";" );
	    strcat ( aaa, str3 );
	    strcat ( aaa, ";" );
	    strcat ( aaa, str4 );
	    cst_rpst ( areout, "DATA", aaa, areout, &ier );
	}
    }

    return;
}
