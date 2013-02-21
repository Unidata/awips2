#include "dg.h"

#define NDT		14
#define NDTM		5

void dg_prft ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_prft								*
 *									*
 * This subroutine accumulates precipitation over a given time period.	*
 * The time period is hh hours given in the parameter name which has	*
 * the form PhhM or PhhI.  Conversions to inches or millimeters will	*
 * be automatic.							*
 *									*
 * The following assumptions are made:					*
 *									*
 *	1.  The precipitation totals exist at forecast times		*
 *	    at regular intervals in whole hours.			*
 *									*
 *	2.  At regular intervals the precipitation is given for		*
 *	    the entire interval.  This is a major interval.		*
 *									*
 *	3.  At regular subintervals within the major intervals,		*
 *	    the precipitation is accumulated since the start of		*
 *	    the major interval.						*
 *									*
 * dg_prft ( time1, time2, level1, level2, ivcord, parm, num, iret )    *
 *									*
 * Input parameters:							*
 *      *time1          const char      Date/time                       *
 *      *time2          const char      Date/time                       *
 *      *level1         const int       Level                           *
 *      *level2         const int       Level                           *
 *      *ivcord         const int       Vertical coordinate             *
 *      *parm           const char      Parameter name                  *
 *									*
 * Input and output parameters:						*
 *      *num            int             Location of grid                *
 *									*
 * Output parameters:							*
 *      *iret           int             Return code                     *
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 **									*
 * Log:									*
 * K. Brill/NMC		10/92						*
 * K. Brill/NMC		 2/93	Use first character of parm 		*
 * M. desJardins/NMC	 7/92	DG_UHDR --> DG_UPSG			*
 * K. Brill/NMC		11/93	Take care of negative precip		*
 * M. desJardins/NMC	 3/94	Reorganize				*
 * S. Jacobs/NMC	10/94	Check for parms w/ PR which aren't rates*
 * K. Brill/NMC		 5/95	Add 3-h major interval			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * K. Brill/HPC		 2/03	Call DG_FRIG to free internal grids	*
 * T. Lee/SAIC		10/03	Increased temporal resolution for hrly	*
 * T. Lee/SAIC		 1/04	Handled Canadian precip. grids		*
 * R. Tian/SAIC		 1/05	Fixed bug for GFS data			*
 * S. Jacobs/NCEP	 9/05	Fixed counter when in CANADIAN mode	*
 * R. Tian/SAIC          2/06   Recoded from Fortran                 	*
 * S. Jacobs/NCEP	 8/09	Added 1hr to major interval list	*
 ************************************************************************/
{
    /*
     * Set possible subinterval accumulation periods.
     */
    char cdt[NDT][3] = { "03", "06", "09", "12", "01", "02",
			 "04", "05", "07", "08", "10", "11", 
			 "24", "48" };
    int idt[NDT]     = {  3,    6,    9,    12,   1,    2,
    			  4,    5,    7,    8,    10,   11,
			  24,   48 };
    /*
     * Set possible major interval lengths (hours).
     */
    char cdtm[NDTM][4] = { "01", "03", "06", "12", "24" };
    int idtm[NDTM]     = {   1,    3,    6,   12,   24 };

    char thold[5], pnum[14], fstchr, lstchr, accum[73], ftype, ftime[4],
        time21[21], time22[21];
    float rmult, signp;
    int intdtf[3], jvcord, iacc, lenstr, ihrs, ihhlst, iptime, lenst,
        numsub, nummaj, majts, iz, itet, i, ier, ierr;
    int done;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * Set the vertical coordinate to missing since it can be ignored.
     */
    jvcord = -1;

    /*
     * Check to see if precipitation rate is requested.  If so,
     * check for the data immediately.
     */
    if ( strncmp ( parm, "PR", 2 ) == 0 ) {
	strcpy ( accum, &parm[2] );
	cst_numb ( accum, &iacc, &ierr );
	if ( ierr == 0 ) {
	    dg_grdr ( time1, time2, level1, level2, &jvcord, parm, num, iret );
	}
	return;
    }

    /*
     * Check that precipitation in form P|S|C nnn I|M has been requested.
     */
    cst_lstr ( (char *)parm, &lenstr, &ier );
    if ( lenstr <= 2 ) return;
    fstchr = parm[0];
    if ( ( fstchr != 'P' ) && ( fstchr != 'S' ) && ( fstchr != 'C' ) ) return;
    lstchr = parm[lenstr-1];
    if ( ( lstchr != 'I' ) && ( lstchr != 'M' ) ) return;

    /*
     * Get number of hours for the precipitation accumulation.
     */
    strncpy ( pnum, &parm[1], lenstr - 2 );
    pnum[lenstr-2] = '\0';
    cst_numb ( pnum, &ihrs, &ier );
    if ( ier != 0 ) return;

    /*
     * Check to see if precipitation can be found as a rate or as a
     * combination of P, S, C precipitation values.
     */
    dg_prcp ( time1, time2, level1, level2, &jvcord, parm, num, iret );
    if ( *iret == 0 ) return;

    /*
     * Get the forecast time for this grid.
     */
    tg_ctoi ( (char *)time1, intdtf, &ier, strlen(time1) );
    if ( intdtf[2] == 0 ) return;

    /*
     * Read data based on forecast hours (for Canadian data)
     */
    ctg_cftm ( intdtf[2], &ftype, ftime, &ier );
    if ( ftime[0] == '0' ) { 
	pnum[0] = fstchr;
	strcpy ( &pnum[1], &ftime[1] );
	lenstr = strlen ( pnum );
	pnum[lenstr] = lstchr;
	pnum[lenstr+1] = '\0';
    } else {
	pnum[0] = fstchr;
	strcpy ( &pnum[1], ftime );
	lenstr = strlen ( pnum );
	pnum[lenstr] = lstchr;
	pnum[lenstr+1] = '\0';
    }
    dg_prcp ( time1, time2, level1, level2, &jvcord, pnum, num, &ier );

    /*
     * Get the accumulation at present time, over x hours, where
     * x is to be determined by looping over the possiblities.
     */
    if ( ier != 0 ) {
	i = -1;
	while ( ( ier != 0 ) && ( i < NDT - 1 ) ) {
	    i++;
	    if ( idt[i] != ihrs ) {
		pnum[0] = fstchr;
		strcpy ( &pnum[1], cdt[i] );
		lenstr = strlen ( pnum );
		pnum[lenstr] = lstchr;
		pnum[lenstr+1] = '\0';
		dg_prcp ( time1, time2, level1, level2, &jvcord, pnum,
		          num, &ier );
	    }
	}

	/*
	 * If some value for precipitation was found, check the number 
	 * of hours accumulated.
	 */
	if ( ier == 0 ) {
	    ihhlst = idt[i];
	} else {
	    return;
	}
    } else {
	cst_numb ( ftime, &ihhlst, iret );
    }

    strcpy ( time22, time2 );
    if ( ihhlst > ihrs ) {
	/*
	 * Go back IHRS hours, get the accumulation at that
	 * forecast time and subtract.
	 */
	intdtf[2] = intdtf[2] - ihrs * 100;
	ctg_itoc ( intdtf, time21, &ier );
	iptime = ihhlst - ihrs;
	cst_inch ( iptime, thold, &ier );
	cst_lstr ( thold, &lenst, &ier );
	if ( lenst == 1 ) {
	    pnum[0] = fstchr;
	    pnum[1] = '0';
	    pnum[2] = thold[0];
	    pnum[3] = lstchr;
	    pnum[4] = '\0';
	} else {
	    pnum[0] = fstchr;
	    strcpy ( &pnum[1], thold );
	    lenstr = strlen ( pnum );
	    pnum[lenstr] = lstchr;
	    pnum[lenstr+1] = '\0';
	}
	dg_nxts ( &numsub, &ier );
	dg_prcp ( time21, time22, level1, level2, &jvcord, pnum,
	          &numsub, iret );
	if ( *iret == 0 ) {
	    rmult = -1.;
	    pd_prcp ( _dggrid.dgg[(*num)-1].grid, _dggrid.dgg[numsub-1].grid,
		&rmult, &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
	}
	dg_frig ( &numsub, &ier );
    } else {
	/*
	 * Accumulate the precipitation.  Total elapsed time is itet.
	 */
	itet = ihhlst;
	dg_nxts ( &nummaj, &ier );
	done = G_FALSE;
	while ( done == G_FALSE ) {
	    /*
	     * Go back IHHLST hours, get accum. and sum it in.
	     */
	    intdtf[2] = intdtf[2] - ihhlst * 100;
	    ctg_itoc ( intdtf, time21, &ier );

	    /*
	     * Determine the major time step.
	     */
	    ier = 999;
	    i = -1;
	    while ( ( ier != 0 ) && ( i < NDTM - 1 ) ) {
		i++;
		pnum[0] = fstchr;
		strcpy ( &pnum[1], cdtm[i] );
		lenstr = strlen ( pnum );
		pnum[lenstr] = lstchr;
		pnum[lenstr+1] = '\0';
		dg_prcp ( time21, time22, level1, level2, &jvcord,
		          pnum, &nummaj, &ier );
	    }
	    if ( ier != 0 ) {
		*iret = -7;
		dg_frig ( &nummaj, &ier );
		return;
	    }
	    majts = idtm[i];

	    /*
	     * Add in the accumulation.
	     */
	    signp = 1.;
	    pd_prcp ( _dggrid.dgg[(*num)-1].grid, _dggrid.dgg[nummaj-1].grid,
		&signp, &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );

	    /*
	     * Increment elapsed time.
	     */
	    itet += majts;
	    if ( itet == ihrs ) {
		done = G_TRUE;
		*iret = 0;
	    } else if ( itet < ihrs ) {
		ihhlst = majts;
	    } else {
		/*
		 * Subtract out over accumulation.
		 */
		iz = ( ihrs - itet ) + majts;
		intdtf[2] = intdtf[2] - iz * 100;
		ctg_itoc ( intdtf, time21, &ier );
		iptime = majts - iz;
		cst_inch ( iptime, thold, &ier );
		cst_lstr ( thold, &lenst, &ier );
		if ( lenst == 1 ) {
	    	    pnum[0] = fstchr;
	    	    pnum[1] = '0';
	    	    pnum[2] = thold[0];
	    	    pnum[3] = lstchr;
	    	    pnum[4] = '\0';
		} else {
	    	    pnum[0] = fstchr;
	    	    strcpy ( &pnum[1], thold );
	    	    lenstr = strlen ( pnum );
	    	    pnum[lenstr] = lstchr;
	    	    pnum[lenstr+1] = '\0';
		}
		dg_prcp ( time21, time22, level1, level2, &jvcord, pnum,
		          &nummaj, &ier );
		if ( ier == 0 ) {
		    signp = -1.;
		    pd_prcp ( _dggrid.dgg[(*num)-1].grid,
		              _dggrid.dgg[nummaj-1].grid, &signp, &_dgfile.kxyd,
			      _dggrid.dgg[(*num)-1].grid, &ier );
		    *iret = 0;
		} else {
		    *iret = -7;
		    dg_frig ( &nummaj, &ier );
		    return;
		}
		done = G_TRUE;
	    }
	}
	dg_frig ( &nummaj, &ier );
    }

    return;
}
