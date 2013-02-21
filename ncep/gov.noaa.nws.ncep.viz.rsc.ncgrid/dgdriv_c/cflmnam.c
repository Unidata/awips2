#include "geminc.h"
#include "gemprm.h"

void cfl_mnam ( char *dattim, char *templt, char *filnam, int *iret )
/************************************************************************
 * cfl_mnam								*
 *									*
 * This function constructs a file name using the template and a GEMPAK	*
 * date/time string.  The GEMPAK date/time string must be complete and	*
 * standard, i.e., it may not have a 4-digit year.			*
 *									*
 *	YYYY		Year with the century				*
 *	YY		Year without the century			*
 *	MMM		Month 3 letter abbreviation			*
 *	NNN		Month 3 letter abbreviation, all caps		*
 *	MM		Month number					*
 *	DD		Day						*
 *	HH		Hour						*
 *	NN		Minute						*
 *	DWK		Day of the week 3 letter abbreviation		*
 *	DWU		Day of the week 3 letter abbreviation, all caps	*
 *	fFFF		Forecast hour (3 digits)			*
 *      fFF             Forecast hour (2 digits)                        *
 *									*
 * cfl_mnam ( dattim, templt, filnam, iret )				*
 *									*
 * Input parameters:							*
 *	*dattim		char		Full GEMPAK Date/time string	*
 *	*templt		char		File name template		*
 *									*
 * Output parameters:							*
 *	*filnam		char		File name			*
 *	*iret		int		Return code			*
 **									*
 * G. Krueger/EAI	 3/96						*
 * S. Jacobs/NCEP	 6/96	Changed to mixed case for month and day	*
 *				names; changed minutes to NN		*
 * G. Krueger/EAI	 8/96	Match with FL library; Simplified	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 4/99	Added UC ver of month and day-of-week	*
 * D.W.Plummer/NCEP	 3/00	Added FFF forecast hour processing	*
 * M. Li/SAIC		02/02	Added check for the length of data/time	*
 * M. Li/SAIC		03/01	Modified the forecast time processing	*
 ***********************************************************************/
{
	int	dtyear, dtmonth, idtarr[5], idaywk, lf, ier;
	char	*pyy, *pmm, *pdd, *phh, *pmn, *pfff, *pdwk, tstr[3], 
		fcst[8], tfc[8];
	char	moname[12][4] = { "Jan", "Feb", "Mar", "Apr", "May",
				  "Jun", "Jul", "Aug", "Sep", "Oct",
				  "Nov", "Dec" };
	char	noname[12][4] = { "JAN", "FEB", "MAR", "APR", "MAY",
				  "JUN", "JUL", "AUG", "SEP", "OCT",
				  "NOV", "DEC" };
	char	dwname[7][4] = { "Sun", "Mon", "Tue", "Wed", "Thu",
				 "Fri", "Sat" };
	char	wdname[7][4] = { "SUN", "MON", "TUE", "WED", "THU",
				 "FRI", "SAT" };
/*---------------------------------------------------------------------*/
	*iret = 0;
	strcpy ( filnam, templt );
/*
 *	Substitute the first 2 digits of a 4 digit year for YYYY.
 */
	pyy = strstr ( filnam, "YYYY" );
	if ( pyy != NULL ) {
	    cst_ncpy ( tstr, dattim, 2, &ier );
	    cst_numb ( tstr, &dtyear, &ier );
	    if ( dtyear <= 50 ) {
		memcpy ( pyy, "20", 2 );
	    } else {
		memcpy ( pyy, "19", 2 );
	    }
	}
/*
 *	Substitute the 2 digit year for YY.
 */
	pyy = strstr ( filnam, "YY" );
	if ( pyy != NULL ) memcpy ( pyy, dattim, 2 );
/*
 *	Substitute the 3 character month for MMM.
 */
	pmm = strstr ( filnam, "MMM" );
	if ( pmm != NULL ) {
	    cst_ncpy ( tstr, dattim + 2, 2, &ier );
	    cst_numb ( tstr, &dtmonth, &ier );
	    if ( dtmonth >= 1 && dtmonth <= 12 )
		memcpy ( pmm, moname[dtmonth-1], 3 );
	}
/*
 *	Substitute the 3 character month for NNN.
 */
	pmm = strstr ( filnam, "NNN" );
	if ( pmm != NULL ) {
	    cst_ncpy ( tstr, dattim + 2, 2, &ier );
	    cst_numb ( tstr, &dtmonth, &ier );
	    if ( dtmonth >= 1 && dtmonth <= 12 )
		memcpy ( pmm, noname[dtmonth-1], 3 );
	}
/*
 *	Substitute the 2 digit month for MM.
 */
	pmm = strstr ( filnam, "MM" );
	if ( pmm != NULL ) memcpy ( pmm, dattim + 2, 2 );
/*
 *	Substitute the 2 digit day for DD.
 */
	pdd = strstr ( filnam, "DD" );
	if ( pdd != NULL ) memcpy ( pdd, dattim + 4, 2 );
/*
 *	Substitute the 2 digit hour for HH.
 */
	phh = strstr ( filnam, "HH" );
	if ( phh != NULL ) memcpy ( phh, dattim + 7, 2 );
/*
 *	Substitute the 2 digit minute for NN.
 */
	pmn = strstr ( filnam, "NN" );
	if ( pmn != NULL ) memcpy ( pmn, dattim + 9, 2 );
/*
 *	Substitute the 3 character day of the week for DWK.
 */
	pdwk = strstr ( filnam, "DWK" );
	if ( pdwk != NULL ) {
	    ti_ctoi ( dattim, idtarr, &ier, strlen (dattim) );
	    ti_dayw ( idtarr, &idaywk, &ier );
	    memcpy ( pdwk, dwname[idaywk-1], 3 );
	}
/*
 *	Substitute the 3 character day of the week for DWU.
 */
	pdwk = strstr ( filnam, "DWU" );
	if ( pdwk != NULL ) {
	    ti_ctoi ( dattim, idtarr, &ier, strlen (dattim) );
	    ti_dayw ( idtarr, &idaywk, &ier );
	    memcpy ( pdwk, wdname[idaywk-1], 3 );
	}

	if (strlen(dattim) > (size_t)11) {
/*
 *	    Add '0' if not 3 or 5 letters
 */
	    cst_ncpy(fcst, dattim + 12, strlen(dattim)-12, &ier);
	    lf = strlen(fcst);

	    if ((lf == 4) || (lf ==2)) {
		strcpy(tfc, "0");
		strncat(tfc, fcst, lf);
		strcpy(fcst, tfc);
	    }
	    else if (lf ==1) {
                strcpy(tfc, "00");
                strncat(tfc, fcst, lf);
                strcpy(fcst, tfc);
	    }
		
/*
 *	    Substitute the 3-digit forecast hour for fFFF
 */
	    pfff = strstr ( filnam, "FFF" );
	    if ( pfff != NULL )  {
		memcpy(pfff, fcst, 3);
	    }
/*
 *          Substitute the 2-digit forecast hour for fFF
 */
            pfff = strstr ( filnam, "FF" );
            if ( pfff != NULL )  {
                memcpy ( pfff, fcst+1, 2 );
            }

	}

}
