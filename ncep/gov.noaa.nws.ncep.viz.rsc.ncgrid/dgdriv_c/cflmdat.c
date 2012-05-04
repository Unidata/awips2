#include "geminc.h"
#include "gemprm.h"

void cfl_mdat ( char *filnam, char *templt, char *defdat,
		char *dattim, int *iret )
/************************************************************************
 * cfl_mdat								*
 *									*
 * This function constructs a GEMPAK date/time string using the		*
 * template and a file name.						*
 *									*
 * Valid substrings for the template include:				*
 *									*
 *	YYYY		Year with the century				*
 *	YY		Year without the century			*
 *	MMM		Month 3 letter abbreviation			*
 *	MM		Month number					*
 *	DD		Day						*
 *	HH		Hour						*
 *	NN		Minute						*
 *	FFF             3-character forecast hour                       *
 *      FF              2-character forecast hour			*
 *									*
 * cfl_mdat ( filnam, templt, defdat, dattim, iret )			*
 *									*
 * Input parameters:							*
 *	*filnam		char		File name			*
 *	*templt		char		File name template		*
 *	*defdat		char		Default GEMPAK date/time string	*
 *									*
 * Output parameters:							*
 *	*dattim		char		GEMPAK date/time string		*
 *	*iret		int		Return code			*
 *					= 0, normal			*
 *					= -12, cannot decode		*
 **									*
 * G. Krueger/EAI	 8/96						*
 * T. Lee/GSC		 5/99	Initialized month/date to 0101		*
 * S. Jacobs/NCEP	 9/99	Changed call to accept default date/time*
 * D.W.Plummer/NCEP	11/99	Added error checking for translation	*
 *                              and added forecast hour checking	*
 * R. Tian/SAIC		07/06	First replace *'s in template with the	*
 *				corresponding strings from the file	*
 * S. Jacobs/NCEP	 8/06	Counted correctly for adding fcst time	*
 ***********************************************************************/
{
	int	intg, istar, len, ier;
	char	tstr[8], tplate[MXFLSZ];
	char	*pslash, *pcolon, *pname, *pstar;
	char	*pmonth, *pyy, *pmm, *pdd, *phh, *pmn, *pff, *pfff, *pplate;
	char	monames[48] =
		    "JAN/FEB/MAR/APR/MAY/JUN/JUL/AUG/SEP/OCT/NOV/DEC";
        char    temptplate[MXFLSZ], tempfname[MXFLSZ];
/*---------------------------------------------------------------------*/
	*iret = 0;
/*
 *	Initialize return value.
 */
	strcpy ( dattim, defdat );
        strcpy (temptplate, templt);
        strcpy (tempfname, filnam);
     //   printf( "cfl_mdate tempfname= %s\n", tempfname );
     //   printf( "cfl_mdate temptplate= %s\n",temptplate);

/*
 *	Find the file name contained in the path.
 */
	pslash = strrchr ( filnam, '/' );
	pcolon = strrchr ( filnam, ':' );
	if ( pslash != NULL && pslash > pcolon ) {
	    pname = pslash + 1;
	} else if ( pcolon != NULL ) {
	    pname = pcolon + 1;
	} else {
	    pname = filnam;
	}
           
     //   printf( "cfl_mdate pname= %s\n", pname );
     //   printf( "cfl_mdate templt= %s\n",templt );
/*
 *     A2DB change
 */
        pplate = strstr(filnam, templt);
     //   printf( "cfl_mdate pplate= %s\n",pplate );
        if ( pplate != NULL ) {
           pname = strtok(filnam, "-");
           pname = strtok( NULL, "-" );
           sprintf (templt, "%s", "YYYYMMDD_HHMMfFFF");
        //   printf( "cfl_mdate new pname= %s\n", pname );
        //   printf( "cfl_mdate new templt= %s\n",templt );
        }

/*
 *	Replace any '*' in the template with actual characters from
 *	the input file name.
 */
 	pstar = strchr ( templt, '*' );	
	if ( pstar ) {
	    istar = (int)( pstar - templt );
	    if ( *(pstar+1) == '\0' ) {
	        strncpy ( tplate, templt, istar );
		strcpy  ( &tplate[istar], &pname[istar] );
	    } else {
		len = strlen(pname) - strlen(templt) + 1;
	    	strncpy ( tplate, templt, istar );
		strncpy ( &tplate[istar], &pname[istar], len );
		strcpy  ( &tplate[istar+len], &templt[istar+1] );
	    }
	} else {
	    strcpy ( tplate, templt );
	}
/*
 *	Extract the 2 digits of a YY year from the file name.
 */
	pyy = strstr ( tplate, "YY" );
	if ( pyy != NULL )  {
	    memcpy ( tstr, pname + (pyy - tplate), 2 );
	    tstr[2] = CHNULL;
	    cst_numb ( tstr, &intg, &ier );
	    if ( ier != 0 )  {
		*iret = -12;
		return;
	    }
	    else  {
		memcpy ( dattim, tstr, 2 );
	    }
	}
/*
 *	Extract the last 2 digits of a YYYY year from the file name.
 */
	pyy = strstr ( tplate, "YYYY" );
	if ( pyy != NULL )  {
	    memcpy ( tstr, pname + (pyy + 2 - tplate), 2 );
	    tstr[2] = CHNULL;
	    cst_numb ( tstr, &intg, &ier );
	    if ( ier != 0 )  {
		*iret = -12;
		return;
	    }
	    else  {
		memcpy ( dattim, tstr, 2 );
	    }
	}
/*
 *	Extract the 2 digits of an MM month from the file name.
 */
	pmm = strstr ( tplate, "MM" );
	if ( pmm != NULL )  {
	    memcpy ( tstr, pname + (pmm - tplate), 2 );
	    tstr[2] = CHNULL;
	    cst_numb ( tstr, &intg, &ier );
	    if ( ier != 0 )  {
		*iret = -12;
		return;
	    }
	    else  {
		memcpy ( dattim + 2, tstr, 2 );
	    }
	}
/*
 *	Extract the month value from a 3 character MMM month from the
 *	file name.
 */
	pmm = strstr ( tplate, "MMM" );
	if ( pmm != NULL ) {
	    cst_ncpy ( tstr, pname + (pmm - tplate), 3, &ier );
	    cst_lcuc ( tstr, tstr, &ier );
	    pmonth = strstr ( monames, tstr );
	    if ( pmonth == (char *)NULL )  {
		*iret = -12;
		return;
	    }
	    else  {
	        sprintf ( tstr, "%2.2d", (int) ( pmonth - monames ) / 4 + 1 );
	        memcpy ( dattim + 2, tstr, 2 );
	    }
	}
/*
 *	Extract the 2 digits of a DD day from the file name.
 */
	pdd = strstr ( tplate, "DD" );
	if ( pdd != NULL )  {
	    memcpy ( tstr, pname + (pdd - tplate), 2 );
	    tstr[2] = CHNULL;
	    cst_numb ( tstr, &intg, &ier );
	    if ( ier != 0 )  {
		*iret = -12;
		return;
	    }
	    else  {
		memcpy ( dattim + 4, tstr, 2 );
	    }
	}
/*
 *	Extract the 2 digits of an HH hour from the file name.
 */
	phh = strstr ( tplate, "HH" );
	if ( phh != NULL )  {
	    memcpy ( tstr, pname + (phh - tplate), 2 );
	    tstr[2] = CHNULL;
	    cst_numb ( tstr, &intg, &ier );
	    if ( ier != 0 )  {
		*iret = -12;
		return;
	    }
	    else  {
		memcpy ( dattim + 7, tstr, 2 );
	    }
	}
/*
 *	Extract the 2 digits of an NN minute from the file name.
 */
	pmn = strstr ( tplate, "NN" );
	if ( pmn != NULL )  {
	    memcpy ( tstr, pname + (pmn - tplate), 2 );
	    tstr[2] = CHNULL;
	    cst_numb ( tstr, &intg, &ier );
	    if ( ier != 0 )  {
		*iret = -12;
		return;
	    }
	    else  {
		memcpy ( dattim + 9, tstr, 2 );
	    }
	}

	if ( strlen( dattim ) > (size_t)11 )  {
/*
 *	    Extract the 3 digits of an FFF forecast hour from the file name.
 */
	    pfff = strstr ( tplate, "FFF" );
	    if ( pfff != NULL )  {
	        memcpy ( tstr, pname + (pfff - tplate), 3 );
	        tstr[3] = CHNULL;
	        cst_numb ( tstr, &intg, &ier );
	        if ( ier != 0 )  {
		    *iret = -12;
		    return;
	        }
	        else  {
		    dattim[11] = 'F';
		    memcpy ( dattim + 12, tstr, 3 );
	        }
	    }
	    else  {
/*
 *	        Extract the 2 digits of an FF forecast hour from the file name.
 */
	        pff = strstr ( tplate, "FF" );
	        if ( pff != NULL )  {
	            memcpy ( tstr, pname + (pff - tplate), 2 );
	            tstr[2] = CHNULL;
	            cst_numb ( tstr, &intg, &ier );
	            if ( ier != 0 )  {
		        *iret = -12;
		        return;
	            }
	            else  {
		        dattim[11] = 'F';
		        dattim[12] = '0';
		        memcpy ( dattim + 13, tstr, 2 );
	            }
	        }
	    }

	}
        if ( pplate != NULL ) {
           strcpy (templt, temptplate);
           strcpy (filnam, tempfname);
        //   printf( "cfl_mdate templt= %s\n",templt);
        //   printf( "cfl_mdate filnam= %s\n",filnam);
        }

}
