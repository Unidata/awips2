#include <stdio.h>
/*
#include "geminc.h"
#include "gemprm.h"
*/
#ifdef UNDERSCORE
#define gbf_ropn gbf_ropn_
#endif

FILE	*fptr;

void
   gbf_ropn ( filnam, lfilnam, iret )

	char *filnam;
	int  *lfilnam, *iret;

/************************************************************************
 * gbf_ropn								*
 *									*
 * This function opens a GRIB file for appending.  If the file does not	*
 * exist, it is created.  All writes are forced to the end of the file.	*
 *									*
 * This function is intended to be called from FORTRAN to open a single *
 * output GRIB file for write access using gbf_writ.  When the writing	*
 * is done, call gbf_clos to close the file.  Only one file can be open	*
 * at a time.								*
 *									*
 * gbf_ropn ( filnam, lfilnam, iret )						*
 *									*
 * Input parameters:							*
 *	*filnam		char		File name			*
 *      *lfilnam        int             Length of file name             *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * G. Krueger/EAI	3/96						*
 * G. Krueger/EAI	8/96	Match with FL library			*
 * K. Brill/HPC		8/99	Adapted from cfl_aopn; Move *fptr from	*
 *				call to external			*
 ***********************************************************************/
{
	int	ier;
/*	char	newname[LLPATH];  */
/*---------------------------------------------------------------------*/
	*iret = 0;

/*	css_envr ( filnam, newname, &ier );
	fptr = fopen ( newname, "a+" ); 
*/
        filnam[*lfilnam] = '\0';
	fptr = fopen ( filnam, "r" );
	if ( fptr == NULL ) {
	    printf ( "GRIB file could not be opened.\n" );
	}

	return;


}
