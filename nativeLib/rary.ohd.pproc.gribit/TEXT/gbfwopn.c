#include <stdio.h>
#include "create_fortran_link.h"

/*
#include "geminc.h"
#include "gemprm.h"
*/
#ifdef UNDERSCORE
#define gbf_wopn gbf_wopn_
#endif

FILE	*fptr;

create_fortran_link( void,gbf_wopn,(char* filnam,int* lfilnam, int* iret),( filnam, lfilnam, iret ))

/************************************************************************
 * gbf_wopn								*
 *									*
 * This function opens a GRIB file for writing.  If the file does not	*
 * exist, it is created.  All writes are forced to the end of the file.	*
 *									*
 * This function is intended to be called from FORTRAN to open a single *
 * output GRIB file for write access using gbf_writ.  When the writing	*
 * is done, call gbf_clos to close the file.  Only one file can be open	*
 * at a time.								*
 *									*
 * gbf_aopn ( filnam, lfilnam, iret )						*
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
	fptr = fopen ( filnam, "w" );
	if ( fptr == NULL ) {
	    printf ( "GRIB file could not be opened.\n" );
	}

	return;


}
