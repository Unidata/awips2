#include <stdio.h>

#include "create_fortran_link.h"

/*
#include "geminc.h"
#include "gemprm.h"
*/
#ifdef UNDERSCORE
#define gbf_clos gbf_clos_
#endif

extern FILE     *fptr;

create_fortran_link( void, gbf_clos, (int* iret), (iret))

/************************************************************************
 * gbf_clos								*
 *									*
 * This function closes the output GRIB file.				*
 *									*
 * gbf_clos ( iret )							*
 *									*
 * Input parameters:							*
 *	None.								*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -6 = No file has been opened	*
 **									*
 * G. Krueger/EAI	3/96						*
 * K. Brill		8/99	Adapted from cvl_clos; make *fptr ext.	*
 ***********************************************************************/
{
	int	ier;
/*---------------------------------------------------------------------*/
	*iret = 0;

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}
/*
 *	Close the file and check the status.
 */
	ier = fclose (fptr);

	if ( ier != 0 ) printf ( "Error closing GRIB file.\n" );


}
