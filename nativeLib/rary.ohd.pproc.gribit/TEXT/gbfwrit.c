#include <stdio.h>

#include "create_fortran_link.h"

/*
#include "geminc.h"
#include "gemprm.h"
  */
#ifdef UNDERSCORE
#define gbf_writ gbf_writ_
#endif

extern FILE     *fptr;

create_fortran_link( void, gbf_writ, (int* nbytes,unsigned char* buffer,int* iret ),( nbytes, buffer, iret ))

/************************************************************************
 * gbf_writ								*
 *									*
 * This function writes the specified number of bytes from the buffer	*
 * to the open file.							*
 *									*
 * gbf_writ ( nbytes, buffer, iret )					*
 *									*
 * Input parameters:							*
 *	*nbytes		int		Number of bytes to write	*
 *	*buffer		unsigned char	Data to write to file		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -6 = No file has been opened	*
 **									*
 * G. Krueger/EAI	3/96						*
 * K. Brill/HPC		8/99	Adapted from cfl_writ -- make nbytes	*
 *				a pointer, move *fptr to external	*
 ***********************************************************************/
{
	int	ier, nbyts, nbout;
/*---------------------------------------------------------------------*/
	*iret = 0;
	nbyts = *nbytes;

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}
/*
 *	Write the record.
 */
	nbout = fwrite ( buffer, sizeof(unsigned char), nbyts, fptr );

	if ( nbout != nbyts ) {
	    printf ( "Error writing to GRIB file.\n" );
	}


}
