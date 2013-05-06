#include "geminc.h"
#include "gemprm.h"
#include "ercmn.h"

void er_gnumerr ( int *errnum, int *iret )
/************************************************************************
 * er_gnumerr								*
 *									*
 * This routine gets the number of error message in the error buffer.	*
 *									*
 * er_gnumerr ( errnum, iret )						*
 *									*
 * Output parameters:							*
 *	*errnum		int		Number of errors		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					  1 = no msgs in buffer		*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	01/04	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    
    *iret = 0;
//printf ( " in er_gnumerr %d \n", nermsg );
    
    if ( nermsg > 0 && nermsg <= MXERST ) {
        *errnum = nermsg;
    }
    else {
        *errnum = 0;	
	*iret = 1;
    }

}
