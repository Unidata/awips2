#include "geminc.h"
#include "gemprm.h"
#include "ercmn.h"

void er_gerrmsg ( int *index, char *msg, int *iret )
/************************************************************************
 * er_gerrmsg								*
 *									*
 * This routine gets the "index"th error message in the error buffer.	*
 *									*
 * er_gerrmsg ( index, msg )						*
 *									*
 * Input parameters:							*
 *	*index		int		Index of the desired message	*
 *									*
 * Output parameters:							*
 *	*msg		char		Error message			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					  1 = no msgs in buffer		*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	01/04	initial coding				*
 * J. Wu/SAIC	 	02/04	input index as a pointer		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    
    *iret = 0;
           
    if ( nermsg > 0 && *index >= 0 && *index < nermsg ) {
        strcpy ( msg, errmsg[*index] );
    }
    else {
        strcpy ( msg, "" );
	*iret = 1;
    }
    
}
