#include "geminc.h"
#include "gemprm.h"
#include "ercmn.h"


void er_smsg ( char *string, int *iret )
/************************************************************************
 * er_smsg								*
 *									*
 * This routine adds an error message to the error buffer.  If the	*
 * buffer is full, the first (oldest) message is removed.		*
 *									*
 * er_smsg  ( string, iret )						*
 *									*
 * Input parameters:							*
 *	*string		char		Error message			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *									*
 **									*
 * Log:									*
 * K. Tyle/GSC		12/96						*
 * K. Tyle/GSC		 1/97	Change nstr to nermsg; MXNSTR to MXERST	*
 * C. Lin/EAI		 1/97	msgstr to errmsg			*
 * S. Maxwell/GSC	 6/97	Documentation changes			*
 * T. Piper/SAIC	12/01	Fixed ABR; reduced loop by one		*
 * T. Piper/SAIC	02/04	Removed lens parameter			*
 ***********************************************************************/
{
	int 	ii;
	char	tmp[133];
/*---------------------------------------------------------------------*/
	*iret = 0;
	if ( nermsg < MXERST ) {
	    strcpy ( errmsg[nermsg], string);  
	    nermsg++;
	}
	else if ( nermsg == MXERST ) {
	    for ( ii = 0; ii < MXERST-1; ii++ ) {
		strcpy ( tmp, errmsg[ii+1] );
		strcpy ( errmsg[ii], tmp );
	    }
	    strcpy ( errmsg[nermsg-1], string );
	}
}
