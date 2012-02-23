#include "geminc.h"
#define COORDSYS_GLOBAL
#include "gemprm.h"
#define ERCMN_GLOBAL
#include "ercmn.h"


void er_init ( int *iret )
/************************************************************************
 * er_init								*
 *									*
 * This routine initializes the error buffer which is used to store	*
 * error messages for later use.					*
 *									*
 * er_init ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Tyle/GSC		12/96						*
 * K. Tyle/GSC		 1/97	Change nstr to nermsg; MXNSTR to MXERST	*
 * C. Lin/EAI		 1/97	Change msgstr to errmsg			*
 * S. Maxwell/GSC	 6/97	Documentation changes			*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * A. Hardy/GSC         11/00   Added #define COORDSYS_GLOBAL 		*
 * S. Jacobs/NCEP	 2/01	Changed <= to < for check on MXERST	*
 ***********************************************************************/
{
	int	i;

/*---------------------------------------------------------------------*/
	*iret = 0;
	for ( i = 0; i < MXERST; ++i ) {
	    strcpy(errmsg[i],"");
	}
	nermsg = 0; 
//	printf ( " nermsg initialized to-------> %d \n",nermsg	); 
}
