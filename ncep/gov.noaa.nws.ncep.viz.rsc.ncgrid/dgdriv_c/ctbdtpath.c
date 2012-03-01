#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Data_t		DtTable;
extern	int		DtReadin;

void ctb_dtpath ( char *alias_i, char *path, int *iret )
/************************************************************************
 * ctb_dtpath								*
 *									*
 * This function returns the path associated with a data alias.		*
 *									*
 * ctb_dtpath ( alias_i, path, iret )					*
 *									*
 * Input parameters:							*
 *      *alias_i	char	Alias name				*
 *									*
 * Output parameters:							*
 *      *path           char	Alias path				*
 * 	*iret		int	Return code				*
 *				  -1 - alias not found			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 * I. Durham/GSC	 5/98   Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 3/01	Added parsing of storm name from alias	*
 ***********************************************************************/
{
int	i, ipos, ier;
char	alias[49];

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    if ( DtReadin == G_FALSE )  {
        ctb_dtrd ( iret );
    }

    cst_lcuc( alias_i, alias, &ier );

    /*
     * Remove the name of the storm/volcano from the alias.
     */
    cst_nocc ( alias, ':', 1, 0, &ipos, &ier );
    if  ( ier == 0 )  alias[ipos] = CHNULL;

    /*
     * Find a match for the alias.
     */
    for ( i = 0; i < DtTable.numtmpl; i++ )  {

	if ( strcmp( DtTable.info[i].alias, alias ) == 0 )  {

	    strcpy( path, DtTable.info[i].path );

	    return;

	}

    }

    *iret = -1;
    return;

}
