#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Data_t		DtTable;
extern	int		DtReadin;

void ctb_dttmpl ( char *alias_i, char *template, int *iret )
/************************************************************************
 * ctb_dttmpl								*
 *									*
 * This function returns the template associated with a data alias.     *
 *									*
 * ctb_dttmpl ( alias_i, template, iret )				*
 *									*
 * Input parameters:							*
 *      *alias_i	char	Alias name				*
 *									*
 * Output parameters:							*
 *      *template       char	Alias template				*
 * 	*iret		int	Return code				*
 *				  -1 - alias not found			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 3/01	Added parsing of storm name from alias	*
 * R. Tian/SAIC		11/04	Added processing of possible storm name	*
 * m.gamazaychikov/SAIC	01/06	Changed templ string length to MXTMPL	*
 ***********************************************************************/
{
int	i, ipos, ier;
char	alias[49], name[49], templ[MXTMPL];

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
    if  ( ier == 0 )  {
	strcpy ( name, &alias[ipos+1] );
        alias[ipos] = CHNULL;
    }

    /*
     * Find a match for the alias.
     */
    for ( i = 0; i < DtTable.numtmpl; i++ )  {

	if ( strcmp( DtTable.info[i].alias, alias ) == 0 )  {

	    /*
             * If the user has input a storm/volcano name, then
             * replace the "*" in the template with the name.
             * The name should be in all lower case, since the
             * user cannot be expected to input the correct case.
	     */
	    if ( ier == 0 ) {
		cst_uclc ( name, name, &ier );
	        strcpy( templ, DtTable.info[i].template );
	        cst_rpst ( templ, "*", name, template, &ier );
	    } else {
	        strcpy( template, DtTable.info[i].template );
	    }

	    return;

	}

    }

    *iret = -1;
    return;

}
