#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"
#include "dbcmn.h"

extern	Data_t		DtTable;
extern	int		DtReadin;

void ctb_dtget ( char *alias_i, char *path, char *template, 
		 int *catgry, int *subcat, int *nframe, int *range, 
		 int *intrvl, int *ionoff, int *hrsbfr, int *mnsbfr,
		 int *hraftr, int *mnaftr, int *mstrct,
                 int *dtmmtch, int *iret )
/************************************************************************
 * ctb_dtget								*
 *									*
 * This function returns all parameters associated with a data alias.	*
 *									*
 * ctb_dtget ( alias_i, path, template, catgry, subcat, nframe, range,	*
 *	       intrvl, ionoff, hrsbfr, mnsbfr, hraftr, mnaftr, mstrct,	*
 *	       dtmmtch, iret )						*
 *									*
 * Input parameters:							*
 *      *alias_i	char	Alias name				*
 *									*
 * Output parameters:							*
 *      *path           char	Alias path				*
 *      *template       char	Alias template				*
 *      *catgry         int 	Alias category				*
 *      *subcat         int 	Alias subcategory			*
 *      *nframe         int 	Alias number of frames			*
 *      *range          int 	Alias time range			*
 *      *intrvl         int 	Alias time interval			*
 *      *ionoff         int 	Alias bin hours OFF(0) / ON (1) flag	*
 *	*hrsbfr		int	Alias bin hour before current time	*
 *	*mnsbfr		int	Alias bin minutes before current time	*
 *	*hraftr		int	Alias bin hour after current time	*
 *	*mnaftr		int	Alias bin minutes after current time	*
 *      *mstrct         int 	Alias most recent only OFF(0)/ON(1) flag*
 *	*dtmmtch	int	Alias time match scheme if dominant src	*
 * 	*iret		int	Return code				*
 *				  -1 - alias not found			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 8/99	Changed format of data.tbl		*
 * S. Jacobs/NCEP	 3/01	Added parsing of storm name from alias	*
 * T. Lee/SAIC		 9/04	Added bin hours				*
 * R. Tian/SAIC         11/04   Added processing of possible storm name *
 * S. Jacobs/NCEP	12/04	Increased size of alias to 256		*
 * m.gamazaychikov/SAIC	12/04	Added ionoff to the calling sequence	*
 * m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
 * m.gamazaychikov/SAIC	04/06	Added dtmmtch to the calling sequence	*
 * F. J. Yen/NCEP	04/08	Added mnsbfr, mnaftr, and mstrct to CS	*
 ***********************************************************************/
{
int	i, ipos, ic, ier;
char	alias[256], name[49], templ[MXTMPL];

char    diagMessage[720];
int     ierm;
/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    sprintf (diagMessage, "%s%s", "before db_a2dtget alias=", alias_i);
    db_msgcave ("ctb_dtget", "debug", diagMessage, &ierm);

    db_a2dtget (alias_i, path, template, &ier );

    sprintf (diagMessage, "%s%s%s%s", "after db_a2dtget path=", path, " template=", template);
    db_msgcave ("ctb_dtget", "debug", diagMessage, &ierm);

    if ( ier == 0 ) return;

    if ( DtReadin == G_FALSE )  {
        ctb_dtrd ( iret );
    }

    cst_lcuc( alias_i, alias, &ier );

    /*
     * Remove the name of the storm/volcano from the alias.
     */
    cst_nocc ( alias, ':', 1, 0, &ipos, &ic );
    if  ( ic == 0 )  {
	strcpy ( name, &alias[ipos+1] );
        alias[ipos] = CHNULL;
    }

    /*
     * Find a match for the alias.
     */
    for ( i = 0; i < DtTable.numtmpl; i++ )  {

	if ( strcmp( DtTable.info[i].alias, alias ) == 0 )  {

	    strcpy( path, DtTable.info[i].path );
	    *catgry = DtTable.info[i].catgry;
	    *subcat = DtTable.info[i].subcat;
	    *nframe = DtTable.info[i].nframe;
	    *range  = DtTable.info[i].range;
	    *intrvl = DtTable.info[i].intrvl;
	    *ionoff = DtTable.info[i].ionoff;
	    *hrsbfr = DtTable.info[i].hrsbfr;
	    *mnsbfr = DtTable.info[i].mnsbfr;
	    *hraftr = DtTable.info[i].hraftr;
	    *mnaftr = DtTable.info[i].mnaftr;
	    *mstrct = DtTable.info[i].mstrct;
	    *dtmmtch = DtTable.info[i].domtmmtch;

            /*
             * If the user has input a storm/volcano name, then
             * replace the "*" in the template with the name.
             * The name should be in all lower case, since the
             * user cannot be expected to input the correct case.
             */
            if ( ic == 0 ) {
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
