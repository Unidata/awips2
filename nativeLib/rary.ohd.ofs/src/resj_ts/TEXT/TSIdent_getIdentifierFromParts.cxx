//------------------------------------------------------------------------------
// TSIdent.getIdentifierFromParts - create identifier string from its parts
//------------------------------------------------------------------------------
// Notes:	(1)	When this routine is called from the TSIdent::ident()
//			function, some fields will be NULL.  In all likelyhood,
//			this routine will be called several times and when all
//			the data are initialized the identifier will be final.
//			So, to make sure that everything works at all stages,
//			deal with the NULLs by ignoring them.
//------------------------------------------------------------------------------
// History:
// 
// 17 Sep 1997	Steven A. Malers, RTi	Create function - model after
//					GetTSIDFromParts.
// 07 Jan 1998	SAM, RTi		Minor changes to bring in line with
//					Java.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: getIdentifierFromParts (	char *full_location, char *full_source,
					char *type, char *interval_string,
					char *scenario, char *full_identifier )
{	char	routine[] = "TSIdent.getIdentifierFromParts";

	full_identifier[0] = '\0';
	if ( full_location ) {
		strcpy ( full_identifier, full_location );
	}
	strcat ( full_identifier, SEPARATOR );
	if ( full_source ) {
		strcat ( full_identifier, full_source );
	}
	strcat ( full_identifier, SEPARATOR );
	if ( type ) {
		strcat ( full_identifier, type );
	}
	strcat ( full_identifier, SEPARATOR );
	if ( interval_string ) {
		strcat ( full_identifier, interval_string );
	}
	if ( scenario ) {
		if ( scenario[0] != '\0' ) {
			strcat ( full_identifier, SEPARATOR );
			strcat ( full_identifier, scenario );
		}
	}
	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_getIdentifierFromParts.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_getIdentifierFromParts.cxx,v 1.2 2000/05/19 13:06:33 dws Exp $";}
/*  ===================================================  */

}
