#include "gui.h"


#define MAP_TBL      "mapinfo.nwx"
#define TABLE_DIR    "nwx"

nwxtbl_t *nwxTable;

/************************************************************************
 * nwxtbl.c                                                             *
 *                                                                      *
 * This module deals with nwx tables.         				*
 *                                                                      *
 * CONTENTS:                                                            *
 *      nwxtbl_init()    read the nwx tables and initialize the         *
 *				structure.              		*
 *      nwxtbl_sdtyp()   search for the data type in the structure.     *
 *      nwxtbl_getstns() read related station table and save in the     *
 *				structure.    				*
 *      _cmp_bulls()     sort bulletin structure based on station ID	*
 ***********************************************************************/

/*=====================================================================*/

int nwxtbl_init ( void )
/************************************************************************
 * nwxtbl_init								*
 *									*
 * This routine will read the data type table and map table into        *
 * the global nwxTable structure.					*
 *									*
 * int nwxtbl_init ( )							*
 *									*
 * Output parameters:							*
 *	nwxtbl_init		int	 0 -- successful		*
 *		       -1 -- table reading error			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * S. Jacobs/NMC	10/94		Changed NWX_TABLES to		*
 *						NAWIPS_TABLES/nwx	*
 * C. Lin/EAI		 8/95						*
 * S. Chiswell/Unidata	 1/02	Modified for use with NSHARP		*
 * T. Piper/SAIC	01/04	removed tbldir and tblfile              *
 ***********************************************************************/
{
int	maxnum, iret;

/*---------------------------------------------------------------------*/
	iret = G_NORMAL;

	/*
	 * allocate space for nwxTable
	 */
	nwxTable = (nwxtbl_t *)malloc(sizeof(nwxtbl_t));

	/*
 	 * Set the map type table name, and maximum array size.
 	 */
	maxnum = MAXTYP;

	/*
 	 * Read the contents of the map table into the structure.
 	 */
	ctb_rmtyp( MAP_TBL, TABLE_DIR, &maxnum, 
		&(nwxTable->nmap), nwxTable->map_info, &iret );
	if ( iret != 0 ) return( -1 );

	return( 0 );
}

