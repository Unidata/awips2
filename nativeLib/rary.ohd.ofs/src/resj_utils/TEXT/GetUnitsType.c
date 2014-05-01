/*------------------------------------------------------------------------------
** GetUnitsType - get units type given NWS abbreviation
**------------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
**------------------------------------------------------------------------------
** returns:	0 if no units found
**		TSUNITS-* if successful
**------------------------------------------------------------------------------
** Notes:	(1)	This routine depends on on the values in the DATAUNIT
**			file orignally supplied by the NWS.  Typical contents
**			for this file are shown in the GetConversion
**			notes.
**------------------------------------------------------------------------------
** History:
**
** 06 Sep 1996	Steven A. Malers, RTi	Split out of HMTS.c file.
** 07 Oct 1996	SAM, RTi		Include <string.h> to prototype string
**					functions.
**------------------------------------------------------------------------------
** Variable	I/O	Description
**
** dfile	L	Path to DATAUNIT file.
** fp		L	Pointer to file.
** i		L	Loop counter.
** key		L	Key to lock file for DATAUNIT file.
** string	L	String used to read in line from file.
** sysdir	L	Directory for NWSRFS system files.
** u		L	Units read from file.
** units	I	Source units.
** utype	O	Units type.
**------------------------------------------------------------------------------
*/

#include <string.h>

#include "ResJ.h"

int GetUnitsType ( char *units, int *utype )
{	FILE	*fp;
	int	i;
	char	dfile[256], key[256], routine[] = "GetUnitsType",
		string[128], sysdir[128], u[5];

	static struct {
		char	*abbr;
		int	tsid;
	} udata[] = {	{ "DIR ",	UNIT_DIR },
			{ "DLES",	UNIT_CONST },
			{ "E   ",	UNIT_ENERGY },
			{ "E/L2",	UNIT_ENERGY_PER_AREA },
			{ "E/T ",	UNIT_POWER },
			{ "L   ",	UNIT_LENGTH },
			{ "L/T ",	UNIT_SPEED },
			{ "L2  ",	UNIT_AREA },
			{ "L3  ",	UNIT_VOLUME },
			{ "L3/T",	UNIT_DISCHARGE },
			{ "PRES",	UNIT_PRESSURE },
			{ "TEMP",	UNIT_TEMP },
			{ "TIME",	UNIT_TIME },
			{ "", 0 } };

	*utype = 0;

	if ( GetDef("rfs_sys_dir", sysdir) ) {
		PrintWarning ( 1, routine,
		"Can't find definition for \"rfs_sys_dir\"" );
		return STATUS_FAILURE;
	}
	sprintf ( dfile, "%s/DATAUNIT", sysdir );
	if ( !(fp = fopen(dfile,"r")) )	return STATUS_FAILURE;
	while ( fgets(string,128,fp) ) {
		if ( string[0] == '*' ) {
			/*
			** A comment line...
			*/
			if ( !strncmp(string,"* END",5) )
				break;
		}
		else {	/*
			** A units line...
			*/
			sscanf ( &string[10], "%s", u );
			if ( !strcmp(u,units) ) {
				/*
				** Have found the correct units...
				*/
				i = 0;
				while ( *udata[i].abbr ) {
					if ( !strncmp(string,udata[i].abbr,4)) {
						fclose ( fp );
						*utype = udata[i].tsid;
						return STATUS_SUCCESS;
					}
					++i;
				}
			}
		}
	}
	fclose ( fp );
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetUnitsType.c,v $";
 static char rcs_id2[] = "$Id: GetUnitsType.c,v 1.1 1999/02/18 15:16:52 dws Exp $";}
/*  ===================================================  */

}
