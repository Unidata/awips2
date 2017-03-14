/*-----------------------------------------------------------------------------
** GetUnitsFormat -	Given a unit abbreviation return the precision and a
**			format string.
**-----------------------------------------------------------------------------
** History:
**
** 06-08-95	Peter T. Abplanalp, RTi		Created routine.
** 06-29-95	Steven A. Malers, PTA, RTi	Use default precision if units
**						are empty.
** 01 Dec 95	SAM, RTi			Add some debugs.
**-----------------------------------------------------------------------------
** Notes:
**	(1)	returns:	0 if successful
**				1 if unable to lock DATAUNIT file
**				2 if unable to read DATAUNIT file
**				7 general failure
**	(2)	This routine depends on the DATAUNITS file.  See the
**		GetNWSConversion* routines for details on this file.
**	(3)	The format variable is output in C format (i.e. %10.2f)
**-----------------------------------------------------------------------------
** Variable	I/O	Description
**
** dufile	L	Path to DATAUNIT file.
** format	O	The format string output by the routine.
** found	L	Switch to tell of a match was found for units.
** funits	L	Units gotten from the DATAUNITS file.
** group	L	The group associated with funits.
** fp		L	File pointer to DATUNIT file.
** name		L	Full name of funits.
** oneline	L	One line of data from DATAUNIT file.
** precision	O	Precision found in the DATAUNIT file.
** routine	L	Routine name.
** sysdir	L	NWSRFS system directory.
** units	I	Units for which a precision and format are wanted.
** width	I	Width of columns wanted.
**-----------------------------------------------------------------------------
*/

#include "ResJ.h"

int GetUnitsFormat ( char *units, int width, int *precision, char *format )
{	FILE	*fp;
	char	dufile[256], funits[256], group[256], message[256],
		name[256], oneline[256], routine[] = "GetUnitsFormat",
		sysdir[256];
	int	found = 0;

	/*
	** Make sure that we have units to look for...
	*/

	if ( !units || !*units ) {
		*precision	= 2;
		found		= 1;
		PrintWarning ( 2, routine,
		"No units abbreviation specified.  Using precision 2" );
	}

	if ( !found ) {
		/*
		** Get path to the DATAUNIT file
		*/
		if ( GetDef ( "rfs_sys_dir", sysdir ) ) {
			PrintWarning ( 5, routine,
			"Unable to get definition for \"rfs_sys_dir\"" );
			return STATUS_FAILURE;
		}
		/*
		** We are looking for DATAUNIT in the above directory...
		*/
		sprintf ( dufile, "%s/DATAUNIT", sysdir );
		/*
		** Lock the file for reading
		*/
		if ( !(fp = fopen(dufile,"r")) ) {
			sprintf ( message,
			"Unable to read units file \"%s\"", dufile );
			PrintWarning ( 5, routine, message );
			return STATUS_CANNOT_READ_FILE;
		}
		while ( fgets ( oneline, MAXC, fp ) ) {
			if ( oneline[0] == '*' ) {
				/*
				** Comment
				*/
				if ( !strncmp(oneline,"* END",5) )
					break;
			}
			else {
				/*
				** A line with info
				*/
				sscanf ( oneline, "%s %s %s", group, name,
				funits );
				if ( !strcmp ( units, funits ) ) {
					/*
					** Units match, get precision and
					** exit
					*/
					sscanf ( &oneline[52], "%d",
					&precision );
					found = 1;
					break;
				}
			}
		}
	}

	fclose( fp );
	/*
	** At this point we have either found the precision or not
	*/
	if ( found ) {
		if ( ! width )
			sprintf ( format, "%%.%df", precision );
		else
			sprintf ( format, "%%%d.%df", width, precision );
		return	STATUS_SUCCESS;
	}
	else {	return STATUS_FAILURE;
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetUnitsFormat.c,v $";
 static char rcs_id2[] = "$Id: GetUnitsFormat.c,v 1.1 1999/02/18 15:16:51 dws Exp $";}
/*  ===================================================  */

}
