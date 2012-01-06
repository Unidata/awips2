/* ----------------------------------------------------------------------------
** HMGetDataTypeInfo - get information about a data type
** ----------------------------------------------------------------------------
** Notes:	(1)	The initial version of this routine serves mainly to
**			validate a data type by trying to read its information
**			out of the DATATYPE file.  The data type description
**			is also returned.  A later version may actually
**			use the information.  The file looks like:
**
AESC SNOW COVER          AREAL EXTENT        OBSERVED                   00000030
AESC DLES A    INST BOTH  1                                             00000040
AESC FCST PCTD YES  PP    0                                             00000050
AESC CALB PCT  22                                                       00000060
*                                                                       00000070
AIAI ANTECEDENT INDEX  .  .                                             00000080
AIAI DLES A/P  INST BOTH  1                                             00000090
AIAI FCST REAL NO   FC    0                                             00000100
AIAI CALB                                                               00000110
*                                                                       00000120
** ----------------------------------------------------------------------------
** History:
**
** 03 Nov 1995	Steven A. Malers, RTi		Add "units" and "record_method"
**						to return information.
** 14 Dec 1995	SAM, RTi			Add "units_dim" to output.
** ** *** 2003	JRV, RTi	Initialized fdtype_prev.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** dfile	L	Path to the DATATYPE file.
** dtype	I	Requested data type abbreviation.
** fdtype	L	Data type abbreviation read from file.
** fdtype_prev	L	Previous "fdtype" read.
** i		L	Position holder in string.
** key		L	Key for lock file.
** nblank	L	Number of blanks in a row.
** record_method L	Indicates whether data are INST (instantaneous),
**			ACCM (accumulated), or MEAN (mean).
** routine	L	Name of this routine.
** spt		L	Pointer to "string".
** string	L	String read from the file.
** type_line_count L	Line count for a data type.
** units	O	Default units for the data type.
** units_dim	O	Dimension of "units".
** words	O	Word description of units (take the words from columns
**			6-72 of the file, replace multiple spaces with a comma,
**			ignore periods).
** wpt		L	Pointer to "words".
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

int GetDataTypeInfo ( char *dtype, char *words, char *units, char *units_dim,
			char *record_method )

{	char	dfile[MAXC], fdtype[MAXC], fdtype_prev[MAXC], key[MAXC],
		message[80], routine[] = "GetDataTypeInfo", *spt, string[MAXC],
		sysdir[256], *wpt;
	int	i, nblank, type_line_count;
	FILE	*fp;

	*units = '\0';
	*record_method = '\0';
	*words = '\0';
	/* Initialize fdtype_prev with an "impossible" value.  Expected format
	 * expects either left justified information.  Therefore, we put some
	 * junk prefixed by a number of spaces, tabs, etc. 
	*/
	sprintf( fdtype_prev,"  \tX-JUNK_INITIALIZATION-X\t  1234567890~|<>&" );

	/*
	** Get the path to the DATATYPE file...
	*/
	if ( GetDef("rfs_sys_dir",sysdir) ) {
		PrintWarning ( 2, routine,
		"Unable to get value for \"rfs_sys_dir\"" );
		return STATUS_FAILURE;
	}
	sprintf ( dfile, "%s/DATATYPE", sysdir );

	if ( !(fp = fopen(dfile,"r")) )	return STATUS_FAILURE;
	type_line_count = 0;
	while ( fgets(string,128,fp) ) {
		if ( string[0] == '*' ) {
			/*
			** A comment line...
			*/
			if ( !strncmp(string,"* END",5) )
				break;
		}
		else {	/*
			** A line with data type information (always come in
			** groups separated by comments)...
			*/
			sscanf ( string, "%s", fdtype );
			if ( !strcmp(fdtype, fdtype_prev) ) {
				/*
				** More information about a data type...
				*/
				++type_line_count;
			}
			else {	/*
				** First line for this data type...
				*/
				type_line_count = 1;
			}
			sprintf ( message,
			"Looking for \"%s\".  Read data type \"%s\"",
			dtype, fdtype );
			PrintDebug ( 20, routine, message );
			if ( strcmp(dtype,fdtype) ) {
				/*
				** Not interested in this data type...
				*/
				strcpy ( fdtype_prev, fdtype );
				continue;
			}
			if ( type_line_count == 1 ) {
				/*
				** Get the description...
				*/
				PrintDebug ( 20, routine,
				"Getting description" );
				nblank	= 0;
				words[0] = '\0';
				for (	i = 6, spt = &string[i - 1], wpt = &words[0];
					(*spt && (*spt != '\n') && i <= 72);
					++i, ++spt ) {
					/*
					** Process the description...
					*/
					if ( *spt == '.' ) {
						/*
						** Ignore .
						*/
						continue;
					}
					else if ( *spt == ' ' ) {
						/*
						** Count blanks...
						*/
						++nblank;
						continue;
					}
					else {	/*
						** Non-blank.  Interpret blanks
						** and concatenate character to
						** "words"...
						*/
						if ( nblank == 1 ) {
							*wpt = ' ';
							++wpt;
						}
						else if ( nblank > 1 ) {
							*wpt = ',';
							++wpt;
							*wpt = ' ';
							++wpt;
						}
						*wpt = *spt;
						++wpt;	/* spt incremented in for */
						nblank	= 0;
					}
				}
			}
			else if ( type_line_count == 2 ) {
				/*
				** Process the record type...
				*/
				sscanf ( &string[15], "%s", record_method );
				sprintf ( message,
				"Record method for \"%s\" are \"%s\"", dtype,
				record_method );
				PrintDebug ( 20, routine, message );
			}
			else if ( type_line_count == 3 ) {
				/*
				** Process the units...
				*/
				sscanf ( &string[10], "%s", units );
				sprintf ( message,
				"Units for \"%s\" are \"%s\"", dtype, units );
				PrintDebug ( 20, routine, message );
			}
			/*
			** Break out if we have found the item of interest...
			*/
			if (	!strcmp(dtype,fdtype) &&
				(type_line_count >= 3) ) {
				*wpt = '\0';
				fclose ( fp );
				/*
				** As a convenience, get the units dimension
				** here...
				*/
				GetUnitsDimension ( units, units_dim );
				return STATUS_SUCCESS;
			}
			/*
			** Save the data type...
			*/
			strcpy ( fdtype_prev, fdtype );
		}
	}
	if ( fp )
		fclose ( fp );
	return STATUS_FAILURE;
}

/* ----------------------------------------------------------------------------
** GetUnitsDimension - get the dimension abbreviation for units
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine reads the DATAUNIT file to get the
**			dimension for a given units type.  For example, "L"
**			is returned for units "IN".
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** dfile	L	Path to DATAUNIT file.
** dim		O	Dimension for units.
** fp		L	Pointer to DATAUNIT file.
** funits	L	"units" as read from file.
** string	L	Line read from DATAUNIT file.
** units	I	Units.
** ----------------------------------------------------------------------------
*/
int GetUnitsDimension ( char *units, char *dim )
{	char	dfile[MAXC], funits[MAXC], key[MAXC], string[MAXC],
		sysdir[MAXC];
	FILE	*fp;

	*dim = '\0';
	/*
	** Get the path to the DATAUNIT file...
	*/
	if ( GetDef("rfs_sys_dir",sysdir) )
		return STATUS_FAILURE;
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
			** A line with conversion factors...
			*/
			sscanf ( &string[10], "%s", funits );
			if ( !strcmp(units,funits) ) {
				/*
				** Found the correct units...
				*/
				sscanf ( string, "%s", dim );
				fclose ( fp );
				return STATUS_SUCCESS;
			}
		}
	}
	fclose ( fp );
	return STATUS_FAILURE;
}
/* ----------------------------------------------------------------------------
** HMFileReadable - check to see whether a file is readable
** ----------------------------------------------------------------------------
** version:	1.1
** depends on:	<stdio.h>
** returns:	1 if file is readable, 0 if not
** ----------------------------------------------------------------------------
** history:
**
** 1.0 (?)		Steven A. Malers	Created function.
** 1.1 (9-11-92)	SAM, RTi		Standardized header.
** ----------------------------------------------------------------------------
** notes:	none
** ----------------------------------------------------------------------------
** variables:
**
** filename	.... name of file that is to be checked
** fp		.... pointer to the file
** ----------------------------------------------------------------------------
*/
int FileReadable ( char *filename )
{	FILE	*fp;

	if ( (fp = fopen(filename, "r") ) == NULL )  return 0;
	else {	fclose ( fp );
		return 1;
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetDataTypeInfo.c,v $";
 static char rcs_id2[] = "$Id: GetDataTypeInfo.c,v 1.2 2004/09/08 17:14:23 hank Exp $";}
/*  ===================================================  */

}
