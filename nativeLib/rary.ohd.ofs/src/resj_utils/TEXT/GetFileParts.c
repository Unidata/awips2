/* ----------------------------------------------------------------------------
** GetFileParts - get parts of path from a path
** ----------------------------------------------------------------------------
** depends on:	<string.h>
** returns:	0 always
** ----------------------------------------------------------------------------
** history:
**
** 09-16-92	Steven A. Malers	Created function.
** 12-06-93	SAM, RTi		Added extension information.
** 05-16-95	SAM, RTi		Fixed bug where "base" was not getting
**					set if there is no file extension.
**					Reworked variable list.
** 13 Oct 1995	SAM, RTi		Add some debug messages.  Change so that
**					if path ends in /, the leading path
**					is everything in front of the / and the
**					file is empty.  There was a bug where
**					/ was always getting stuck on "lead".
** ----------------------------------------------------------------------------
** Notes:	(1)	If the filename has no leading path, then "lead" will
**			be null.
**		(2)	The leading path DOES NOT include the final directory
**			dividing character, e.g. ../dir1/dir2/file would give
**			"lead" = "../dir1/dir2".  The only exception to this is
**			when "path" ends in "dchar", i.e. "path" is a directory
**			only and does not contain a filename.
**		(3)	The filename does NOT include any leading characters
**			such as "./" unless these are originally included in
**			"path".
**		(4)	The file extension will be determined.  This is the
**			the string after "echar".  In other words, if the
**			filename is "myfile.c", then "echar" should be set to
**			".", and "extension" will be set to "c" on return.  If
**			"echar" is not set, then a default value will be used.
**			("echar" is defined as a string so that this routine
**			can be modified to accept more than one extension
**			character or perhaps a regular expression.)
** ----------------------------------------------------------------------------
** Variables	I/O	Description
**
** base		O	Filename base (filename before extension).
** dflag	L	Indicates whether there is a directory separator in the
**			path.
** echars	I	Characters used to separate file extension
**			(from calling routine).
** echars2	L	Characters used to separate file extension
**			(actual used).
** file		O	Name of file.
** flen		L	Length of filename.
** lead		O	Leading path to file.
** llen		L	Length of leading path.
** message	L	String for messages.
** path		I	Full path to file.
** plen		L	Total length of path not counting trailing NULL.
** ppt		L	Pointer to character in "path".
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

int GetFileParts (	char *path, char *echars, char *lead, char *file,
			char *base, char *extension )
{	char	echars2[256], message[256], *ppt = path,
		routine[] = "GetFileParts";
	int	dflag = 0, len, llen = 0, plen = 0;

	*lead = '\0';
	*file = '\0';

	/*
	** Get to the end of the string.  If a directory separator is
	** encountered, set flag to indicate that path is for a directory...
	*/

	sprintf ( message, "Getting file parts for \"%s\"", path );
	PrintDebug ( 30, routine, message );

	while ( *ppt ) {
		++plen;
		if ( *ppt == DIRSEP_CHAR ) {
			dflag = 1;
		}
		++ppt;
	}
	llen = plen;
	--ppt;		/* now at last character before NULL */

	/*
	** We are at the end of the string.  Check for delimiter at end...
	*/

	if (	(*path == DIRSEP_CHAR) && (strlen(path) == 1) ) {
		/*
		** Root directory...
		*/
		strcpy ( lead, DIRSEP_CHAR_STRING );
	}
	else if ( *ppt == DIRSEP_CHAR ) {
		sprintf ( message, "%c at end - file is empty",
		DIRSEP_CHAR );
		PrintDebug ( 30, routine, message );
		--llen;
		file[0] = '\0';
		strncpy ( lead, path, llen );
		lead[llen] = '\0';
	}
	else if ( !dflag ) {
		/*
		** No directory separators... path length is the same as the
		** file length so copy the path into the file...
		*/
		PrintDebug ( 30, routine,
		"No directory separators - entire string is file" );
		strcpy ( file, path );
	}
	else {	/*
		** Definitely have a lead and file name.  Back down to directory
		** separator...
		*/
		PrintDebug ( 30, routine,
		"Directory separators - getting lead and file" );
		while ( *ppt != DIRSEP_CHAR ) {
			--ppt;
			--llen;
		}
		/*
		** *ppt is now at a directory separator...
		*/
		--llen;		/* need to decrement one more */
		strncpy ( lead, path, llen );
		lead[llen] = '\0';
		/*
		** If the first character is a directory separator and the
		** lead is empty, set the lead as the directory separator.  This
		** would be the top of the system...
		*/
		if ( (lead[0] == '\0') && (*path == DIRSEP_CHAR) ) {
			strcpy ( lead, DIRSEP_CHAR_STRING );
		}
		/*
		** Increment to be after the directory separator...
		*/
		++ppt;
		strncpy ( file, ppt, (plen - llen) );
		file[plen - llen] = '\0';
	}

	/*
	** Get the file extension...
	*/
	if ( *echars )
		strcpy ( echars2, echars );
	else	strcpy ( echars2, FILEEXT_DELIM );
	base[0]		= '\0';
	extension[0]	= '\0';
	len		= strlen ( file );
	if ( len > 0 ) {
		ppt = file;
		while ( *ppt )	++ppt;
		--ppt;
		while ( len ) {
			if ( *ppt == echars2[0] ) {
				++ppt;
				strcpy ( extension, ppt );
				strncpy ( base, file, (len - 1) );
				base[len - 1] = '\0';
				break;
			}
			--len;
			--ppt;
		}
	}
	/*
	** If base is null, then there is no file extension.  Use the entire
	** file as the base...
	*/
	if ( !*base ) {
		strcpy ( base, file );
	}
	sprintf ( message, "lead=\"%s\" file=\"%s\"", lead, file );
	PrintDebug ( 30, routine, message );
	sprintf ( message, "base=\"%s\", ext=\"%s\"", base, extension );
	PrintDebug ( 30, routine, message );
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetFileParts.c,v $";
 static char rcs_id2[] = "$Id: GetFileParts.c,v 1.1 1999/02/18 15:16:45 dws Exp $";}
/*  ===================================================  */

}
