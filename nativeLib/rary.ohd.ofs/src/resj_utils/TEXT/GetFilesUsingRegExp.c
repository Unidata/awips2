/* ----------------------------------------------------------------------------
** GetFilesUsingRegExp - get a list of files given a regular expression
** ----------------------------------------------------------------------------
** Notes:	(1)	See StringMatchesRegExp for a list of valid regular
**			expression symbols.  See GetDirList for a list of
**			valid flag information.
**		(2)	If "regexps" has an absolute path, then files in the
**			directory are retrieved and compared to the regular
**			expression.
**		(3)	This routine does not currently follow the directory
**			tree.  It only gets files in the specified directory.
**			In other words, wildcards in directories will not work.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** basename	L	Base name of file in directory.
** dirname	L	Directory for file.
** filesdir	L	List of files in a directory.
** filesmatch	L	Files that match. 
** flag		I	Flag indicating search options.
** message	L	String for messages.
** i		L	Loop counter for regular expressions.
** j		L	Loop counter for files in directory.
** nfiles	O	Number of files returned.
** prefixdir	L	If specified, prepend to the regular expressions.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

char **GetFilesUsingRegExp (	char *prefixdir, char **regexps,
				unsigned int flag, int *nfiles )
{	char	basename[256], dirname[256], **filesdir,
		**filesmatch = (char **)NULL, fullpath[256], message[256],
		routine[] = "GetFilesUsingRegExp";
	int	i, j, nstrings;

	*nfiles = 0;

	/*
	** Make sure that information is OK...
	*/

	if ( regexps == (char **)NULL ) {
		PrintWarning ( 2, routine, "NULL regular expressions" );
		return (char **)NULL;
	}

	/*
	** Go through the list of regular expressions...
	*/
	i = 0;
	while ( regexps[i] != (char *)NULL ) {
		sprintf ( message, "Getting files for \"%s\"",
		regexps[i] );
		PrintDebug ( 15, routine, message );
		/*
		** First prepend the prefix if specified...
		*/
		if ( prefixdir && *prefixdir ) {
			sprintf ( fullpath, "%s%c%s", prefixdir,
			DIRSEP_CHAR, regexps[i] );
		}
		else {	strcpy ( fullpath, regexps[i] );
		}
		/*
		** Get the directory and file names...
		*/
		Dirname ( fullpath, dirname );
		Basename ( fullpath, basename );
		/*
		** Now get the files in the directory...
		*/
		filesdir =	GetDirList ( dirname, (char **)NULL,
				(char **)NULL, flag, &nstrings );
		if ( filesdir == (char **)NULL ) {
			++i;
			continue;
		}
		/*
		** Now see if any match the string...
		*/
		j = 0;
		while ( filesdir[j] != (char *)NULL ) {
			if ( StringMatchesRegExp(filesdir[j],basename) ) {
				filesmatch =	AddToStringList ( filesmatch,
						filesdir[j], &nstrings );
				++(*nfiles);
						
			}
			++j;
		}
		/*
		** Increment to go to next regular expression...
		*/
		FreeStringList ( filesdir );
		++i;
	}
	return filesmatch;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetFilesUsingRegExp.c,v $";
 static char rcs_id2[] = "$Id: GetFilesUsingRegExp.c,v 1.1 1999/02/18 15:16:46 dws Exp $";}
/*  ===================================================  */

}
