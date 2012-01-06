/* ----------------------------------------------------------------------------
** GetDirList - get list of files and directories under a directory
** ----------------------------------------------------------------------------
** Notes:	(1)	This code for this routine is very much platform
**			dependent, although UNIX versions seem to be somewhat
**			standard.
**		(2)	This routine gets a list of files or directories in
**			a given directory.
**		(3)	The memory allocated by this routine should be freed
**			with the "FreeStringList" routine.
**		(4)	The files and directories are alphabetized totally on
**			the basis of name (i.e., directories are not listed
**			ahead of files).
** ----------------------------------------------------------------------------
** History:
**
** 06-14-95	Steven A. Malers, RTi	Update documentation.  Fix so that
**					"stat" uses full path.
** 06-21-95	SAM, RTi		Update to have an include list that is
**					the opposite of the ignore list.
** 07-19-95	SAM, RTi		Fix so that the "doinclude" flag gets
**					recognized at the end of the function.
** 28 May 1996	SAM, RTi		Sort retrieved files alphabetically.
** 07 Jan 97	Matthew J. Rutherford, RTi
**					Bump up some debug and warning levels.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** dent		L	Pointer to directory entry.
** dir		I	Directory to list.
** doignore	L	Indicates whether file is to be ignored.
** doinclude	L	Indicates whether file is to be included.
** dpt		L	Pointer to open directory stream.
** filestat	L	File status information.
** flag		I	Flag indicating options.
**			DIR_NOCURRENT	-> ignore "." and ".."
**			DIR_NOHIDDEN	-> ignore hidden files
**			DIR_NOFILES	-> ignore files
**			DIR_NODIRS	-> ignore directories
** i		L	Counter for files.
** ignore_list	I	Files and directories to ignore (e.g. "README").
** include_list I	Files and directories to include.  If specified, only
**			these files are returned.
** list		O	List of requested files from directory.
** message	G	String for messages.
** nlist	O	Number of items in returned list.
** nlist2	L	Number of items in string list while adding.
** path		L	Full path to file.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

char **GetDirList (	char *dir, char **ignore_list, char **include_list,
			unsigned int flag, int *nlist )
{	int		doignore, doinclude, i, nlist2;
	char		**list = (char **)NULL, message[256], path[256],
			routine[] = "GetDirList";
#ifdef UNIX
	struct dirent	*dent;
	DIR		*dpt;
	struct stat	filestat;

	doinclude	= 0;
	doignore	= 0;
	*nlist		= 0;

	if (	(ignore_list != (char **)NULL) &&
		(include_list != (char **)NULL) ) {
		PrintWarning ( 2, routine,
		"Cannot specify ignore and include files at the same time" );
		return (char **)NULL;
	}

	/*
	** Debug for flag...
	*/
	if ( flag & DIR_NOHIDDEN )
		PrintDebug ( 15, routine, "Not returning hidden files" );
	if ( flag & DIR_NOCURRENT )
		PrintDebug ( 15, routine, "Not returning current directory" );
	if ( flag & DIR_NOFILES )
		PrintDebug ( 15, routine,
		"Not returning files (unless in include list)" );
	if ( flag & DIR_NODIRS )
		PrintDebug ( 15, routine,
		"Not returning directories (unless in include list)" );
	/*
	** Print ignore/include lists...
	*/
	if ( include_list != (char **)NULL ) {
		i = 0;
		while ( include_list[i] != (char *)NULL ) {
			sprintf ( message, "Include list [%d] = \"%s\"",
			i, include_list[i] );
			PrintDebug ( 15, routine, message );
			++i;
		}
	}
	if ( ignore_list != (char **)NULL ) {
		i = 0;
		while ( ignore_list[i] != (char *)NULL ) {
			sprintf ( message, "Ignore list [%d] = \"%s\"",
			i, ignore_list[i] );
			PrintDebug ( 15, routine, message );
			++i;
		}
	}
	/*
	** Open directory...
	*/
	dpt = opendir ( dir );
	if ( !dpt ) {
		sprintf ( message, "Unable to open \"%s\"", dir );
		PrintWarning ( 5, routine, message );
		return (char **)NULL;
	}
	/*
	** Get entries from directory...
	*/
	while ( (dent = readdir(dpt)) ) {
		/*
		** Determine item is a file or a directory.  Note that at this
		** point the directory entries (dent->d_name) do not have the
		** leading path so we have to cat them together...
		*/
		sprintf ( path, "%s/%s", dir, dent->d_name );
		if ( stat(path, &filestat ) ) {
			sprintf ( message, "Unable to get stats for \"%s\"",
			path );
			PrintWarning ( 5, routine, message );
			continue;
		}
		if (	!(filestat.st_mode & S_IFDIR) &&
			(include_list != (char **)NULL) ) {
			/*
			** Make absolutely sure that the files are included...
			*/
			i		= 0;
			doinclude	= 0;
			while ( include_list[i] != (char *)NULL ) {
				if ( !strcmp(include_list[i], dent->d_name) ) {
					doinclude = 1;
					sprintf ( message,
					"\"%s\" found in include list - include absolutely",
					include_list[i] );
					PrintDebug ( 15, routine, message );
					break;
				}
				++i;
			}
			if ( !doinclude ) {
				/*
				** The file is not to be included in the list...
				*/
				continue;
			}
		}
		if (	!doinclude && (flag & DIR_NOHIDDEN) ) {
			/*
			** Ignore hidden files that have a . as the first
			** character and are not the current and parent
			** directories.  Only do this if the file is not part
			** of the include list (it is impossible that it is,
			** given the checks above).
			*/
			if (	(dent->d_name[0] == '.') &&
				strcmp(dent->d_name, ".") &&
				strcmp(dent->d_name, "..") ) {
				continue;
			}
		}
		if (	!doinclude && (flag & DIR_NOCURRENT) ) {
			/*
			** Ignore "." and ".." entries.  Only do this if the
			** file is not part of the include list (it is
			** impossible that it is, given the checks above).
			*/
			if (	!strcmp(".", dent->d_name) ||
				!strcmp("..", dent->d_name) ) {
				continue;
			}
		}
		if ( ignore_list != (char **)NULL ) {
			/*
			** Have some files to ignore (don't return them in the
			** list).  Will not have this case if the include_list
			** list has been given.
			*/
			i		= 0;
			doignore	= 0;
			while ( ignore_list[i] != (char *)NULL ) {
				if ( !strcmp(ignore_list[i], dent->d_name) ) {
					doignore = 1;
					break;
				}
				++i;
			}
			if ( doignore )		continue;
		}
		/*
		** Entry has passed through.  Add it to the list...
		*/
		if ( filestat.st_mode & S_IFDIR ) {
			if ( flag & DIR_NODIRS ) {
				/*
				** This is a directory and we do not want
				** directories...
				*/
				sprintf ( message,
				"\"%s\" is a directory (mode %u).  Ignoring",
				path, filestat.st_mode );
				PrintDebug ( 15, routine, message );
				continue;
			}
		}
		else if ( !doinclude && (flag & DIR_NOFILES) ) {
			/*
			** This is a regular file (or symbolic link) and we
			** do not want files...
			*/
			sprintf ( message,
			"\"%s\" is a file (mode %u).  Ignoring", path,
			filestat.st_mode );
			PrintDebug ( 15, routine, message );
			continue;
		}
		/*
		** Now add the entry to the list...
		*/
		list = AddToStringList ( list, dent->d_name, &nlist2 );
		if ( list == (char **)NULL ) {
			sprintf ( message,
			"Unable to malloc for dir list [%d] (%d chars)",
			*nlist, strlen(dent->d_name) );
			PrintWarning ( 2, routine, message );
			return (char **)NULL;
		}
		++(*nlist);
	}
	/*
	** Close directory...
	*/
	closedir ( dpt );
#else
	PrintWarning ( 1, routine, "GetDirList not implemented for PC" );
#endif
	return SortStringList ( list, 0 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetDirList.c,v $";
 static char rcs_id2[] = "$Id: GetDirList.c,v 1.1 1999/02/18 15:16:44 dws Exp $";}
/*  ===================================================  */

}
