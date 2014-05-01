// ----------------------------------------------------------------------------
// TSUtil.findTSFile - find a time series file using a path to the file.
// ----------------------------------------------------------------------------
// Notes:	(1)	This routine searches for a time series data file using
//			the following search order:
//
//				I.	If the time series identifier has a
//					scenario that is a file in the current
//					directory or the full path to a file,
//					use it.
//				II.	If the TS ID has a scenario but the file
//					is not in the current directory, use
//					the path information to
//					search.  If a file is found, use it.
//				III.	If the scenario is blank, use the
//					information in the TS ID to form
//					reasonable file names and search the
//					path for a file.  If one is found use
//					it.
//				IV.	If unable to find a file, return non-
//					zero.
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Write initial version.
// 10 Mar 1997	SAM, RTi		Use semi-colons for the path so that
//					this code will work on the PC and UNIX.
// 22 Sep 1997	SAM, RTi		Fold in code from DSSFindTSFile in DSS
//					library.
// 10 Jan 1998	SAM, RTi		Move from DSSApp library to here.
// ----------------------------------------------------------------------------
// Variable	I/O	Description
//
// fulltsdatadirs L	Fill paths to possible time series files.
// i		L	Loop counter for directories.
// nfulltsdatadirs L	Number of "fulltsdatadirs".
// ntsdatadirs	G	Number of "tsdatadirs".
// scenario	L	Scenario part of time series identifier.  Used to
//			specify a file name.
// string	L	Generic string.
// tsdatapath	L	Time series data directory path as a string.
// tsdatadirs	G	Time series data directory path as string list.
// tsfile	O	Full path to time series file.
// tsid_string0	I	Time series identifier string (original).
// tsid_string	L	Time series identifier string.
// ----------------------------------------------------------------------------

#include "TSUtil.h"

char TSUtilTsfile[256];  //cfan 

char * TSUtil :: findTSFile ( char *tsid_string0, char *tsdatapath0 )
{	char		routine[] = "TSUtil.findTSFile", scenario[256],
			string[256], tsdatapath[1000], tsid_string[256];
	int		i, nfulltsdatadirs = 0;
	static char	**tsdatadirs = (char **)NULL;
	static int	ntsdatadirs = 0;
//cfan	char		tsfile[256];

	// Make sure we have a non-NULL identifier...

	if ( !tsid_string0 ) {
		PrintWarning ( 1, routine, "Time series identifier is NULL" );
		return (char *)NULL;
	}
	else if ( !tsid_string0[0] ) {
		PrintWarning ( 1, routine,
		"Time series identifier is empty" );
		return (char *)NULL;
	}

	// Make sure that the path is non-NULL...

	if ( !tsdatapath0 ) {
		PrintWarning ( 1, routine,
		"Time series path is NULL.  Using \".\"" );
		strcpy ( tsdatapath, "." );
	}
	else if ( !tsdatapath0[0] ) {
		PrintWarning ( 1, routine,
		"Time series path is empty.  Using \".\"" );
		strcpy ( tsdatapath, "." );
	}

	strcpy ( tsid_string, tsid_string0 );
	PrintDebug ( 1, routine,
	"Trying to find time series for ID \"%s\"", tsid_string );

	// If the paths are NULL, initialize so we can use...
	// This will always be the case at this time because of the way that
	// the code was split out of DSSApp.

	if ( tsdatadirs == (char **)NULL ) {
		PrintDebug ( 1, routine,
		"Trying to initialize global path data..." );
/*
		if ( getDef("DSS_TSDATA_PATH", tsdatapath) ) {
			// Trouble, use the default...
			PrintWarning ( 1, routine,
			"Unable to resolve DSS_TSDATA_PATH.  Using \".\"" );
			tsdatadirs = AddToStringList ( tsdatadirs, ".",
			&ntsdatadirs );
		}
		else {
*/
			// We got a path.  Split it into its parts...
			tsdatadirs = BreakStringList ( tsdatapath, "\t; ",
			DELIM_SKIP_BLANKS, &ntsdatadirs );
			if ( !ntsdatadirs ) {
				// Trouble, use the default...
				PrintWarning ( 1, routine,
				"DSS_TSDATA_PATH is empty.  Using \".\"");
				tsdatadirs = AddToStringList ( tsdatadirs, ".",
				&ntsdatadirs );
			}
			else {	for ( i = 0; i < ntsdatadirs; i++ ) {
					PrintDebug ( 10, routine,
					"tsdatadir[%d] = \"%s\"",
					i, tsdatadirs[i] );
				}
			}
/*
		}
*/
	}

	// Now we have a list of directories to search.  Initialize a TSIdent
	// with the character string identifier so that we can get to the
	// parts...

	TSIdent tsident ( tsid_string );

	strcpy ( scenario, tsident.getScenario() );
	if ( scenario[0] ) {
		// We have scenario information...  Let's see if the
		// file exists...
		PrintDebug ( 10, routine,
		"Trying TS file from scenario:  \"%s\"", scenario );
		if ( FileReadable(scenario) ) {
			// It is, use it...
			PrintDebug ( 10, routine,
			"Found TS file from scenario:  \"%s\"", scenario );
//cfan			strcpy ( tsfile, scenario );
			strcpy ( TSUtilTsfile, scenario );  //cfan
//cfan			return tsfile;
			return TSUtilTsfile;  //cfan
		}
		// If we have gotten to here, then we could not get the file
		// directly and we need to check the path...
		char	**fulltsdatadirs = GetFilesFromPathList (
			tsdatadirs, scenario, &nfulltsdatadirs );
		for ( i = 0; i < nfulltsdatadirs; i++ ) {
			PrintDebug ( 10, routine,
			"Trying TS file from path:  \"%s\"", fulltsdatadirs[i]);
			if ( FileReadable(fulltsdatadirs[i]) ) {
				// Found a match, use it...
//cfan				strcpy ( tsfile, fulltsdatadirs[i] );
				strcpy ( TSUtilTsfile, fulltsdatadirs[i] );  //cfan
				fulltsdatadirs =
				FreeStringList ( fulltsdatadirs );
//cfan				return tsfile;
				return TSUtilTsfile;  //cfan
			}
		}
		fulltsdatadirs = FreeStringList ( fulltsdatadirs );
	}

	// If we have gotten to here, then we do not have scenario information
	// and we need to guess at a file name from the other ID parts...

	// First try the full time series identifier...

	char	**fulltsdatadirs = GetFilesFromPathList (
		tsdatadirs, tsid_string, &nfulltsdatadirs );

	for ( i = 0; i < nfulltsdatadirs; i++ ) {
		PrintDebug ( 10, routine,
		"Trying TS file from path:  \"%s\"", fulltsdatadirs[i]);
		if ( FileReadable(fulltsdatadirs[i]) ) {
			// Found a match, use it...
//cfan			strcpy ( tsfile, fulltsdatadirs[i] );
			strcpy ( TSUtilTsfile, fulltsdatadirs[i] );  //cfan
			fulltsdatadirs = FreeStringList ( fulltsdatadirs );
//cfan			return tsfile;
			return TSUtilTsfile;  //cfan
		}
	}
	fulltsdatadirs = FreeStringList ( fulltsdatadirs );

	// If there is no scenario, try composing the file name without the
	// scenario part...

	if ( !scenario[0] ) {
		sprintf ( tsid_string, "%s.%s.%s.%s",
		tsident.getLocation(), tsident.getSource(), tsident.getType(),
		tsident.getInterval() );
		fulltsdatadirs = GetFilesFromPathList (
		tsdatadirs, tsid_string, &nfulltsdatadirs );

		for ( i = 0; i < nfulltsdatadirs; i++ ) {
			PrintDebug ( 10, routine,
			"Trying TS file from path:  \"%s\"", fulltsdatadirs[i]);
			if ( FileReadable(fulltsdatadirs[i]) ) {
				// Found a match, use it...
//cfan				strcpy ( tsfile, fulltsdatadirs[i] );
				strcpy ( TSUtilTsfile, fulltsdatadirs[i] );  //cfan
				fulltsdatadirs =
				FreeStringList ( fulltsdatadirs );
//cfan				return tsfile;
				return TSUtilTsfile;  //cfan
			}
		}
		fulltsdatadirs = FreeStringList ( fulltsdatadirs );

		strcpy ( string, tsident.getSource() );
		ToUpper ( string );
		// Try using all uppercase data source...
		sprintf ( tsid_string, "%s.%s.%s.%s",
		tsident.getLocation(), string, tsident.getType(),
		tsident.getInterval() );
		fulltsdatadirs = GetFilesFromPathList (
		tsdatadirs, tsid_string, &nfulltsdatadirs );

		for ( i = 0; i < nfulltsdatadirs; i++ ) {
			PrintDebug ( 10, routine,
			"Trying TS file from path:  \"%s\"", fulltsdatadirs[i]);
			if ( FileReadable(fulltsdatadirs[i]) ) {
				// Found a match, use it...
//cfan				strcpy ( tsfile, fulltsdatadirs[i] );
				strcpy ( TSUtilTsfile, fulltsdatadirs[i] );  //cfan
				fulltsdatadirs =
				FreeStringList ( fulltsdatadirs );
//cfan				return tsfile;
				return TSUtilTsfile;  //cfan
			}
		}
		fulltsdatadirs = FreeStringList ( fulltsdatadirs );
	}

	// Need to maybe try different upper/lowercase combinations for the
	// data type and interval??	

	// If we have gotten to here we do not know where the time series is...

	PrintWarning ( 1, routine,
	"Unable to find TS file for \"%s\"", tsid_string );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSUtil_findTSFile.cxx,v $";
 static char rcs_id2[] = "$Id: TSUtil_findTSFile.cxx,v 1.3 2006/04/10 16:11:00 xfan Exp $";}
/*  ===================================================  */

	return (char *)NULL;

}
