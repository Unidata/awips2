/* ----------------------------------------------------------------------------
** SetProgramData - set the program name and program version
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine copies the program name and version into
**			global variables so that they can be used in routines
**			outside of the main program (e.g. debug routines).
**		(2)	The user name is also saved.
**		(3)	The command line arguments are saved for use by the
**			HMPrintCreatorHeader function.
** ----------------------------------------------------------------------------
** History:
**
** 06-26-95	Steven A. Malers, RTi	Updated to include argc, argv and
**					update documentation format.
** 07-20-95	SAM, RTi		Add HMcurrentdir and
**					HMprogram_initialized.
** 04 Mar 96	SAM, RTi		Also pass in the environment pointer.
** 06 Sep 96	SAM, RTi		Split out of HMUtil.c
** 27 Sep 96	CEN, RTi		Copied from HMData
** 03 Dec 96	SAM, RTi		Change commandfile to ESPcommandfile.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** argc		I	Number of command line arguments.
** argv		I	Command line arguments.
** envp		I	Environment variables.
** ESPcommandfile G	Program command file.
** HMprog_argc	G	Program number of command line arguments.
** HMprog_argv	G	Program command line arguments.
** HMprog_env	G	Saved environment variables.
** HMprogname	G	Program name.
** HMuser	G	Program user.
** HMprogver	G	Program version.
** len		L	Length of string.
** message	L	Message string.
** progname	I	Program name.
** progver	I	Program version.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int SetProgramData (	char *progname, char *progver, int argc, char *argv[],
			char **envp )
{	char	message[256], routine[] = "SetProgramData";
	int	len;

	strcpy ( ESPprogname, progname );
	strcpy ( ESPprogver, progver );
	prog_argc = argc;
	GetUser ( ESPuser );
	len = MAXC;
	GetWorkingDir ( ESPworkingdir, &len );

	/*
	** Now store the command line arguments in a string list.  Do this
	** because the user may accidentally modify the command line argument
	** pointers (though increments, etc.)...
	*/

	prog_argv = ArrayToStringList ( argc, argv );
	if ( !prog_argv ) {
		sprintf ( message,
		"Cannot create memory for %d command line arguments" );
		PrintWarning ( 2, routine, message );
		prog_argc = 0;
		return STATUS_FAILURE;
	}

	/*
	** Save a pointer to the environment.  Don't create a duplicate string
	** list because it is unlikely the environment will change (but may
	** have to do this later - does putenv cause problems?)...
	*/

	prog_env = envp;

	ESPcommandfile[0]	= '\0';
	program_initialized	= 1;

	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetProgramData.c,v $";
 static char rcs_id2[] = "$Id: SetProgramData.c,v 1.2 2000/05/18 13:08:34 dws Exp $";}
/*  ===================================================  */

}
