#include "ResJ.h"

/* ----------------------------------------------------------------------------
** SpawnProcess - spawn a background process
** ----------------------------------------------------------------------------
** Notes:	(1)	In UNIX terms, this routine does a fork() and exec()
**			for a command.
** ----------------------------------------------------------------------------
** History:
**
** 15 May 1996	Steven A. Malers, RTi	Add the UNIX wrapper so we can compile
**					on the PC.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** command	I	The command to run, as a string.
** exec_command	L	Command used to do "exec".
** flags	I	Indicate how process is to be spawned (unused at the
**			moment).
** message	G	Global message string.
** i		L	Loop counter for command arguments.
** nstrings	L	Number of "strings".
** pid		L	Process ID.
** status	L	Return status from functions.
** STDERR	L	Integer file unit corresponding to standard error.
** stderrfp	I	Pointer to an open file that is to receive standard
**			error messages from the process.
** STDOUT	L	Integer file unit corresponding to standard output.
** stdoutfp	I	Pointer to an open file that is to receive standard
**			output messages from the process.
** strings	L	"command" broken into string list.
** ----------------------------------------------------------------------------
*/
int SpawnProcess (	char *command, FILE *stdoutfp, FILE *stderrfp,
			unsigned int flags )
{	int	i, pid, nstrings, status, STDOUT = 1, STDERR = 2;
	char	exec_command[ESPMAXC], message[ESPMAXC], routine[] = "SpawnProcess",
		**strings = (char **)NULL;

	sprintf ( message, "Trying to spawn process \"%s\"", command );
	PrintDebug ( 5, routine, message );

	/*
	** Make sure that the pointers are not NULL...
	*/

	if ( command == (char *)NULL ) {
		PrintWarning ( 2, routine, "Command string is NULL" );
		return STATUS_FAILURE;
	}
	if ( stdoutfp == (FILE *)NULL ) {
		PrintWarning ( 2, routine,
		"Standard output file pointer is NULL" );
		return STATUS_FAILURE;
	}
	if ( stderrfp == (FILE *)NULL ) {
		PrintWarning ( 2, routine,
		"Standard error file pointer is NULL" );
		return STATUS_FAILURE;
	}

#ifdef UNIX
	/*
	** Create forked process...
	*/

	if ( (pid = fork()) < 0 ) {
		sprintf ( message, "Unable to fork process for \"%s\"",
		command );
		PrintWarning ( 2, routine, message );
		return STATUS_FAILURE;
	}

	/*
	** Execute the child process...
	*/

	if ( pid == 0 ) {
		/*
		** Reset the standard output for the child to the desired file
		** pointer...
		*/
		if ( stdoutfp == stdout ) {
			PrintWarning ( 2, routine,
			"Not resetting stdout - also used for file" );
		}
		else {	close ( STDOUT );
			status = dup(fileno(stdoutfp));
			if ( status != STDOUT ) {
				sprintf ( message,
				"Unable to reset stdout for \"%s\" (dup %d)",
				command, status );
				PrintWarning ( 2, routine, message );
				return STATUS_FAILURE;
			}
		}
		/*
		** Reset the standard error for the child to the desired file
		** pointer...
		*/
		if ( stderrfp == stderr ) {
			PrintWarning ( 2, routine,
			"Not resetting stderr - also used for file" );
		}
		else {	close ( STDERR );
			status = dup(fileno(stderrfp));
			if ( status != STDERR ) {
				sprintf ( message,
				"Unable to reset stderr for \"%s\" (dup %d)",
				command, status );
				PrintWarning ( 2, routine, message );
				return STATUS_FAILURE;
			}
		}
		/*
		** Break the command into a string list...
		*/
		strings =	BreakStringList ( command, " \t\n",
				DELIM_SKIP_BLANKS, &nstrings );
		if ( strings == (char **)NULL ) {
			sprintf ( message, "Unable to split \"%s\"",
			command );
			PrintWarning ( 2, routine, message );
			return STATUS_FAILURE;
		}
		/*
		** Execute the process (let the string list pass all the
		** arguments)...
		*/
		sprintf ( message, "Full path to command = \"%s\"",
		strings[0] );
		PrintDebug ( 5, routine, message );
		for ( i = 0; i < nstrings; i++ ) {
			sprintf ( message, "Command argv[%d] = \"%s\"",
			i, strings[i] );
			PrintDebug ( 5, routine, message );
		}
		/*
		** Call the routine that takes the environment information.  It
		** appears that only execlp and execvp correctly search the
		** PATH such that a full path is not required.  The environment
		** does NOT need to be passed in (rather ODD).
		*/
		strcpy ( exec_command, "execvp" );
		status = execvp ( strings[0], strings );
		if ( status ) {
			sprintf ( message,
			"Unable to exec process for \"%s\" (%s %d)",
			command, exec_command, status );
			/*
			** Need to process the errno here!
			*/
			PrintWarning ( 2, routine, message );
			FreeStringList ( strings );
			return STATUS_FAILURE;
		}
	}

	/*
	** Assume that all is OK...
	*/

	FreeStringList ( strings );
#else
	sprintf ( message, "%s not enabled outside of UNIX", routine );
	PrintWarning ( 1, routine, message );
#endif
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SpawnProcess.c,v $";
 static char rcs_id2[] = "$Id: SpawnProcess.c,v 1.1 1999/02/18 15:17:20 dws Exp $";}
/*  ===================================================  */

}
