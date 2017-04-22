/* ----------------------------------------------------------------------------
** Extern - external varialbes that need to be included in ESP files
** ============================================================================
** Copyright:	See the ../COPYRIGHT file.
** ============================================================================
** History:
**
** 06-06-95	Steven A. Malers, RTi	Change so that multiple file pointers
**					and message levels are available for
**					each set of message routines.  Add
**					prefix for messages.
** 06-26-95	SAM, RTi		Add command line information arguments.
** 07-19-95	SAM, RTi		Add HMworkingdir and
**					HMprogram_intialized variables.
** 26 Sep 1995	SAM, RTi		Add HMcommandfile variable.
** 09 Nov 1995	SAM, RTi		Redimension HMmessage.
** 10 Nov 1995	SAM, RTi		Change STZ code to TZ because we are
**					really dealing with standard and
**					daylight savings time zones.
** 14 Nov 1995	SAM, RTi		Add HMmsgsuffix.
** 06 Feb 1996	SAM, RTi		Add C++ wrapper.
** 04 Mar 1996	SAM, RTi		Add HMprog_env.
** 11 Sep 1996	SAM, RTi		Add HMshow* for the print routines.
** 03 Dec 1996	SAM, RTi		Start using the command file.
** 03 Jan 1997	CEN, RTi		Add HMmonthabbr.
** 03 Nov 1997	MJR, RTi		Changed ESPUtil.h to Util.h
** ----------------------------------------------------------------------------
** Variable		L/G	Description
**
** HMcommandfile	G	Command file for program.
** HMcurrent_dir	G	Directory from which program was executed.
** HMfiles_initialized	G	Indicates whether FILEs have been initialized.
** HMmsgprefix		G	Prefix for HMData messages.
** HMmsgsuffix		G	Suffix for HMData messages.
** HMprog_argc		G	Program's "argc".
** HMprog_argv		G	Program's "argv" as a string list.
** HMprog_env		G	Program's environment as string list.
** HMprogram_initialized G	Indicates whether HMSetProgramData has been
**				called.
** HMshowdebuglevel	G	Indicates whether debug levels should be shown
**				in messages.
** HMshowstatuslevel	G	Indicates whether status levels should be shown
**				in messages.
** HMshowwarninglevel	G	Indicates whether warning levels should be shown
**				in messages.
** ----------------------------------------------------------------------------
*/

#ifndef ExternH_INCLUDED
#define ExternH_INCLUDED

#include <stdio.h>

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif /* C++ */

#include "ResJ.h"

/*
extern int		HMlocalTZDsflag, HMlocalTZNumber, HMlocalTZOffset, HMlocksused;
*/
extern int		prog_argc, program_initialized;

extern char		ESPcommandfile[MAXC],
			message[BIGSTRING],
			**prog_argv,
			**prog_env,
			ESPprogname[MAXC], ESPprogver[MAXC], ESPuser[MAXC],
			ESPworkingdir[MAXC];

			/*
			HMlocalTZ[HMMAXC],
			*/

/*
** Message routine data...
*/

extern int		files_initialized;
extern int		warning_count;

extern char     *ESPmonthabbr[13];

extern int		showdebuglevel;
extern int		showstatuslevel;
extern int		showwarninglevel;

extern char		msgprefix[MAXC];
extern char		msgsuffix[MAXC];

extern MsgData	ESPdebug[MAX_MESSAGE_FILE];
extern MsgData	ESPerror[MAX_MESSAGE_FILE];
extern MsgData	ESPstatus[MAX_MESSAGE_FILE];
extern MsgData	ESPwarning[MAX_MESSAGE_FILE];

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif /* C++ */

#endif /* ExternH_INCLUDED */
