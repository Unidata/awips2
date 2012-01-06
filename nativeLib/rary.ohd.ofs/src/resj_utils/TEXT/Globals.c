/*-----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ============================================================================
** History:
**
** 1.0 (early 93)	Steven A. Malers, RTi	Start library to help in
**						processing HMData data.
** 1.1 (3-30-93)	SAM, RTi		Modify code so both basins and
**						stations have TS data type field
**						and general data field.
** 1.2 (5-19-93)	SAM, RTi		Added the "HMprogname",
**						"HMprogver", and "HMuser" global
**						variables.
** 1.3 (6-10-93)	SAM, RTi		Shorten file names to compile on
**						PC platform.
** 1.4 (8-27-93)	SAM, RTi		Added HMwarninglevel and
**						HMStatuslevel.
** 1.5 (12-7-93)	SAM, RTi		Added file pointers for output
**						messages so that they can be
**						reassigned.
** 1.6 (11-25-94)	SAM, RTi		Added the time variables to
**						deal with NWSRFS variables.
** 1.7 (12-09-94)	SAM, RTi		Added the HMlocksused variable.
** 06-06-95		SAM, RTi		Abandon use of version in this
**						file header - date is enough.
**						Change so that multiple FILE
**						pointers and output levels
**						are available for each type of
**						message to allow output to the
**						screen and a file.  Put the
**						message data in a data
**						structure.  Add prefix to
**						messages.
** 07-19-95		SAM, RTi		Add global variable to hold
**						directory from which program
**						was executed.  Also add
**						HMprogram_initialized variable.
** 07-31-95		SAM, RTi		Add a global HMmessage array
**						for messages so that each
**						routine does not need to
**						allocate space for it.
** 26 Sep 1995		SAM, RTi		Add HMcommandfile variable.
** 09 Nov 1995		SAM, RTi		Increase size of HMmessage to
**						HMBIGSTRING to allow for very
**						long messages.
** 10 Nov 1995		SAM, RTi		Change all STZ code to TZ
**						because we are really dealing
**						with standard and daylight
**						savings time zones.
** 16 Nov 1995		SAM, RTi		Add suffix to messages (mainly
**						so things like <br> can be
**						added for CGI programs).
** 04 Mar 1996		SAM, RTi		Add HMenv_vars to store pointer
**						to environment variables.
** 03 Sep 1996		SAM, RTi		Move the messaging variables
**						into the HMMsg.c code.
** 11 Sep 1996		SAM, RTi		Add HMshow* variables to turn
**						on/off display of message levels
**						in messages.
** 17 Sep 1996		CEN, RTi		Copied from HMData to ESPUtil	
** 03 Dec 1996		SAM, RTi		Change commandfile to
**						ESPcommandfile.
**-----------------------------------------------------------------------------
** Notes:	(1)	The routines in this library perform operations related
**			to hydrometeorology.
**		(2)	The variables in this file are the only global variables
**			in the HMData library.  They should be declared as
**			"extern" in other library files.
**		(3)	Make sure to initialize the file pointers to NULL.
**-----------------------------------------------------------------------------
** Variables		L/G	Description
**
** HMcommandfile	G	Command file used for a program (basically
**				command line arguments, but in a file).
** HMenv_vars		G	List of environment variables.
** HMerror		G	HMData error data.
** HMfiles_initialized	G	Indicates whether the FILE pointers have been
**				initialized.
** HMlocksused		G	Indicates whether locks are used for data files.
** HMmessage		G	Global array for messages.
** HMmsgprefix		G	Prefix for all messages.
** HMmsgsuffix		G	Suffix for all messages.
** HMprogram		G	Name of program that is running.
** HMprogram_initialized G	Indicates whether HMSetProgramData has been
**				called.
** HMprogver		G	Program version.
** HMshowdebuglevel	G	Indicates whether debug levels should be shown
**				in messages.
** HMshowstatuslevel	G	Indicates whether status levels should be shown
**				in messages.
** HMshowwarninglevel	G	Indicates whether warning levels should be shown
**				in messages.
** HMstatus		G	HMData status data.
** HMuser		G	Login name of current user.
** HMwarning		G	HMData warning data.
** HMworkingdir		G	Directory from which program was executed
**				(used with HMSetProgramData).
**-----------------------------------------------------------------------------
*/

#include <stdio.h>
#include "ResJ.h"

int		locksused		= 0,
		prog_argc		= 0,
		program_initialized	= 0;

char		ESPcommandfile[MAXC]	= "",
		**prog_argv = (char **)NULL,
		**prog_env = (char **)NULL,
		ESPprogname[MAXC]	= "",
		ESPprogver[MAXC]	= "",
		ESPuser[MAXC]		= "",
		ESPworkingdir[MAXC]	= "";

/*
** Global data for the time/date routines...
*/

char    *ESPmonthabbr[13] = {    "Jan", "Feb", "Mar", "Apr",
	"May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "" };


/*
** Global time variables that may be used for modeling (especially the NWSRFS).
**
** notes:	(1)	These variables are defined here and are initialized
**			by utility routines.  These variables should never
**			be touched "by human hands".
**
**		(2)	HMLocalTZOffset is the difference between the local
**			standard time and the internal clock:
**
**				InternalHour + HMLocalTZOffset = LocalSTD
**				HMLocalTZOffset = LocalSTD - InternalHour
*/
/*
int	HMlocalTZDsflag = 0, HMlocalTZOffset = 0, HMlocalTZNumber = 0;
char	HMlocalTZ[HMMAXC] = "";
*/

/*
** Global variables for message routines...
*/

int		files_initialized	= 0;
int		warning_count		= 0;

int		showdebuglevel	= 0;
int		showstatuslevel	= 0;
int		showwarninglevel	= 0;

char		msgprefix[ESPMAXC];
char		msgsuffix[ESPMAXC];

MsgData	ESPdebug[MAX_MESSAGE_FILE];
MsgData	ESPerror[MAX_MESSAGE_FILE];
MsgData	ESPstatus[MAX_MESSAGE_FILE];
MsgData	ESPwarning[MAX_MESSAGE_FILE];

/*  ==============  Statements containing RCS keywords:  */
static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/Globals.c,v $";
static char rcs_id2[] = "$Id: Globals.c,v 1.2 1999/04/27 16:09:47 dws Exp $";
/*  ===================================================  */
