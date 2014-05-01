/* ----------------------------------------------------------------------------
** InitFILEs - initialize the file pointers
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine should be called by any routine that uses
**			HMData file pointers (including HMSet*FILE, HMGet*FILE,
**			HMPrint*, HMSet*Level, HMIs*MessageOn).  This routine
**			initializes all global pointers if they have not been
**			initialized.
**		(2)	This routine assigns message levels and files for the
**			0 index.  The files correspond to stderr, which is 
**			often the screen.
** ----------------------------------------------------------------------------
** History:
**
** 06-06-95	Steven A. Malers, RTi	Initial version of routine.
** 03 Sep 96	SAM, RTi		Split code out of HMMsg.c and make more
**					stand-alone.
** 18 Sep 96	CEN, RTi		Copied from HMData
** 08 May 2001	James R. VanShaar, RTi	Minor adjustments to appearance and
**					documentation
** 10 May 2001	JRV, RTi	Caused default status level to be 0 as 
**				consistent with intention that status now be
**				part of debug output only.  Also warning level
**				to be assigned at 0--no warnings to stderr.
** ----------------------------------------------------------------------------
** Variable		I/O	Description
**
** HMfiles_initialized	G	Flag indicating whether FILE pointers have been
**				initialized.
** i			L	Counter for files.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int InitFILEs ( void )
{	int	i;

	if ( !files_initialized ) {
		for ( i = 0; i < MAX_MESSAGE_FILE; i++ ) {
			ESPdebug[i].fp    = (FILE *)NULL;
			ESPdebug[i].func  = NULL;
			ESPdebug[i].level = 0;
		}
		for ( i = 0; i < MAX_MESSAGE_FILE; i++ ) {
			ESPerror[i].fp   = (FILE *)NULL;
			ESPerror[i].func = NULL;
		}
		for ( i = 0; i < MAX_MESSAGE_FILE; i++ ) {
			ESPstatus[i].fp	   = (FILE *)NULL;
			ESPstatus[i].func  = NULL;
			/***ESPstatus[i].level = 1;***/
			ESPstatus[i].level = 0;
		}
		for ( i = 0; i < MAX_MESSAGE_FILE; i++ ) {
			ESPwarning[i].fp    = (FILE *)NULL;
			ESPwarning[i].func  = NULL;
			ESPwarning[i].level = 0;
			ESPwarning[i].level = 1;
			/***ESPwarning[i].level = 1;***/
		}
/*These have been removed so messages no longer go to screen... only
  output file.		
		ESPdebug[0].fp    = stderr;
		ESPerror[0].fp    = stderr;
		ESPstatus[0].fp   = stderr;
		ESPwarning[0].fp  = stderr;
 */
		files_initialized = 1;
		strcpy ( msgprefix, "ResJ" );
		msgsuffix[0]		= '\0';
	}
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/InitFILEs.c,v $";
 static char rcs_id2[] = "$Id: InitFILEs.c,v 1.4 2004/09/09 18:00:06 hank Exp $";}
/*  ===================================================  */

}
