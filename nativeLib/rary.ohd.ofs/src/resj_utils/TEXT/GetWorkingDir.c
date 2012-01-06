/* ----------------------------------------------------------------------------
** GetWorkingDir - get the working directory for the program
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine gets the program working directory (the
**			directory from which the program was executed).
** ----------------------------------------------------------------------------
** History:
**
** 06 Sep 1996	Steven A. Malers, RTi	Split code out of the HMUtil.c file.
** 27 Sep 1996	Catherine E. Nutting, RTi	Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** dir		O	Program working directory.
** dirpt	L	Pointer to directory string.
** message	G	String for messages.
** nchar	I	Dimensioned size of the string on entry, length without
**			trailing null on return.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

int GetWorkingDir ( char *dir, int *nchar )
{	char	dirtmp[256], message[256], routine[] = "GetWorkingDir";
	int	len;

	/*
	** Get the current working directory...
	*/
	*dir = '\0';
#ifdef UNIX
#if defined (HPUX) || (SunOS)
	if ( !getcwd(dirtmp,256) ) {
		sprintf ( message, "Unable to get working directory" );
		PrintWarning ( 2, routine, message );
		return STATUS_FAILURE;
	}
#else
	if ( !getwd(dirtmp) ) {
		sprintf ( message, "Unable to get working directory" );
		PrintWarning ( 2, routine, message );
		return STATUS_FAILURE;
	}
#endif
	/*
	** Get the length of the string...
	*/
	len = strlen ( dirtmp );
	if ( len < *nchar ) {
		strcpy ( dir, dirtmp );
		*nchar = len;
	}
	else {	*nchar = 0;
		sprintf ( message,
		"Working dir path too long (%d) to fit in allocated array (%d)",
		len, *nchar );
	}
#else
	PrintWarning ( 2, routine, "Unable to return for PC" );
#endif /* UNIX */
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetWorkingDir.c,v $";
 static char rcs_id2[] = "$Id: GetWorkingDir.c,v 1.1 1999/02/18 15:16:54 dws Exp $";}
/*  ===================================================  */

}
