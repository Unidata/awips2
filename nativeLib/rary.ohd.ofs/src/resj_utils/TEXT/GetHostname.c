/* ----------------------------------------------------------------------------
** GetHostname - get the hostname
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine gets the hostname on which the program is
**			running.
** ----------------------------------------------------------------------------
** History:
**
** 29 Nov 1995	Steven A. Malers, RTi	Update to work with SunOS.
** 16 Feb 1996	Steven A. Malers, RTi	Update to work with Visual C++.
** 06 Sep 1996	SAM, RTi		Split code out of HMUtil.c file.
** 27 Sep 1996	Catherine E. Nutting, RTi	Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** hostname	O	Hostname.
** dirpt	L	Pointer to directory string.
** flags	L	Modifier for how host name is to be returned.
**
**				HMHOST_SHORT	- only the first part
**				HMHOST_FULL	- the entire host name
**				HMHOST_IP	- the IP address
**
**			Only the first option works at this time.
** list		L	String list of hostname broken into parts.
** message	L	String for messages.
** nchar	I	Dimensioned size of the string on entry, length without
**			trailing null on return.
** nlist	L	Number of components in "list".
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

int GetHostname ( char *hostname, int *nchar, unsigned int flags )
{	char	dirtmp[256], **list, message[256], routine[] = "GetHostname";
	int	len, nlist;

	*hostname = '\0';

#ifdef UNIX
#ifdef SunOS
	if ( (sysinfo(SI_HOSTNAME, hostname, *nchar)) == -1 ) {
#else
	if ( gethostname(hostname, *nchar) ) {
#endif
		/*
		** Error since non-zero was returned...
		*/
		PrintWarning ( 2, routine, "Error getting hostname" );
		*hostname	= '\0';
		*nchar		= 0;
		return STATUS_FAILURE;
	}

	if ( flags & HOST_SHORT ) {
		/*
		** We want only the first part of the hostname...
		*/
		list = BreakStringList ( hostname, ".", 0, &nlist );
		if ( list == (char **)NULL ) {
			/*
			** Trouble breaking the list.  Leave the hostname as
			** is...
			*/
			sprintf ( message,
			"Trouble breaking hostname \"%s\".  Leaving as is",
			hostname );
			PrintWarning ( 2, routine, message );
		}
		else if ( nlist > 0 ) {
			/*
			** Use the first item in the list...
			*/
			strcpy ( hostname, list[0] );
			FreeStringList ( list );
		}
		/*
		** else, hostname is already set...
		*/
	}
	else {	/*
		** Don't know how to do any of the other flags yet...
		*/
		hostname[*nchar - 1] = '\0';
	}
#else
	strcpy ( hostname, "PChost" );
#endif /* UNIX */
	*nchar = strlen ( hostname );
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetHostname.c,v $";
 static char rcs_id2[] = "$Id: GetHostname.c,v 1.1 1999/03/10 15:14:53 dws Exp $";}
/*  ===================================================  */

}
