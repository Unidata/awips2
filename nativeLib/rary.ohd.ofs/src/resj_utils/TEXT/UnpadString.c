#include "ResJ.h"

/*-----------------------------------------------------------------------------
** UnpadString - Unpad a string
**-----------------------------------------------------------------------------
** History:
**
** 01/08/96	Peter T. Abplanalp, RTi	Created routine.
** 17 Jan 96	SAM, RTi		Allow user specified list of delimiter
**					characters.
** 03 Feb 96	SAM, RTi		Previous version did not seem to work
**					correctly.
** 14 Oct 98	Daniel Weiler	 	Commented out PrintDebug b/c the 
**					message char* limits the size of 
**					the incoming string we can deal with.
** ** *** 2003	James R. VanShaar, RTi	Revised to eliminate strcpy with 
**					overlapping strings.
**-----------------------------------------------------------------------------
** Notes:	(1)	Input string is modified by this routine.
**-----------------------------------------------------------------------------
** Variable	I/O	Description
**
** default_white L	Default white space characters.
** flag		I	Flag to specify routine behavior.
** message	G	Global message string.
** i		L	Size of character array.
** pt		L	Generic pointer.
** routine	L	Routine name.
** string	I/O	String to unpad.
** white0	I	List of user-supplied characters to use for whitespace.
** white	L	List of characters to use for whitespace within this
**			routine.
**-----------------------------------------------------------------------------
*/
int UnpadString ( char *string, char *white0, unsigned long int flag )
{	int	i, len;
	char	default_white[] = " \t\n", *pt,
		routine[] = "UnpadString", white[ESPMAXC];
		/*
		*temp;
		*temp[ESPMAXC];
		*/

	/*
	** Check for NULL prointers...
	*/

	if ( string == (char *)NULL ) {
		return STATUS_FAILURE;
	}

	/*
	** Set default whitespace characters if not specified...
	*/

	if ( white0 == (char *)NULL ) {
		strcpy ( white, default_white );
	}
	else if ( white0[0] == '\0' ) {
		strcpy ( white, default_white );
	}
	else {	strcpy ( white, white0 );
	}

	if ( string[0] == '\0' ) {
		return STATUS_SUCCESS;
	}

	if ( flag & PAD_BACK ) {
		/*
		** Remove whitespace from back...
		*/
		i	= strlen(string) - 1;
		pt	= &string[i];
		while ( (i >= 0) && (strchr(white,*pt) != (char *)NULL) ) {
			/*
			** Set whitespace to NULL character as we backtrack...
			*/
			*pt = '\0';
			--pt;
			--i;
		}
		/*
		sprintf ( message,
		"Result after \"%s\" off back: \"%s\".", white, string );
		PrintDebug ( 45, routine, message );
		*/
	}

	if ( flag & PAD_FRONT ) {
		/*
		** Remove whitespace from front...
		*/
		pt = &string[0];
		while ( *pt && (strchr(white,*pt) != (char *)NULL) ) {
			/*
			** Skipping leading whitespace...
			*/
			pt++;
		}
		/*
		strcpy(string, pt);
		*/
		len = strlen(pt);
		if( len != strlen(string) ) {
			for( i=0; i<len; i++ ) {
				string[i] = pt[i];
			}
			string[i] = '\0';
		}
		/*
		strcpy (temp, pt);
		strcpy ( string, temp );
		sprintf ( message,
		"Result after \"%s\" off front: \"%s\".", white, string );
		PrintDebug ( 45, routine, message );
		*/
	}

	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/UnpadString.c,v $";
 static char rcs_id2[] = "$Id: UnpadString.c,v 1.2 2004/09/08 17:14:31 hank Exp $";}
/*  ===================================================  */

}
