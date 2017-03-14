/*------------------------------------------------------------------------------
** BreakStringList - get a list of strings from a string
**------------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
**------------------------------------------------------------------------------
** Notes:	(1)	The list is assumed to be of the form "val,val,val",
**			where the commas indicate the delimiter character.
**		(2)	Call "FreeStringList" when done with the list.
**		(3)	The list always has one NULL element at the end so that
**			we know how to free the memory.  However, "nlist" does
**			not include this element.
**		(4)	If the DELIM_ALLOW_STRINGS flag is set, then we
**			strings to be treated as one token, even if they contain
**			blanks.  The first quote character, either " or ' is
**			used to contain the string.  The quote characters
**			cannot be in the list of delimiting characters.
**		(5)	It would be nice to allow return of all the tokens.
**			Add the "flag" variable to allow for this enhancement
**			in the future.
**------------------------------------------------------------------------------
** History:
**
** ?		Steven A. Malers, RTi	Created routine.
** 06-08-95	SAM, RTi		Document all variables.
** 08-21-95	SAM, RTi		Change so that delimiter list is a
**					string so that more than one
**					"whitespace" character can be used
**					(e.g., spaces and tabs).  Also allow
**					more than one whitespace character in
**					sequence (skip them all).  Also add
**					check to make sure that substring is not
**					overrun.
** 04 Oct 1995	SAM, RTi		Use AddToStringList to do bulk of
**					work.
** 02 Sep 1996	SAM, RTi		Break this routine out of Util.c.  Do
**					minor cleanup to make more stand-alone.
** 07 Oct 1996	SAM, RTi		Add <string.h> to prototype functions.
** 21 Jan 1997	SAM, RTi		Add flag to allow quoted strings to be
**					separated out.
** 17 Jun 1997	Matthew J. Rutherford, RTi
**					Adjust string stuff so that a quote
**					in the middle of a string is found.
**------------------------------------------------------------------------------
** Variable	I/O	Description
**
** delim	I	Character delimiter list.
** flag		I	Flag to modify parsing.
** i		L	Counter for characters in substring.
** instring	L	Indicates if we are inside a quoted string.
** list		L	List of broken out strings.
** nlist	O	Number of strings in the final list.
** nlist2	L	Used when adding strings to list.
** pt		L	Pointer to original string.
** pt2		L	Pointer to split out string.
** quote	L	Character used for a quoted string.
** routine	L	Name of this routine.
** string	I	String of delimiter-separated items.
** tempstr	L	String used when splitting out sub-strings.
**------------------------------------------------------------------------------
*/

#include <string.h>

#include "ResJ.h"

#ifndef BIGSTRING
#define BIGSTRING 2000
#endif

char **BreakStringList (	char *string, char *delim, unsigned int flag,
				int *nlist )
{	char	**list = (char **)NULL, *pt, *pt2, quote,
		routine[] = "BreakStringList", tempstr[BIGSTRING]="";
	int	i, instring, nlist2;
	
	*nlist = 0;
	if ( !string ) {
	 	return (char **)NULL;
	}
	if ( !string[0] ) {
	 	return (char **)NULL;
	}
	PrintDebug ( 50, routine,
	"Breaking \"%s\" using \"%s\"", string, delim );
	pt = string;
	while ( *pt ) {
		/*
		** Start the next string in the list.  Move characters to the
		** temp string until a delimiter is found.  If inside a string
		** then go until a closing delimiter is found.
		*/
		instring	= 0;
		pt2		= tempstr;
		i	= 1;
		while ( *pt ) {
			/*
			** Process this string...
			*/
			if (	(flag & DELIM_ALLOW_STRINGS) &&
				!instring &&
				((*pt == '"') || (*pt == '\'')) ) {
				/*
				** We have found the start of a quote...
				*/
				instring	= 1;
				quote		= *pt;
				/*
				** Skip over the quote since we don't want to 
				** store or process again...
				*/
				++pt;
			}
			if ( instring && (*pt == quote) ) {
				/*
				** We are in a string and have found the closing
				** quote.  Need to skip over it.
				*/
				instring = 0;
				++pt;
				/*
				** break;
				*/
			}
			if ( strchr(delim, *pt) ) {
				/*
				** We have a delimiter character...  We could
				** be in a string or not...
				*/
				if ( !instring ) {
					/*
					** Not in a string so OK to break...
					*/
					break;
				}
				/*
				** Else, treat as a character that needs to be
				** part of the token...
				*/
			}
			if ( i < BIGSTRING ) {
				*pt2++ = *pt++;
			}
			else {	/*
				** Don't want to run out of space in the
				** temporary string...
				*/
				pt++;
			}
			++i;
		}
		*pt2 = '\0';
		/*
		** Now skip any additional delimiters that may be present in a
		** sequence...
		*/
		if ( flag & DELIM_SKIP_BLANKS ) {
			while ( *pt && strchr(delim, *pt) ) {
				++pt;
			}
			if ( tempstr[0] == '\0' ) {
				/*
				** The string is empty (e.g., at start of
				** original string.  Skip it...
				*/
				continue;
			}
		}
		if ( (flag & DELIM_ALLOW_STRINGS) && instring ) {
			PrintWarning ( 10, routine,
			"Quoted string \"%s\" is not closed", tempstr );
		}
		list = AddToStringList ( list, tempstr, &nlist2 );
		if ( list == (char **)NULL ) {
			PrintWarning ( 5, routine,
			"Unable to break string - can't add to new list" );
			return (char **)NULL;
		}
		PrintDebug ( 50, routine, "Broke out list[%d]=\"%s\"",
		*nlist, list[*nlist] );
		++(*nlist);
		if ( *pt && strchr(delim, *pt) ) {
			++pt;
		}
	}
	return list;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/BreakStringList.c,v $";
 static char rcs_id2[] = "$Id: BreakStringList.c,v 1.1 1999/02/18 15:16:37 dws Exp $";}
/*  ===================================================  */

}
