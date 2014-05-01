/*------------------------------------------------------------------------------
** StringMatchesRegExp - does a string match a regular expression?
**------------------------------------------------------------------------------
** Notes:	(1)	This routine compares a candidate string with a string
**			that contains regular expression wildcard characters and
**			returns 1 if the candidate string matches.  The
**			following wild cards are currently recognized:
**
**				.	Match one character.
**				*	Match zero or more characters.
**				[...]	Match any one the characters enclosed
**					in the brackets.  This can be a list
**					of characters or a character range
**					separated by a -.
**		(2)	This routine is designed to be called by a higher level
**			routine to check actual filenames versus wildcard
**			patterns.
**		(3)	The following combination is known not to work:
**
**				xxx*.[abc]
**
**			and will be fixed as time allows.
**------------------------------------------------------------------------------
** History:
**
** 28 Sep 1995	Steven A. Malers		Original routine - based on
**		Riverside Technology, inc.	CUJ April 1995 article by Mike
**						Cornelison, but use regular
**						expressions from the "sed and
**						awk" O'Reilly book.
** 04 Oct 1995	SAM, RTi			Add [] capability.
**------------------------------------------------------------------------------
** Variable		I/O	Description
**
** asterisk		L	Indicates whether we are in a section of the
**				regular expression that starts with an asterisk.
** candidate_string	I	Candidate string to check.
** dl			L	Debug level for this routine.
** i			L	Loop counter on characters.
** nokchars		L	Number of "okchars".
** okchars		L	Characters that are for the [] operator.
** pt_candidate		L	Pointer to candidate string.
** pt_wild		L	Pointer to regular expression string.
** regexp_string	I	String containing wildcards to check against.
** routine		L	Name of this routine.
**------------------------------------------------------------------------------
*/

#include "ResJ.h"

int StringMatchesRegExp ( char *candidate_string, char *regexp_string )
{	char	message[256], okchars[256], *pt_candidate = candidate_string,
		*pt_regexp = regexp_string, routine[] = "StringMatchesRegExp";
	int	asterisk, dl = 50, i, j, jumptotest = 0, nokchars = 0;

	sprintf ( message, "Comparing \"%s\" to \"%s\"",
	pt_candidate, pt_regexp );
	PrintDebug ( dl, routine, message );

	while ( 1 ) {
		/*
		** Start new segment in the regular expression...
		*/
		PrintDebug ( dl, routine, "Start new segment section" );
		if ( !jumptotest ) {
			asterisk = 0;
			while ( *pt_regexp == '*' ) {
				PrintDebug ( dl, routine, "Skipping *" );
				++pt_regexp;
				asterisk = 1;
			}
		}

		/*
		** Now test for a match...
		*/

		PrintDebug ( dl, routine, "Start test section" );

		jumptotest = 0;
		while ( 1 ) {
			for (	i = 0, j = 0;
				pt_regexp[i] && (pt_regexp[i] != '*');
				i++, j++ ) {
				sprintf ( message,
				"pt_regexp=[%d]=%c pt_candidate[%d]=%c",
				i, pt_regexp[i], j, pt_candidate[j] );
				PrintDebug ( dl, routine, message );
				if ( pt_regexp[i] != pt_candidate[j] ) {
					if ( !pt_candidate[j] ) {
						/*
						** No match...
						*/
						return 0;
						PrintDebug ( dl, routine,
						"Chars do not match - exit 0" );
					}
					else if ( pt_regexp[i] == '.' ) {
						/*
						** Single character match...
						*/
						PrintDebug ( dl, routine,
						"Character . - go to next character" );
						continue;
					}
					else if ( pt_regexp[i] == '[' ) {
						/*
						** Start of character range.
						** First need to get OK characters...
						*/
						PrintDebug ( dl, routine,
						"[ - check range character" );
						++i;
						while ( pt_regexp[i] != ']' ) {
							if ( !pt_regexp[i] ) {
								return 0;
							}
							else if(pt_regexp[i]
								== '-' ) {
								/*
								** Need to find
								** the next
								** character and
								** then go
								** until that
								** matches...
								*/
								++i;
								if ( !pt_regexp[i] ) {
									return 0;
								}
								else if ( (nokchars > 0) &&
									(pt_regexp[i] <
									okchars[nokchars - 1]) ) {
									return 0;
								}
								sprintf ( message,
								"Using range %c to %c",
								okchars[nokchars - 1],
								pt_regexp[i] );
								PrintDebug ( dl,
								routine, message );
								while ( 1 ) {
									okchars[nokchars] =
									okchars[nokchars - 1] + 1;
									++nokchars;
									sprintf ( message,
									"Added %c from [-] list",
									okchars[nokchars - 1] );
									PrintDebug ( dl,
									routine,
									message );
									if (	okchars[nokchars - 1] ==
										pt_regexp[i] ) {
										/*
										** Last character in range...
										*/
										break;

									}
								}
							}
							else {	/*
								** Just add the
								** character...
								*/
								okchars[nokchars] =
								pt_regexp[i];
								++nokchars;
								sprintf ( message,
								"Added %c from [abc] list",
								okchars[nokchars - 1] );
								PrintDebug ( dl,
								routine,
								message );
								++i;
							}
						}
						/*
						** Now check the character...
						*/
						okchars[nokchars] = '\0';
						if (	strchr(okchars,
							pt_candidate[j]) ) {
							/*
							** Matches OK...
							*/
							continue;
						}
						else {	/*
							** No match...
							*/
							return 0;
						}
					}
					else if ( !asterisk ) {
						/*
						** ?
						*/
						PrintDebug ( dl, routine,
						"Not asterisk - exit 0" );
						return 0;
					}
					++pt_candidate;
					/*
					** Reevaluate the loop again...
					*/
					PrintDebug ( dl, routine,
					"Jumping to test" );
					jumptotest = 1;
					break;
				}
				else {	PrintDebug ( dl, routine,
					"Chars are equal.  Increment..." );
				}
			}
			if (	jumptotest ||
				!pt_regexp[i] || (pt_regexp[i] == '*') ) {
				break;
			}
		}

		PrintDebug ( dl, routine, "Outside for loop" );

		if ( !jumptotest ) {
			if ( pt_regexp[i] == '*' ) {
				sprintf ( message,
				"Have an * - increment by %d and restart segment",
				i );
				PrintDebug ( dl, routine, message );
				pt_candidate	+= j;
				pt_regexp	+= i;
				continue;
			}

			if ( !pt_candidate[j] ) {
				/*
				** End of string...
				*/
				PrintDebug ( dl, routine,
				"End of string - return 1" );
				return 1;
			}
			else if ( i && pt_regexp[i - 1] == '*' ) {
				/*
				** Rest of string is wildcard...
				*/
				PrintDebug ( dl, routine,
				"Rest of string * - return 1" );
				return 1;
			}
			else if ( !asterisk ) {
				PrintDebug ( dl, routine,
				"Not asterisk - return 0" );
				return 0;
			}
			++pt_candidate;
			PrintDebug ( dl, routine,
			"Jumping to test" );
			jumptotest = 1;
		}
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/StringMatchesRegExp.c,v $";
 static char rcs_id2[] = "$Id: StringMatchesRegExp.c,v 1.1 1999/02/18 15:17:22 dws Exp $";}
/*  ===================================================  */

}
