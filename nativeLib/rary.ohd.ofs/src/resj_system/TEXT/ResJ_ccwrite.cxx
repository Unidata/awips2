// 	12 Apr 2001	James R. VanShaar, RTi	
//					Modified code to split long strings 
//					here at appropriate white space prior
//					to calling ResJ_fwrite
//	08 May 2001	JRV	Allowed proper handling of '\n' embedded 
//				within the string.
//	** *** 2003	JRV	Fixed reference to uninitialized character in
//				temp.

#include "ResJSys.h" 

int ResJ_ccwrite( int *strngLen, char* resj_string, int *unitNum ){

	char temp[120];
	char *begNext = resj_string;
	char *lastSpace;
	int newLen = *strngLen;

	temp[119] = '\0';	// Remember indexing goes from 0 to 119.
	while ( newLen > 119 ) {
		// Test for '\n' within then first 120 places.  If found, we
		// will break at the '\n', print out the string, and increment
		// to just after the '\n' for the next round.
		newLen = strcspn ( begNext, "\n" );
		if ( newLen < 120 ) {
			// copy the non-'\n' string to temp; indexed from 0 to 
			// newLen-1
			strncpy ( temp, begNext, newLen );

			// After the non-'\n' string, put a null character
			temp[newLen] = '\0';

			// Send temp to be written
			ResJ_fwrite ( &newLen, temp, unitNum );

			// Reset begNext to point to the remainder of the
			// original string (as pointed to by begNext) just 
			// after the '\n' character, at index newLen + 1.
			begNext = &begNext[newLen+1];

			// Calculate newLen of remaining string
			newLen = strlen( begNext );

			continue;
		}

		// copy up to 118 characters into temp so as to
		// allow (in the largest condition--with a ' ' in the 118th slot)
		// a '\\' character in the 119th and a null character in the 120th
		strncpy ( temp, begNext, 118 );
		temp[118] = '\0';
		// Transform any tabs to spaces
		ReplaceChar ( temp, '\t', ' ' );
		// locate the last occurrance of ' '
		lastSpace = strrchr ( temp, ' ' );
		if ( lastSpace == NULL ) {
			// We have a 118 character long word with no
			// spaces!! Unless the 119th (accessed with index 118) is
			// a space we cannot properly break this so the read in
			// will work.
			if ( (begNext[118] == ' ') ) {
				// Assign the ' ' to temp
				temp[119] = ' ';
				newLen = 119;
				// write the line (ResJ_fwrite will supply the
				// '\\' character
				ResJ_fwrite ( &newLen, temp, unitNum );
				// prepare by incrementing to the pointer to 
				// the remainder of the string, determine its
				// length and reinitializing temp.
				begNext = &begNext[newLen];
				newLen = strlen( begNext );
				temp[119] = '\0';
				continue;
			}
			// We have trouble and cannot parse it
			// A warning is included as a comment line and an error
			// is returned following writing using the ResJ_fwrite
			// line breaking method.
			// NOTE the next 3 lines form all part of the same
			// 	command:
			sprintf (temp, "%s%c",
                "#WARNING: The following multiple line split may be incorrect.",
			'\0');
			newLen = strlen( temp ); 
			ResJ_fwrite ( &newLen, temp, unitNum );
			newLen = strlen (begNext);
			ResJ_fwrite ( &newLen, begNext, unitNum );
			return 1;
		}
		// replace the character just after the last ' ' with
		// a continuation character '\\' and the character
		// after that with the null character '\0'
		lastSpace[1] = '\\';
		lastSpace[2] = '\0';
		
		// Identify size of temp and send it to be written
		newLen = strlen( temp );
		ResJ_fwrite ( &newLen, temp, unitNum );

		// Reset begNext to point to the remainder of the
		// original string (as pointed to by begNext) at index
		// newLen - 1 to account for addition of '\\'
		begNext = &begNext[newLen-1];
		// Calculate newLen of remaining string
		newLen = strlen( begNext );
		
	}
	// write last bit of the string (or first if < 119 chars long)
	ResJ_fwrite( &newLen, begNext, unitNum );

	return 0;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJ_ccwrite.cxx,v $";
 static char rcs_id2[] = "$Id: ResJ_ccwrite.cxx,v 1.7 2006/10/26 15:31:48 hsu Exp $";}
/*  ===================================================  */

}
