/*------------------------------------------------------------------------------
** SortStringList - sort a string list and return the sorted list
**------------------------------------------------------------------------------
** Notes:	(1)	The list is assumed to be terminated by a NULL string.
**		(2)	Right now the strings are sorted in ascending order.
**		(3)	The sort is accomplished by adding to a new string
**			list in the proper order.  A temporary array of integers
**			is used to indicate which of the old strings have been
**			processed.  The memory for the original list is freed.
**------------------------------------------------------------------------------
** Variable	I/O	Description
**
** flag		I	Flag indicating sort order, etc. (currently unused).
** message	G	Global message string.
** i		L	Counter for strings.
** ismallest	L	Position of "smallest" in list.
** itmp		L	Temporary array used in sort.
** list		I	List of broken out strings.
** maxlen	L	Maximum length of strings in list.
** newlist	L	New (sorted) string list.
** nlist	L	Number of strings in the new list.
** routine	L	Name of this routine.
** smallest	L	Smallest string.
**------------------------------------------------------------------------------
*/

#include "ResJ.h"

char **SortStringList ( char **list, unsigned long int flag )
{	int	i, ismallest, *itmp = (int *)NULL, listlen, maxlen = 0,
		nlist = 0;
	char	message[256], **newlist = (char **)NULL,
		routine[] = "SortStringList", *smallest = (char *)NULL;

	if ( list == (char **)NULL ) {
		PrintWarning ( 50, routine, "NULL string list" );
		return (char **)NULL;
	}
	/*
	** First determine how big to make our working string...
	*/
	for ( i = 0; list[i] != (char *)NULL; i++ ) {
		maxlen = MAX ( maxlen, strlen(list[i]) );
	}
	/*
	** Now allocate memory for the string...
	*/
	smallest = (char *)(malloc((maxlen + 1)*sizeof(char)));
	if ( smallest == (char *)NULL ) {
		/*
		** Unable to get memory for the string.  Just return
		** the old list...
		*/
		sprintf ( message,
		"Unable to allocate memory for temp string (%d chars)",
		maxlen );
		PrintWarning ( 2, routine, message );
		return list;
	}
	/*
	** Now allocate memory for the temporary int array used to keep
	** track of the sort order...
	*/
	StringListLength ( list, &listlen );
	itmp = (int *)(malloc((listlen)*sizeof(int)));
	if ( itmp == (int *)NULL ) {
		/*
		** Unable to get memory for the temporary array.  Just
		** return the old list...
		*/
		sprintf ( message,
		"Unable to allocate memory for temp sort array (%d ints)",
		listlen );
		PrintWarning ( 2, routine, message );
		return list;
	}
	for ( i = 0; i < listlen; i++ ) {
		itmp[i] = 0;	/* indicates not in new list yet */
	}
	/*
	** OK, now do the sort.  Just do a buble sort and go through the entire
	** list twice.
	*/
	while ( 1 ) {
		ismallest = -1;
		for ( i = 0; list[i] != (char *)NULL; i++ ) {
			if ( itmp[i] ) {
				/*
				** Already in the new list...
				*/
				continue;
			}
			/*
			** Save the "smallest" string.  If this is the first
			** string encountered this iteration, initialize with
			** the first string...
			*/
			if (	(ismallest == -1) ||
				(strcmp(list[i],smallest) < 0) ) {
				strcpy ( smallest, list[i] );
				ismallest = i;
			}
		}
		if ( ismallest == -1 ) {
			/*
			** We have exhausted the search so break out...
			*/
			break;
		}
		newlist = AddToStringList ( newlist, list[ismallest],
		&nlist );
		if ( newlist == (char **)NULL ) {
			/*
			** Trouble adding to the string list...
			*/
			PrintWarning ( 2, routine,
			"Trouble adding string to sorted string list.  Returning original" );
			if ( smallest ) {
				free ( smallest );
			}
			if ( itmp ) {
				free ( itmp );
			}
			return list;
		}
		itmp[ismallest] = 1;
	}
	/*
	** We are done with the sort.  Free the old list and return the new...
	*/
	if ( smallest ) {
		free ( smallest );
	}
	if ( itmp ) {
		free ( itmp );
	}
	FreeStringList ( list );
	return newlist;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SortStringList.c,v $";
 static char rcs_id2[] = "$Id: SortStringList.c,v 1.1 1999/02/18 15:17:19 dws Exp $";}
/*  ===================================================  */

}
