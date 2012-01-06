/*
** GetSubStringList.c - Daniel Weiler, Riverside Technology, inc.
** 
** Function will return a sublist of a stringlist given the 
** strings that mark the beginning and the end of the substring 
**
** February 17, 1998

** July 10, 2001	James R. VanShaar, RTi	Revised to handle case when 
**						beginning and ending keys exist
**						with nothing in between.
*/

#include "ResJ.h"

char** GetSubStringList( char** super_list, int super_size, 
	char* key, int* sub_size, int flag ) 
{ 
	char 	**list=NULL, **sub_list = NULL, temp[256]; 
	int i, start = -1, end = -1, nlist=0, count = 0, start_found = 0,
		end_found = 0;

	if( !strcasecmp( key, "" ) || super_size == 0 || 
		super_list == NULL ) {
		return( list );
	}

	/* Find the beginning and the end of the substring list */	
	sprintf( temp, "END%s", key );
	for( i = 0; i < super_size; i++ ) {
		list = BreakStringList( super_list[i], " \n\t", 0, &nlist ); 

		if( !list || !nlist ){
			continue;
		}

		if( !strcasecmp( list[0], key ) ) {
			start = i;
		}
		else if( !strcasecmp( list[0], temp ) ) {
			end = i;
			list = FreeStringList( list );
			break;
		}
		list = FreeStringList( list );
	}

	/* 
	** If the start and the end are the same, or the start and end
	** keywords are not found, then problems.
	*/
	if( start >= end || start == -1 || end == -1 ) {
		return( (char**)NULL );
	}	

	/* 
	** If the flag is 0, then we want to include the beginning and
	** the end keyword lines. If it is 1, then we just wnat what is
	** between the beginning and ending keywords
	*/
	if( flag == 1 ) {
		start = start + 1;	
		end = end - 1;
	}

	/* The keywords were found, but nothing is between them */
	if( start > end ) {
		sub_list = AddToStringList( sub_list, "****NADA****", &count );
		count = 0;
	}	

	/* Make the sub list */
	for( i = start; i <= end; i++ ) {
		sub_list = AddToStringList( sub_list, super_list[i], &count );	
	}

	/* Set the size of the substring */
	*sub_size = count;

	/* Return the created sublist */
	return( sub_list );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetSubStringList.c,v $";
 static char rcs_id2[] = "$Id: GetSubStringList.c,v 1.2 2002/02/12 22:15:47 dws Exp $";}
/*  ===================================================  */

}
