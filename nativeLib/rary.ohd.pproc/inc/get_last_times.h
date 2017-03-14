/*******************************************************************************
* FILENAME:            get_last_times.h
* GENERAL INFORMATION:
* DESCRIPTION:         Contains the prototype for the get_last_times 
*                      routine in the get_last_times.ec file.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       February 11, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE              PROGRAMMER        DESCRIPTION/REASON
* February 11, 2002  Bryon Lawrence    Original Coding
* November 4, 2004   Bryon Lawrence    Modified the rfc and datetime calling
*                                      arguments with the const qualifier.
********************************************************************************
*/
#ifndef GET_LAST_TIMES_H
#define GET_LAST_TIMES_H

void get_last_times ( const char * rfc , const char * datetime , 
		      char str1 [ ] , char str2 [ ] ) ;

#endif /* #ifndef GET_LAST_TIMES_H */
