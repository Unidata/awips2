/*******************************************************************************
* FILENAME:              ffgcontrol_show.h
* GENERAL INFORMATION:
* DESCRIPTION:           This file contains the prototypes
*                        of the 
*				display_ffg_show and 
*                        	close_display_ffg 
*			 routines.
*
* ORIGINAL AUTHOR:       Moria Shebsovich
* CREATION DATE:         April 24, 2002
* ORGANIZATION:          OHD / HSEB
* MACHINE:               HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE                PROGRAMMER        DESCRIPTION/REASON
* April 24, 2002   Moria Shebsovich       Original Coding
********************************************************************************
*/

#ifndef FFGCONTROL_SHOW_H
#define FFGCONTROL_SHOW_H

#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>

#include "ArealDisplayControl.h"
#include "List.h"
#include "read_netcdf_ffg.h"

#define LEN_REPLY 1024
#define MAX_RECORD_LENGTH 1024
#define BUF_SIZE 100

#define LENGTH_OF_FFG_DAY 3
#define LENGTH_OF_FFG_YEAR 5
#define LENGTH_OF_FFG_MONTH 3
#define LENGTH_OF_FFG_HOUR 3
#define LENGTH_OF_FFG_MINUTE 3
#define LENGTH_OF_FFG_DURATION_STRING 3
#define FFG_DIRNAME_LENGTH 1024
#define FFG_FILENAME_LENGTH 100

#define WFO_FFG_FILENAME_LEN 23
#define RFC_FFG_FILENAME_LEN 13

typedef struct _Direct {

	Node		node;
        char            loc_id [ 10 ] ;
	char		dir_name [ FFG_DIRNAME_LENGTH ] ;
        char            file_name [ FFG_FILENAME_LENGTH ] ;
        enum            FfgDurationIds  duration ;
        time_t          ticks ;
	List		list;

} Direct;

void ffg_display_show ( Widget w ) ;
void ffg_clear_display ( Widget w , XtPointer clientdata ,
                                XtPointer calldata );

#endif /* #ifndef  FFGCONTROL_SHOW_H */
