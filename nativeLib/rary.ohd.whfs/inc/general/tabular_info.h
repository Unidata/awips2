
/*
	Name:           tabular_info.h

	Description:    Header file for tabular_info.h

	Author:         Sung Vo

	Created:        9/28/1999

*/


#ifndef _tabular_info_h
#define _tabular_info_h

#include "DbmsDefs.h"
#include "Xtools.h"

typedef struct _TAB_INFO
{
	int	nitems,
		selected_pos;

	time_t	Begin_time,
		End_time;

	RussText	*buf;

}TAB_INFO;

#endif
