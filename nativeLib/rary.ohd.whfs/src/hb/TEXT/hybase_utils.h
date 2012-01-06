/*
	File:		hybase_utils.h
	Date:		April 4, 1997
	Author:		Paul Taylor
	
	Purpose:	Utility functions for HydroBase.
*/


#ifndef hybase_utils_h
#define hybase_utils_h


/*
	Utility functions.
*/
int	hb_check_lat_bounds(char *lat);
int	hb_check_lon_bounds(char *lon);
char*	hb_cvt_latlon_from_string(char *string);




#endif



