/*
	File:		gpp_input.h
	Date:		June 2005
	Author:	Russell Erb
	
	Purpose:
	
*/


#ifndef gpp_input_h
#define gpp_input_h


/*
	Function prototypes.
*/
void  gpp_hourly_precip ( Observation *obsRow );
void  rename_gpp_workfile ( char *product_id, time_t current_timet );


#endif
