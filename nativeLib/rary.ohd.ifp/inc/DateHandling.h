/****************************************************************/
/*								*/
/*	FILE:		DateHandling.h				*/
/*								*/
/*								*/
/*								*/
/*	Coded by:	Tom Adams (TEA)				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/01/94				*/
/*	Modified:	11/02/94				*/
/*			11/03/94				*/
/*								*/
/****************************************************************/

#ifndef DateHandling_h
#define DateHandling_h

#include "ifp_inc/Date.h"

	
/*--------------------------------------------------------------*/
/* Structure to hold date & time values	for the Mods Start,	*/
/* End, and Valid dates						*/
/*--------------------------------------------------------------*/
typedef struct  /* gfs 950219 _date gfs 950219 */
	{
	date	*StartDate;
	date	*EndDate;
	date	*ValidDate;
	}	datesStruct_t, *datesStruct_p;

/*--------------------------------------------------------------*/
/* Structure to hold pointers to the Increment & Decrement	*/
/* Callback functions for changing the Time & Date fields	*/
/*--------------------------------------------------------------*/
typedef struct
	{
	void	(*increment)();
	void	(*decrement)();
	}       dateCBStruct_t, *dateCBStruct_p;
 

/*--------------------------------------------------------------*/
/* We have different Callback functions depending on which	*/
/* field was selected by the user...				*/
/*--------------------------------------------------------------*/
 typedef struct
	{
	dateCBStruct_p	month;
	dateCBStruct_p	day;
	dateCBStruct_p	year;
	dateCBStruct_p	time;
	}       dateFieldCBStruct_t, *dateFieldCBStruct_p;
 


/*--------------------------------------------------------------*/
/*--------------------------------------------------------------*/
typedef struct
	{
	Widget		month;
	Widget		day;
	Widget		year;
	Widget		time;
	}    dateWidgetStruct_t, *dateWidgetStruct_p;
	
	
/*--------------------------------------------------------------*/
/*--------------------------------------------------------------*/
typedef struct
	{
	date			*dateStruct;
	Widget			selected;
	dateCBStruct_p		CBFuncs;
	dateWidgetStruct_p	dateWidgets;
	}      dateFieldStruct_t, *dateFieldStruct_p;
	
#endif	

 
 
 
 
 
 
 
 
 
 
 
 
