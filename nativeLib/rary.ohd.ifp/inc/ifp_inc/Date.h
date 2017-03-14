/****************************************************************/
/*								*/
/*	FILE:		Date.h					*/
/*								*/
/*								*/
/*								*/
/*	Coded by:	Tom Adams (TEA)				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/01/94				*/
/*	Modified:	11/08/94				*/
/*								*/
/****************************************************************/

#ifndef Date_h
#define Date_h


/*--------------------------------------------------------------*/
/* Structure to hold date & time values				*/
/*--------------------------------------------------------------*/
/* gfs 950219 */
typedef struct
	{
	int     month;
	int     day;
	int     year;
	int     hour;
	char    time_zone[5];
	}       date;
/* gfs 950219 */	

#endif 
