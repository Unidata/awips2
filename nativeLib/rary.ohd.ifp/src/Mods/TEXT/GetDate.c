/****************************************************************/
/*                                                              */
/*	FILE:		GetDate.c										*/
/*                                                              */
/*                                                              */
/*	Coded by:	Tom Adams                               		*/
/*			NWS * Office of Hydrology * HRL         			*/
/*	Date:		11/01/94                                		*/
/*                                                              */
/*      NOTE:													*/
/*																*/
/*                                                              */
/****************************************************************/


#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <time.h>

#include "DateHandling.h"
#include "ifp_atoms.h"






date *GetDate(Display *display, Atom dateAtom, Atom dateAtomType)
{

	char            string[5];
	char            the_zone[5];
	char            *time_zone_code;

	int             type;    /* type of data stored in the window property */
	int             format;  /* format of the stored data */
	int             nitems;  /* number of bytes retrieved */
	int             left;    /* remaining bytes stored in the window */
	int             the_month, the_day, the_year, the_hour;
	int             timeZoneCodePropIsPresent = FALSE;
	long            offset = 0;
	long            tp;

	date            *date_ptr;
	Window          root;

	struct  tm      *time_pointer;



	root = DefaultRootWindow(display);


	/* Get the Time Zone Code	*/
	if(XGetWindowProperty
		(
		display,
		root,
		IFPA_time_zone_code,
		offset,
		(long) 5,
		FALSE,
		IFPA_time_zone_code_type,
		(Atom *)&type,
		&format,
		(unsigned long *)&nitems,
		(unsigned long *)&left,
		(unsigned char **)&time_zone_code
		) == Success && type == IFPA_time_zone_code_type){
		strcpy(the_zone, time_zone_code);
		timeZoneCodePropIsPresent = TRUE;
		}
/*	else    get_ofs_default_tzc(the_zone);				*/


	/*----------------------------------------------*/
	/* Get the date from the Root Window Property,	*/
	/*  if it's there; otherwise, get it from the	*/
	/* system clock...				*/
	/*----------------------------------------------*/
	if(XGetWindowProperty(
		display,
		root,
		dateAtom,
		offset,
		(long) sizeof(date),
		FALSE,
		dateAtomType,
		(Atom *)&type,
		&format,
		(unsigned long *)&nitems,
		(unsigned long *)&left,
		(unsigned char **)&date_ptr) == Success && type == dateAtomType){

			if(timeZoneCodePropIsPresent) strcpy(date_ptr->time_zone, the_zone);
			else strcpy(the_zone, date_ptr->time_zone);
			}
		else	{
			time(&tp);
			time_pointer = localtime(&tp);

			date_ptr = (date *) XtMalloc(sizeof(date));
			date_ptr->month = time_pointer->tm_mon + 1;
			date_ptr->day = time_pointer->tm_mday;
			date_ptr->year = 1900 + time_pointer->tm_year;
			date_ptr->hour = time_pointer->tm_hour;
			strcpy(date_ptr->time_zone, the_zone);
			}

/*			
	XChangeProperty(
		display,
		root,
		dateAtom,
		dateAtomType,
		8,
		PropModeReplace,
		date_ptr,
		sizeof(date)
		);
*/

	return(date_ptr);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/GetDate.c,v $";
 static char rcs_id2[] = "$Id: GetDate.c,v 1.2 2006/04/18 13:33:54 aivo Exp $";}
/*  ===================================================  */

}
