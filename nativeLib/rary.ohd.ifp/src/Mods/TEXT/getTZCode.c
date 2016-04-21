/* File: /get_TZ_code.c
 *
 * Used to obtain the time zone values.
 */

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "ifp_atoms.h"







/************************************************************************/
/*									                                    */
/*	get_timeZone_code()						                            */
/*									                                    */
/************************************************************************/

char *get_timeZone_code(Display *display)
{

	Window          root;

	int             type;			/* type of data stored in the window property	*/
	int             format;			/* format of the stored data			*/
	int             nitems;			/* number of bytes retrieved			*/
	int             left;			/* remaining bytes stored in the window		*/
	long            offset = 0;		/* property starting point data			*/
	char            *time_zone_code;	/* time zone code pointer			*/



 root = DefaultRootWindow(display);

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_time_zone_code,
	offset,
	(long) (sizeof(char) * 5),      /* Time Zone code is 4 characters + 1 for '\0'...       */
	FALSE,
	IFPA_time_zone_code_type,
	(Atom *)&type,
	&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&time_zone_code
	) == Success && type == IFPA_time_zone_code_type) return(time_zone_code);
 else   return(NULL);



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/getTZCode.c,v $";
 static char rcs_id2[] = "$Id: getTZCode.c,v 1.2 2006/04/18 15:29:11 aivo Exp $";}
/*  ===================================================  */

}
