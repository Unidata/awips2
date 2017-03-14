#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "ifp_atoms.h"







/****************************************************************/
/*								*/
/*	getUnits()						*/
/*								*/
/****************************************************************/

int getUnits(Display *display, Atom atomName, Atom atomType)
{

	Window          root;
	int		*unitsFlag;
	int             type;			/* type of data stored in the window property	*/
	int             format;			/* format of the stored data			*/
	int             nitems;			/* number of bytes retrieved			*/
	int             left;			/* remaining bytes stored in the window		*/
	long            offset = 0;		/* property starting point data			*/
	char		*metric = "METRIC";
	char		*english = "ENGLISH";
	char		*unknown = "UNKNOWN";
	char		string[20];


 root = DefaultRootWindow(display);

 if(XGetWindowProperty
	(
	display,
	root,
	atomName,
	offset,
	(long) (sizeof(int)),
	FALSE,
	atomType,
	(Atom *)&type,
	&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&unitsFlag
	) == Success && type == atomType) {
		if(*unitsFlag) strcpy(string, metric);
		else strcpy(string, english);
		return(*unitsFlag);
		}
 else   {
	strcpy(string, unknown);
 	return(-99999);
 	}



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_getUnits.c,v $";
 static char rcs_id2[] = "$Id: Mods_getUnits.c,v 1.3 2006/04/18 15:27:22 aivo Exp $";}
/*  ===================================================  */

}
