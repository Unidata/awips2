/*
	File:		DialogRsrc.c
	Author:		Paul Taylor
	Date:		4/18/1997
	
	Purpose:	Provide support for miscellaneous dialog functions.
*/


#include "Xtools.h"


/*
	Remaps a given widget by
	unmapping it and mapping it again.
*/
void 	RemapWidget(Widget w)
{
   if (XtIsRealized(w))
   {
      XtSetMappedWhenManaged(w, False);
      XtSetMappedWhenManaged(w, True);
   }

   return;
}
