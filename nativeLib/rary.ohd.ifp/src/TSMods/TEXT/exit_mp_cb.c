/* File: exit_mp_cb.c
 *
 * Exit the tulplot application by pressing an upper or
 * lower case q in the hydrograph drawing area.
 * Written by George Smith, 6/21/91
 */

#include "mods_plot.h"

void exit_mp_cb(w, data, call_data)

  Widget                        w;          /* widget data structure */
  caddr_t                       data;       /* generic data pointer  */
  XmDrawingAreaCallbackStruct   *call_data; /* structure pointer     */
{
  char                  string[5];          /* string buffer */
  KeySym                key;                /* key symbol structure  */
  XComposeStatus        cs;
  int                   count;              /* number of characters entered */

  if(call_data->reason == XmCR_INPUT)       /* in drawing area input cb */
    {
     if(call_data->event->xkey.type == KeyPress) /* have a keypress          */
       {
	count = XLookupString((XKeyEvent *)call_data->event, string, 5, &key, &cs);
	  {
	   if(count == 1)                   /* only one character was typed */
	     {
	      string[count] = 0;            /* insert null character        */
	      if(strcmp(string, "q") == 0 || strcmp(string, "Q") == 0)
		{
		   printf("exiting mods_plot application\n");
		   exit(0);
		}
	     }
	  }
       }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/exit_mp_cb.c,v $";
 static char rcs_id2[] = "$Id: exit_mp_cb.c,v 1.2 2006/04/07 14:34:36 aivo Exp $";}
/*  ===================================================  */

}
