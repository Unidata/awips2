/* File: exit_tulplot_cb.c
 *
 * Exit the tulplot application by pressing an upper or
 * lower case q in the hydrograph drawing area.
 * Written by George Smith, 6/21/91
 */

#include "plot.h"
#include "ifp_struct.h"

void exit_tulplot_cb(w, data, call_data)

  Widget                        w;          /* widget data structure */
  caddr_t                       data;       /* generic data pointer  */
  XmDrawingAreaCallbackStruct   *call_data; /* structure pointer     */
{
  KeySym                key;                /* key symbol structure  */
  char                  string[5];          /* string buffer */
  XComposeStatus        cs;
  int                   count;              /* number of characters entered */

  if(call_data->reason == XmCR_INPUT)       /* in drawing area input cb */
    {
     if(call_data->event->xkey.type == KeyPress) /* have a keypress */
       {
	count = (int)XLookupString((XKeyEvent *)call_data->event, string, 5, &key, &cs);
	  {
	   if(count == 1)                   /* only one character was typed */
	     {
	      string[count] = 0;            /* insert null character        */
	      if(strcmp(string, "q") == 0 || strcmp(string, "Q") == 0)
		{
		   printf("exiting tulplot application\n");
		   exit(0);
		}
	     }
	  }
       }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/exit_tulplot_cb.c,v $";
 static char rcs_id2[] = "$Id: exit_tulplot_cb.c,v 1.2 2006/03/28 20:43:47 aivo Exp $";}
/*  ===================================================  */

}
